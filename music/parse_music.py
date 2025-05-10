from copy import deepcopy
import numpy as np
from math import log,ceil, floor

from pyparsing import (
    Literal,
    Word,
    Group,
    Forward,
    alphas,
    alphanums,
    Regex,
    ParseException,
    CaselessKeyword,
    Suppress,
    delimitedList,
    OneOrMore,
    ZeroOrMore
    )


def push_first(toks):
    return None

def push_unary_minus(toks):
    return None

def get_calculator_expression():
#   https://github.com/pyparsing/pyparsing/blob/master/examples/SimpleCalc.py
    e = CaselessKeyword("E")
    pi = CaselessKeyword("PI")
    
    fnumber = Regex(r"[+-]?\d+(?:\.\d*)?(?:[eE][+-]?\d+)?")
    ident = Word(alphas, alphanums + "_$")
    
    plus, minus, mult, div = map(Literal, "+-*/")
    lpar, rpar = map(Suppress, "()")
    addop = plus | minus
    multop = mult | div
    expop = Literal("^")
    
    expr = Forward()
    expr_list = delimitedList(Group(expr))
    # add parse action that replaces the function identifier with a (name, number of args) tuple
     
    def insert_fn_argcount_tuple(t):
        fn = t.pop(0)
        num_args = len(t[0])
        t.insert(0, (fn, num_args))
    
    fn_call = (ident + lpar - Group(expr_list) + rpar).setParseAction( insert_fn_argcount_tuple)
    # addop[...] just means ZeroOrMore(addop)
    atom = (
      addop[...]
      + (
      (fn_call | pi | e | fnumber | ident).setParseAction(push_first)
      | Group(lpar + expr + rpar)
      )
      ).setParseAction(push_unary_minus)
    
    factor = Forward()
    factor <<= atom + (expop + factor).setParseAction(push_first)[...]
    term = factor + (multop + factor).setParseAction(push_first)[...]
    expr <<= term + (addop + term).setParseAction(push_first)[...]
    return expr


    
    
def add_note(toks):
    
    global notesqueue 
    debug = True

    notesqueue.append(''.join(toks))
    if debug:
        print("add_note - %s" % str(notesqueue))
    return None

def add_single_unit(toks):
    global taalaqueue
    global notesqueue 
    debug = True
    taalaqueue.append(deepcopy(notesqueue ))
    if debug:
        print("notesqueue =" + repr(notesqueue ))
    notesqueue = []
    
    return None
    

def add_collection_group(s,loc,toks):
    debug = True
    if toks:
        notesarr =toks
        
        if debug:
            print("add_collection_group= ( " + ' '.join(notesarr)  + ")")
        index=0
        #ensure that first letter is not a single-quote
        if notesarr[index]=="'":
            raise ValueError("Cannot have first letter as single-quote")
        #since the first letter is not a single-quote, it can be treated 
        #as the first note
        notestoappend=[]
        notestoappend.append(notesarr[index])
        index = index + 1
        while index < len(notesarr):
            if notesarr[index] == "'":
                # change the string of the last element
                notestoappend[-1]=notestoappend[-1]+"'"
            elif notesarr[index] == "%":
                # change the string of the last element
                notestoappend[-1]=notestoappend[-1]+"%"
            else:
                notestoappend.append(notesarr[index])
            index = index +1
        
        
        # remove last notes added in the queue before adding them as 
        # a group
        global notesqueue 
        for _ in range(0,len(notesarr)):
            notesqueue = notesqueue [0:-1]
        notesqueue.append(notestoappend)

def approx_time_signature(notelen, num):
    """
    The notelen is the length of the note the num units of size (1/2**x) for some integer x are approximately 
    meant to fit into.   (1/2**x)*(num+d) thus approximates notelen where d is an integer. We run the search 
    until d < num/3 
    """
    solutions = []
    d = 0 
    while d <= num/3 :
        for k in range(8):
            div = 2**(k+1)
            solutions.append({'error':notelen - (num+d)/div,'solution':{'div':div,'d':d}})
        d=d+1
    minerr = min(abs(x['error']) for x in solutions)
    sol = [x for x in solutions if x['error']==minerr][0]
    return sol['solution']

def grammar():
    gap = Literal("-")
    lpar, rpar , squote, pct, lcurl, rcurl= map(Literal, "()'%{}")
    lsqb, rsqb= map(Literal, "[]")
    
    singlenote = gap | (Word(alphanums) + ZeroOrMore(squote|pct)) 
    
    onebeat = (lpar + OneOrMore(singlenote).set_parse_action(add_collection_group) + rpar) \
        | singlenote.set_parse_action(add_note)
    
    singleunit =  ( lcurl + OneOrMore(onebeat).set_parse_action(add_single_unit)+ rcurl ) 
    
    staff = (lsqb + delimitedList(Group(singleunit)) + rsqb)
    return staff
    

def parse_note(note,unit_denominator):
    m={
        '-':'r',
        'S':'c',
        'r':'des',
        'R':'d',
        'g':'ees',
        'G':'e',
        'm':'f',
        'M':'fis',
        'P':'g',
        'd':'aes',
        'D':'a',
        'n':'bes',
        'N':'b',
        }
    
    notetotranslate=note
    addendum = ""
    if notetotranslate.endswith("'"):
        
        while notetotranslate.endswith("'"):
            notetotranslate = notetotranslate[0:-1]
            addendum = addendum  + "'"
        
    elif notetotranslate.endswith("%"):
        while notetotranslate.endswith("%"):
            notetotranslate = notetotranslate[0:-1]
            addendum = addendum  + ","
    else:
        addendum = ""
        
    return m[notetotranslate ]  + addendum + str(unit_denominator)
    
def parse_music_sequence(input_string):
    global notesqueue
    global taalaqueue
    pattern = grammar()
   
    notesqueue = []
    taalaqueue =[]
   
    if input_string != "":
        try:
            L = pattern.parse_string(input_string, parseAll=True)
            if notesqueue:
                taalaqueue += notesqueue
                notesqueue=[]
        except ParseException as err:
            L = ["Parse Failure", input_string, (str(err), err.line, err.column)]
            print(L)
            raise err
        return taalaqueue
    else:
        return []


def create_lilypond_sequence(seq,unit_denominator):
    if not seq or not isinstance(seq,list):
        raise ValueError("seq must be a list")
    num_units = len(seq[0])
    if not isinstance(unit_denominator ,int) or unit_denominator not in (2,4,8,16,32,64):
        raise ValueError("Unsupported unit_denominator : %s" % str(unit_denominator))
    
    if any(x !=0 for x in np.diff( [len(x) for x in seq])):
        raise ValueError("Cannot have different lengths")
    outnotes=[]
    for i,t in enumerate(seq):
        for note in t:
            if isinstance(note, str):
                outnotes.append(parse_note(note,unit_denominator))
            elif isinstance(note,list):
                sol = approx_time_signature(1/unit_denominator ,len(note))
                newnote =  note + ['-']*sol['d'] if sol['d'] else note
                outnotes = outnotes + [parse_note(x,sol['div']) for x in newnote]
            else:
                raise ValueError("Unsupported note-unit")

    return {'numerator':num_units, 'denominator': unit_denominator, 'notes':' '.join(outnotes)}
    
def get_input_string():
    #return "[(S (r' -) m (P r G -) - ) , (G - r - S)]"
    #return "[{ - - - - G m } , { d - D (P d) P -}]"
    
    #return "[{ - - - - - - - - G m } , { d - d P ( P d ) P m ( m P ) m (G m) }]"
    #return "[{r - G m P m (G m) (G r) S -},{ r (S N) S G m P d N S r}]"
    #return "[{P - P (N d) - N S S S -}, { ( N d) - ( N d) S N r ( S N ) S ( N d ) P } , { P m ( m P) G m P d N S r }, { S N d P N d ( P m ) P - -  }]"
    #return "[{m P - N d N S' - S' -},{ S' N d P m G r - - S }]"
    return "[{ S R }, { g - } ]"


if __name__ == '__main__':
    input_string = get_input_string()
    taalaseq = parse_music_sequence(input_string) 
    print(taalaseq )
    print(create_lilypond_sequence(taalaseq ,unit_denominator=2))
    
