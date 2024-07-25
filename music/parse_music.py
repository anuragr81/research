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
    #if len(toks)>1:
    #    raise RuntimeError("cannot have more than one note within the same token")
    
    global notesqueue 
    debug = False

    notesqueue.append(''.join(toks))
    if debug:
        print("add_note - %s" % str(notesqueue))
    return None

def add_single_unit(toks):
    global taalaqueue
    global notesqueue 
    global taalasize
    debug = False
    taalaqueue.append(deepcopy(notesqueue ))
    if debug:
        print("notesqueue =" + repr(notesqueue ))
    notesqueue = []
    
    return None
    

def add_collection_group(toks):
    debug = False
    if toks:
        notesarr = [str(toks[i]) for i,_ in enumerate(toks) if i > 0 and  i < len(toks)-1 ]
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
            else:
                notestoappend.append(notesarr[index])
            index = index +1
            
        notesqueue.append(notestoappend)

def approx_time_signature(c,u,x):
    """
    The function returns approximate time signature for the x count of beats
    that are to be played with c-counts of u-unit. For example, if 7 units (x=7) are played in
    one bar which comprises of 4 counts (c=4) or a quarter-note (u=4), then the time signature 
    approximates 1/7 ( c/(u*x) = 4 / (4*7)) by using x* (2**y) = c/u => y = log(c/(x*u),2) 
    if y=3, then 1/(2**3) i.e. 1/8 approximates 1/7.
    
    Similarly, if c=3, u=4, x=7, then 3/(4*7) is approximated 3/4 ~ 7/8
    
    """
    return int(2**ceil(log(c/(u*x),1/2)))

def grammar():
    gap = Literal("-")
    lpar, rpar , squote= map(Literal, "()'")
    lsqb, rsqb= map(Literal, "[]")
    
    singlenote = gap| (Word(alphanums) + ZeroOrMore(squote))
    
    singleunit = Forward()
    singleunit <<=  (lpar + OneOrMore(singlenote) + rpar).setParseAction(add_collection_group)  \
        | ( lpar + OneOrMore(singleunit).setParseAction(add_single_unit)+ rpar ) \
            | OneOrMore(singlenote).setParseAction(add_note)
    
    staff = (lsqb + delimitedList(Group(singleunit)) + rsqb)
    return staff
    

def get_mapping():
    return {
        '-':'-',
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
def parse_music_sequence(input_string):
    global notesqueue
    global taalaqueue
    pattern = grammar()
   
    notesqueue = []
    taalaqueue =[]
   
    if input_string != "":
        try:
            L = pattern.parseString(input_string, parseAll=True)
            if notesqueue:
                taalaqueue += notesqueue
                notesqueue=[]
        except ParseException as err:
            L = ["Parse Failure", input_string, (str(err), err.line, err.column)]
            print(L)
            return []
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
    m = get_mapping()   
    output = []
    for i,t in enumerate(seq):
        outnotes=[]
        for note in t:
            if isinstance(note, str):
                outnotes.append(m.get(note))
            elif isinstance(note,list):
                ts = approx_time_signature(c=1,u=unit_denominator,x=len(note))
            else:
                raise ValueError("Unsupported note-unit")
            print("note(type=%s)=%s" % (type(note),str(note)))
            
    return("num_units="+str(num_units) + " den_units=" + str(unit_denominator) + " ts=" + str(ts))
    
if __name__ == '__main__':
    input_string = "[(S (R' -) M (P G'') - ) , (G - R - S)]"
    taalaseq = parse_music_sequence(input_string) 
    print(taalaseq )
    print(create_lilypond_sequence(taalaseq ,unit_denominator=4))
    