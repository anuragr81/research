from copy import deepcopy
from math import log
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

    notesqueue.append(toks[0])
    print("add_note - %s" % str(notesqueue))
    return None

def add_single_unit(toks):
    global taalaqueue
    global notesqueue 
    global taalasize
    taalaqueue.append(deepcopy(notesqueue ))
    print("notesqueue =" + repr(notesqueue ))
    notesqueue = []
    
    return None
    
def add_collection(toks):
    if toks:
        notesarr = [str(toks[i]) for i,_ in enumerate(toks) if i > 0 and  i < len(toks)-1 ]
        print("add_collection = ( " + ' '.join(notesarr)  + ")")
        notesqueue.append(notesarr)


def approx_time_signature(c,u,x):
    """
    The function returns approximate time signature for the x count of beats
    that are to be played with c-counts of u-unit. For example, if 7 units (x=7) are played in
    one bar which comprises of 4 counts (c=4) or a quarter-note (u=4), then the time signature 
    approximates 1/7 ( c/(u*x) = 4 / (4*7)) by using x* (2**y) = c/u => y = log(c/(x*u),2) 
    if y=3, then 1/(2**3) i.e. 1/8 approximates 1/7.
    
    Similarly, if c=3, u=4, x=7, then 3/(4*7) is approximated 3/4 ~ 7/8
    
    """
    return 2**round(log(c/(u*x),1/2),0)

def grammar():
    gap = Literal("-")
    lpar, rpar = map(Literal, "()")
    lsqb, rsqb= map(Literal, "[]")
    singlenote = gap| Word(alphanums) 
    
    singleunit = Forward()
    singleunit <<=  (lpar + OneOrMore(singlenote) + rpar).setParseAction(add_collection)  \
        | ( lpar + OneOrMore(singleunit).setParseAction(add_single_unit)+ rpar ) | OneOrMore(singlenote).setParseAction(add_note)
        
    return singleunit
    
    #staff = (lsqb + delimitedList(Group(singleunit)) + rsqb)
    #return staff
    
if __name__ == '__main__':
    #input_string = "((A1,A2,C3),(A2,-,A3))"
    #input_string = "[ (A1,A2),(A2,-),(A3,-)]"
    input_string = "(A0 (A1 A2) A3 (A4 A5) A6) "
    notesqueue = []
    taalaqueue =[]
    taalasize = -1
    pattern = grammar()
    if input_string != "":
            # try parsing the input string
            try:
                L = pattern.parseString(input_string, parseAll=True)
            except ParseException as err:
                L = ["Parse Failure", input_string, (str(err), err.line, err.column)]
                print(L)

    print(taalaqueue)