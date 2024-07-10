from copy import deepcopy

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



def add_unit(toks):
    global taalaqueue
    global notesqueue 
    global taalasize
    taalaqueue.append(deepcopy(notesqueue ))
    notesqueue = []
    
    
def add_note(toks):
    if len(toks)>1:
        raise RuntimeError("cannot have more than one note within the same token")
    
    global notesqueue 

    notesqueue.append(toks[0])
    
def add_collection(toks):
    print("add_collection = " + repr(toks))
    
def grammar():
    gap = Literal("-")
    comma = Literal(",")
    lpar, rpar = map(Literal, "()")
    lsqb, rsqb= map(Literal, "[]")
    singlenote = ( gap| Word(alphanums) ) .setParseAction(add_note)
    notecollection = (lpar + delimitedList(Group(singlenote)) [...] + rpar ).setParseAction(add_unit)
    singleunit = notecollection.setParseAction(add_collection) + (comma [...] + notecollection)[...]
    
    staff = (lsqb + delimitedList(Group(singleunit)) + rsqb)
    return staff
    
if __name__ == '__main__':
    #input_string = "((A1,A2,C3),(A2,-,A3))"
    input_string = "[ (A1,A2),(A2,-),(A3,-)]"
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