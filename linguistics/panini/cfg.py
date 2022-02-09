from collections import OrderedDict

from sutras.common_definitions import *
from functools import reduce

from sutras import adhyaaya1 as a1
from sutras import adhyaaya2 as a2
from sutras import adhyaaya3 as a3
from sutras import adhyaaya4 as a4
from sutras import adhyaaya5 as a5
from sutras import adhyaaya6 as a6
from sutras import adhyaaya7 as a7
from sutras import adhyaaya8 as a8

from sutras.adhyaaya1 import *
from sutras.adhyaaya2 import *
from sutras.adhyaaya3 import *
from sutras.adhyaaya4 import *
from sutras.adhyaaya5 import *
from sutras.adhyaaya6 import *
from sutras.adhyaaya7 import *
from sutras.adhyaaya8 import *


"""
Currently the naayaka is broken because aardhadhaatukasyavalaadeH has been enabled due
to default aardhadhaatukaH
"""

def get_sutras_for_module (j):
    return [ (int(re.search('([0-9]+)$',x).group(1)),getattr(j,x)) for x in dir(j) if not x.startswith('_')  and re.search('.*[0-9]+',x)]

def get_sutras_ordered ():    
    all_sutras = reduce(lambda x , y : x + get_sutras_for_module (y) , [a1,a2,a3,a4,a5,a6,a7,a8],[]) 
    return OrderedDict(sorted(all_sutras))

def transformation_sutras():
    return [701001, 701002, 702115, 702116, 703052, 704114, 801015, 802066]

class Application:
    def __init__(self,entry):
        self._entry = entry

class It:
    def __init__(self,data):
        self._data= data

class Dhaatu:
    def __init__(self,data):
        self._data= data

def get_supported_types ():
    return (Suffix,It,Dhaatu)

def is_known_type(nd):
    return any ( isinstance(nd,x) for x in get_supported_types())

def find_known_parent(nd):
    # return parent of Node that is not of a known type
    if nd is None:
        return None
    
    if is_known_type(nd):
        return nd
    else:
        return find_known_parent(nd.parent)

"""
We are going to a model where known parents are always provided
"""

class Node:
    def __init__(self,data,parent):
        if all ( not isinstance(data,x) for x in list(get_supported_types()) + [list] ):
            raise ValueError("Unsupported type %s" % type(data))
        if parent is not None:
            if isinstance(parent,list):
                # temporary type (list etc.) so go to the first typed parent
                self.parent = find_known_parent(parent)
            elif all ( not isinstance(parent,x) for x in get_supported_types() ):
                raise ValueError("Unsupported parent type %s" % type(parent))
            else:
                self.parent = parent
        else:
            self.parent = None
        
        self._data =data
        


def desuffix(x):
    return x.get_suffix() if isinstance(x,Suffix) else x

def reduce_to_it_lopa(expr,index):
    """
    Applied recursively until no it saMNcjyA is implied
    The decompostion of suffix into it-s retains the collective property of being a Suffix
    """
    #TODO: remove recursive node formation
    
    # parts of expr other than suffix can be used if needed
    suffix= expr[index]._data
    # don't make the suffix a parent if it's not proper
    to_be_parent = suffix if is_known_type(suffix) else find_known_parent(expr[index])
    
    it_pos = lashakvataddhite_103008(suffix)
    
    if it_pos is not None:
        # splitting the suffix and asking for next phase with the index + 1
        return reduce_to_it_lopa(expr=expr[0:index] + [Node(It(desuffix(suffix)[0:it_pos+1]),parent=to_be_parent) , Node(desuffix(suffix) [it_pos+1:],parent=to_be_parent)] + expr[index+1:] ,index=index+1)

    it_pos_list = aadirNciXtuXdavaH_103005(suffix.get_suffix() if isinstance(suffix,Suffix) else suffix)
    pos_order = [it_pos_list[i+1]-it_pos_list [i]for i in range(len(it_pos_list)-1)]
    
    if any(x <0 for x in pos_order ):
        raise RuntimeError("Cannot process list of it-s")
        
    if it_pos_list :
        return reduce_to_it_lopa(expr=expr[0:index] + [ Node(It(''.join(desuffix(suffix)[i] for i in it_pos_list ))), 
                                       Node(desuffix(suffix)[it_pos_list[-1]+1:])] + expr[index+1:],index=index+1)
    
    it_pos = halantyam_103003(desuffix(suffix))
    #only ends are removed not the start
    if it_pos is not None:
        return reduce_to_it_lopa(expr=expr[0:index] + [Node(desuffix(suffix)[0:it_pos],parent=to_be_parent) , 
                                                       Node(It(desuffix(suffix) [it_pos:]),parent=to_be_parent)] + expr[index+1:] ,
                                 index=index)
    
    it_pos = chuXtuu_10307(desuffix(suffix))
    if it_pos is not None:
        return reduce_to_it_lopa(expr=expr[0:index] + [Node(It(desuffix(suffix)[0:it_pos+1]),parent=to_be_parent) , Node(desuffix(suffix) [it_pos+1:],parent=to_be_parent)] + expr[index+1:] ,index=index+1)
        
    return expr
    
def pick_last_child(nodes, parent):
    if nodes[0].parent != parent:
        raise ValueError("Invalid Start")
    cur_index = 0
    for i,x in enumerate(nodes):
        if x.parent == parent:
            cur_index = i
        else:
            return cur_index
    return cur_index


def join_non_its(nodes):
    strout=""
    for x in nodes:
        if not isinstance(x._data,It):
            if any (not isinstance(j,str) for j in x._data):               
                raise RuntimeError("Invalid input for Joining")
            else:
                strout=strout+''.join(x._data)
                
    return strout

def combine_dhaatu_suffix(dhaatu,suffix):
    #TODO: instead of string - apply transformations that apply to dhaatu + suffix combination e.g. upadhaayaaH etc.
    
    return ''.join(dhaatu._data._data) + join_non_its(suffix)

def apply_transformation(transformation_rule,new_expr):
    for i in range(0,len(new_expr)):
        if new_expr[i].parent:
            # handle a pratyaya
            # only adjacent 
            if isinstance(new_expr[i].parent,Suffix):
                dhaatu_index = pick_last_dhaatu(new_expr[0:i])
                parent_boundary_end=pick_last_child(new_expr[i:],new_expr[i].parent)
                # reducing expression with combination
                # this involves appending plain-strings (that cannot be reduced further)
                return new_expr[0:dhaatu_index] + transformation_rule (new_expr[dhaatu_index],new_expr[i:i+parent_boundary_end+1]) + new_expr[i+parent_boundary_end+1:]
    return new_expr


def reduce_dhaatu_combination(new_expr):
    for i in range(0,len(new_expr)):
        if new_expr[i].parent:
            # handle a pratyaya
            # only adjacent 
            if isinstance(new_expr[i].parent,Suffix):
                dhaatu_index = pick_last_dhaatu(new_expr[0:i])
                parent_boundary_end=pick_last_child(new_expr[i:],new_expr[i].parent)
                # reducing expression with combination
                # this involves appending plain-strings (that cannot be reduced further)
                return new_expr[0:dhaatu_index] + [combine_dhaatu_suffix(new_expr[dhaatu_index],new_expr[i:i+parent_boundary_end+1])] + new_expr[i+parent_boundary_end+1:]
    return new_expr
            
            
def pick_last_dhaatu(nodes):
    for i,x in enumerate(nodes):        
        if isinstance(x._data,Dhaatu):
            return i
        elif not isinstance(x._data,It):
            raise RuntimeError("Must be either It or Dhaatu")
            
    raise RuntimeError("Dangling Dhaatu")
    
def process_list(expr):
    all_sutras= get_sutras_ordered()
    if any(not isinstance(entry,Node) for entry in expr):
        raise ValueError("Only nodes are to be present")
    new_expr = expr.copy()
    
    # the logic for applying it-lopa is that 
    # all suffixes are searched and then lopa-application is applied
    # this is the application phase - and it ensued only because we invoked it at first
    # there may be other things that are needed before application occurs

    while True:
        suffix_indices= [ index for index,node in enumerate(new_expr) if isinstance(node._data,Suffix)]
        if suffix_indices:
            new_expr = reduce_to_it_lopa(expr=new_expr,index=suffix_indices[0])
        else:
            break
    
    
    # apply transformations until there is no change in the expression
    for transformation_ruleid in transformation_sutras():
        new_expr = apply_transformation(all_sutras[transformation_ruleid],new_expr)
        
    # list could be further reduced before dhaatu-suffix combination is invoked
    # other reduction may apply before or after
    new_expr = reduce_dhaatu_combination(new_expr)                               
            
    # join all immediate neighours - 2-neighborhoods
    
    return new_expr


a = get_sutras_ordered()

expression=[Node(Dhaatu(parse_string("bhaj")),parent=None),Node(Suffix("ghaNc"),parent=None)]


processed_expr=(process_list(expression))

print([e._data for e in processed_expr])