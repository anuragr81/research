from collections import OrderedDict

from sutras.common_definitions import Dhaatu,Node,Suffix, parse_string
import re, sys
from copy import deepcopy
from functools import reduce
from pprint import pprint
import inspect

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
    res=[]
    for x in dir(j) :        
         if not x.startswith('_')  and re.search('[0-9]+$',x):
             res.append((float(re.search('([0-9]+_*[0-9]*)$',x).group(1)),getattr(j,x)))
    return res

def get_sutras_ordered ():    
    all_sutras = reduce(lambda x , y : x + get_sutras_for_module (y) , [a1,a2,a3,a4,a5,a6,a7,a8],[]) 
    return OrderedDict(sorted(all_sutras))

def transformation_sutras():
    ll = [2040850,3010331,3040820,6010080,6010630, 6010750, 6040880,6010980, 
          6010990,6011030, 6041200, 6041480, 7010030,7010010, 7010020, 7010120,
          7020021, 7021150, 7021160, 7030520, 7030840,7031010,
          8010150, 8020660, 8030059]
    return sorted(float(x) for x in ll)



def prepend_sutras():
    return [6040710]

def insertion_sutras():
#   to be considered: 601008
    ll=[3010460,3010680,3010330,7041140,7030960,7030961]
    return sorted(float(x) for x in ll)

def apply_transformation(transformation_rule,new_expr):
    #print(transformation_rule.__name__)
    sig_params = inspect.signature(transformation_rule.__call__).parameters
    for i in range(0,len(new_expr)):
        #print(transformation_rule.__name__ + ": " + ''.join(new_expr[i-1].get_output()) + "+" + ''.join(new_expr[i].get_output()))
        if isinstance(new_expr[i]._data,Suffix):
            if i>0 :
                if isinstance(new_expr[i-1]._data,Dhaatu):
                    dhaatu_index=i-1
                    if 'anga_node' in sig_params :
                        new_expr[i].set_output(transformation_rule,anga_node=new_expr[dhaatu_index])           
                    if 'suffix_node' in sig_params :
                        new_expr[dhaatu_index].set_output(transformation_rule,suffix_node=new_expr[i])
                else:
                    if 'suffix_node' in sig_params :
#                        if (transformation_rule.__name__=="saarvadhaatukaardhadhaatukayoH_703084"):
#                            j = 1
                        new_expr[i-1].set_output(transformation_rule,suffix_node=new_expr[i])
                    if 'anga_node' in sig_params :
                        
                        
                        
                        ## WORKS: new_expr[i-1].set_output(transformation_rule,anga_node=new_expr[i])
                        new_expr[i].set_output(transformation_rule,anga_node=new_expr[i-1])

                    
            if 'anga_node' not in sig_params  and 'suffix_node' not in sig_params :
                new_expr[i].set_output(transformation_rule)
                
            
                    
    return new_expr
            
            
    
def apply_dhaatu_lopa(dhaatu_node):
    if not isinstance(dhaatu_node,Node):
        raise ValueError("Need Node")

    if not isinstance(dhaatu_node._data,Dhaatu):
        raise ValueError("Need Dhaatu")
        
    MAX_TIMES=10000
    lopa_functions = [halantyam_1030030]
    
    for lopafunc in  lopa_functions :
        not_done=True
        while not_done:
            prev_output=dhaatu_node.get_output()
            dhaatu_node.set_output(lopafunc)
            if dhaatu_node.get_output() == prev_output:
                not_done=False
                
    return dhaatu_node
        
def apply_lopa(suffix_node):
    """
    Only tripaadii sutras need to be re-applied
    others can potentially be applied
    """
    
    if not isinstance(suffix_node,Node):
        raise ValueError("Need Node")

    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("Need Suffix")
    MAX_TIMES=10000
    lopa_functions = [lashakvataddhite_1030080, aadirNciXtuXdavaH_1030050, \
                      halantyam_1030030, chuXtuu_103070,upadesheajanunaasikait_1030020, \
                      itashcha_3041000]
    
    for lopafunc in  lopa_functions :
        not_done=True
        while not_done:
            prev_output=suffix_node.get_output()
            suffix_node.set_output(lopafunc)
            if suffix_node.get_output() == prev_output:
                not_done=False
                
    return suffix_node
                    
                
    for pos in reversed(new_inserts):
        new_expr.insert(pos,Node(new_inserts[pos],parent1=None))
    return new_expr


def apply_prepend(prepend_rule,new_expr):
    #TODO: trace history of insertion by modifying the output of both sides of the insertion
    new_inserts=OrderedDict()
    for i in range(1,len(new_expr)):
        if isinstance(new_expr[i]._data,Suffix):
            # reducing expression with combination
            # this involves appending plain-strings (that cannot be reduced further)
            sig_params = inspect.signature(prepend_rule.__call__).parameters
            if 'dhaatu_node' in sig_params :         
                to_prepend = prepend_rule()(dhaatu_node=new_expr[i-1],suffix_node=new_expr[i])
                if to_prepend :
                    new_inserts[i-1]={'node_data':to_prepend ,'input_indices':(i-1,i),'rule':prepend_rule}
                
    for pos in reversed(new_inserts):
        dat=new_inserts[pos]
        new_node = Node(dat['node_data'],parent1=new_expr[dat['input_indices'][0]],parent2=new_expr[dat['input_indices'][1]])
        new_node.assign_output_properties(rule=dat['rule'], dhaatu_node=new_node.get_parent1(),suffix_node=new_node.get_parent2())
        new_expr[pos].assign_output_properties(rule=dat['rule'], dhaatu_node=new_node.get_parent1(),suffix_node=new_node.get_parent2())
        new_expr.insert(pos,new_node)
    return new_expr

def apply_insertion(insertion_rule, new_expr):
    #TODO: trace history of insertion by modifying the output of both sides of the insertion
    new_inserts=OrderedDict()
    for i in range(1,len(new_expr)):
        if isinstance(new_expr[i]._data,Suffix):
            # reducing expression with combination
            # this involves appending plain-strings (that cannot be reduced further)
            sig_params = inspect.signature(insertion_rule.__call__).parameters
            if 'dhaatu_node' in sig_params :         
                to_insert = insertion_rule()(dhaatu_node=new_expr[i-1],suffix_node=new_expr[i])
                if to_insert :
                    new_inserts[i]={'node_data':to_insert ,'input_indices':(i-1,i),'rule':insertion_rule}
                    #new_inserts[i]=to_insert 
            if 'presuffix_node' in sig_params and isinstance(new_expr[i-1]._data,Suffix):         
                to_insert = insertion_rule()(presuffix_node=new_expr[i-1],suffix_node=new_expr[i])
                if to_insert :
                    new_inserts[i]={'node_data':to_insert ,'input_indices':(i-1,i),'rule':insertion_rule}
                    #new_inserts[i]=to_insert     
    for pos in reversed(new_inserts):
        #parent1 = new_expr[pos-1]
        #parent2 = new_expr[pos]
        
        #new_expr.insert(pos,Node(new_inserts[pos],parent1=parent1,parent2=parent2))
        
        
        dat=new_inserts[pos]
        new_node = Node(dat['node_data'],parent1=new_expr[dat['input_indices'][0]],parent2=new_expr[dat['input_indices'][1]])
        #assigning properties to both sides of the insertion
        new_node.assign_output_properties(rule=dat['rule'], node1=new_node.get_parent1(),node2=new_node.get_parent2())
        new_expr[pos].assign_output_properties(rule=dat['rule'], node1=new_node.get_parent1(),node2=new_node.get_parent2())
        new_expr.insert(pos,new_node)


    return new_expr


def process_until_finish(expr):
    output_processed_string = lambda e: ''.join(reduce(lambda x ,y : x + y.get_output(),  e, []))
    old_string = output_processed_string (expr)
    new_expr = process_list(expr)
    if output_processed_string (new_expr)==old_string:
        return expr
    else:
        return process_until_finish(new_expr)
        
def process_list(expr):
    all_sutras= get_sutras_ordered()
    if any(not isinstance(entry,Node) for entry in expr):
        raise ValueError("Only nodes are to be present")
    new_expr = expr.copy()
    
    # the logic for applying it-lopa is that 
    # all suffixes are searched and then lopa-application is applied
    # the lopa changes the suffix's state
    
    
    suffix_indices= [ index for index,node in enumerate(new_expr) if isinstance(node._data,Suffix)]
    dhaatu_indices= [ index for index,node in enumerate(new_expr) if isinstance(node._data,Dhaatu)]
        
    # apply lopa to suffixes
    for suffix_index in suffix_indices:
       suffix_node  = new_expr[suffix_index]
       new_expr[suffix_index ] = apply_lopa(suffix_node)
    
    # apply lopa to dhaatus
    for dhaatu_index in dhaatu_indices:
       dhaatu_node  = new_expr[dhaatu_index ]
       new_expr[dhaatu_index ] = apply_dhaatu_lopa(dhaatu_node  )
      
    # apply insertions
    for insertion_sutra_id in insertion_sutras():
        new_expr = apply_insertion(all_sutras[insertion_sutra_id],new_expr)
      
        
    # apply insertions
    for prepend_sutra_id in prepend_sutras():
        new_expr = apply_prepend(all_sutras[prepend_sutra_id],new_expr)
    # apply transformations until there is no change in the expression
    for transformation_ruleid in transformation_sutras():        
        #print("transformation_ruleid="+str(transformation_ruleid))
        new_expr = apply_transformation(all_sutras[transformation_ruleid],new_expr)
    
    return new_expr

def output_string (expr):
    return ''.join(reduce(lambda x ,y : x + y.get_output(),  process_until_finish(expr), []))

def test_siddhis ():
    
    
    assert output_string ([Node(Dhaatu(parse_string("bhajNN")),parent1=None),Node(Suffix("ghaNc"),parent1=None)]) == "bhaaga"
    assert output_string ([Node(Dhaatu(parse_string("NniiNN")),parent1=None),Node(Suffix("Nnvul"),parent1=None)]) == "naayaka"
    assert output_string ([Node(Dhaatu(parse_string("bhuu")),parent1=None),Node(Suffix("tip",lakaara='laXt'),parent1=None)]) == "bhavati"
    assert output_string ([Node(Dhaatu(parse_string("bhuu")),parent1=None),Node(Suffix("tas",lakaara='laXt'),parent1=None)]) == "bhavatas"
    assert output_string ([Node(Dhaatu(parse_string("bhuu")),parent1=None),Node(Suffix("mip",lakaara='laXt'),parent1=None)]) == "bhavaami"
    assert output_string ([Node(Dhaatu(parse_string("paXthNN")),parent1=None),Node(Suffix("tip",lakaara='luXt'),parent1=None)]) == "paXthitaa"
    assert output_string ([Node(Dhaatu(parse_string("chiNN")),parent1=None),Node(Suffix("tip",lakaara='luNg'),parent1=None)] ) == "achaiXshiit"
    assert output_string ([Node(Dhaatu(parse_string("paXthNN")),parent1=None),Node(Suffix("tip",lakaara='lRiXt'),parent1=None)]) == "paXthiXshyati"
    assert output_string ([Node(Dhaatu(parse_string("paXthNN")),parent1=None),Node(Suffix("tip",lakaara='liXt'),parent1=None)]) == "papaaXtha"
    #assert output_string ([Node(Dhaatu(parse_string("paXthNN")),parent1=None),Node(Suffix("tas",lakaara='liXt'),parent1=None)]) == "peXthatuH"
    # liNg is aardhadhaatuk in aashir-liNg


def generate_tibaadi(dhaatu_string):
    
    res = {}
    las = ('tip','tas','jhi','sip','thas','tha','mip','vas','mas')
    
    lakaaras = ('liXt','lRiXt','laXt','luXt','luNg')
    for lakaara_string in lakaaras :
        print(lakaara_string )
        res[lakaara_string ] = []
        for la in las:
            dhaatu_node = Node(Dhaatu(parse_string(dhaatu_string)),parent1=None)
            result = output_string ([dhaatu_node, Node(Suffix(la,lakaara=lakaara_string),parent1=None)])
            res[lakaara_string ].append(result)
    return res
    

def generate_subaadi(sup_expression,linga):
    if not isinstance(sup_expression,list):
        raise ValueError("sup_expression must be a list of Nodes")
    if any (not isinstance(x,Node) for x in sup_expression):
        raise ValueError("sup_expression must be a list of Nodes")
    res = []
    sups = ('sNN','au','jas','am','auXt','shas', 'Xtaa','bhyaam','bhis',
            'Nge','bhyaam','bhyas','Ngasi','bhyaam','bhyas',
            'Ngas','os','am','Ngi','os','sup')
    

    for sup_string in sups :
        cur_sup_expression=deepcopy(sup_expression)
        cur_sup_expression.append(Node(Suffix(sup_string,linga=linga),parent1=None))        
        #print(sup_string )
        result = output_string (cur_sup_expression)
        #print(sup_string + " gives " +result )
        #print(result)
        res.append(result)
    return res

    
F=False
T=True

if F:
    test_siddhis ()
    #print("Test")
    #f=Functor()
    #f(2)
    
else:   
    
    #pprint(generate_tibaadi("paXthNN"))   ;sys.exit(0)
    if T:
        sup_expr = [Node(Dhaatu(parse_string("rajNN")),parent1=None),Node(Suffix("ghaNc"),parent1=None)]
        pprint(generate_subaadi(sup_expr ,linga=1))   ;
        sys.exit(0)
    
    else:
        #Xtaa
        #
        expression=[Node(Dhaatu(parse_string("bhuu")),parent1=None),Node(Suffix("tip",lakaara='laXt'),parent1=None)]
        #expression=[Node(Dhaatu(parse_string("rajNN")),parent1=None),Node(Suffix("ghaNc"),parent1=None),Node(Suffix("Xtaa",linga=1),parent1=None)]
        #expression=[Node(Dhaatu(parse_string("paXthNN")),parent1=None),Node(Suffix("tas",lakaara='liXt'),parent1=None)]
    
    
    # sorting order is increasing in general but can be superseded by nitya condition (if nitya occurs in a later sutra then that later sutra takes advantage) 
    # which in turn would be superseded by the minimal condition criteria (antaraNga) 
    # The only exception is when there is a an exception that prevents application
    
    print("NEXT: luNglaNglRiNgkXshvaXdudaataH has prepending issue because we don't trace insertion/prepending of vikaraNna histories. This should allow the immediate dhaatu in context.")
    # for paXtheta - we need to have for liNg : yaasuXtparasmaipadeXshuudaatto Ngichcha 3.4.103 and then ato yeyaH (because of a-ending paXtha after shap)
    print("PENDING :rename of anga_node to nonsuffix_node, https://ashtadhyayi.com/sutraani/6/4/120, ,eruH ")
    
    
    
    processed_expr=(process_until_finish(expression))

    output_processed_string = lambda expr: ''.join(reduce(lambda x ,y : x + y.get_output(),  expr, []))
    print(output_processed_string (processed_expr))
    print("DONE")
    if F:
        print("===")
        pprint(processed_expr[0]._output)
        print("===")
        pprint(processed_expr[1]._output)
        
        
        print("===")
        pprint(processed_expr[2]._output)