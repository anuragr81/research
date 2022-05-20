from ..common_definitions import Suffix,Node, Dhaatu, ach, hal, sup_pratyayaaH
from ..common_definitions import pratyaahaara, make_diirgha, guna_letters_for_aat
from ..common_definitions import vriddhi, list_past_rules_applied
from ..common_definitions import find_eldest_parent1_of_condition 
from ..common_definitions import find_eldest_parent2_of_condition


class liXtidhaatoranabhyaasasya_6010080:
    def __init__(self):
        self._types={'node':[Dhaatu],'suffix_node':[Suffix,'lakaara']}
        
    def __call__(self,node,suffix_node):
        if not isinstance(suffix_node,Node):
            raise    ValueError ("suffix_node must be of type Node")
        if not isinstance(node,Node):
            raise    ValueError ("node must be of type Node")
        if isinstance   (node._data,Dhaatu):
            if suffix_node._data._lakaara == 'liXt':
                applied_rules= [int(x['rule'].__name__.split('_')[-1]) for x in node._output if 'rule' in x]
                if 6010080 not in applied_rules :
                    hals = [i for i,x in enumerate(node.get_output()) if x in hal() and i>0]
                    
                    if hals:
                        if len(hals)==1 and hals[0] >1 and node.get_output()[hals[0]-1] in ('a',): # ekahalmadhye achaH, asaMyoga 
                            if len([x for x in node._output if 'new' in x])==1: # no adesha
                                if suffix_node._output[1]['inputs']['state']._data._lakaara  == 'liXt': # check on suffix
                                    if suffix_node._output[1]['inputs']['state']._data._suffix[-1]!='p':# cannot be p-it
                                        return  node.get_output()[:hals[0]-1] + ['e'] + node.get_output()[hals[0]:]
                            
                        # ignore hals after second
                        return node.get_output()[:hals[0]]+node.get_output()
        return node.get_output()

class NnonaH_6010630:
    def __init__(self):
        self._types={'node':[Dhaatu,'literal']}
        
    def __call__(self,node,suffix_node):    
        if not isinstance   (node,Node):
            raise    ValueError ("node must be of type Node")
        if not isinstance   (node._data,Dhaatu):
            return node.get_output()
        dhaatu_string=node.get_output()
        if dhaatu_string[0] == "Nn":
            return ['n']+dhaatu_string[1:]
        else:
            return dhaatu_string

class echoayavaayaavaH_6010750:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
    def __call__(self,node, suffix_node):
        if not node.get_output():
            return node.get_output()
        if not suffix_node.get_output():
            return node.get_output()
        if suffix_node.get_output()[0] not in ach():
            return node.get_output()
    
        
        node_output=node.get_output()
        if node_output[-1] == "e":
            return node_output[0:-1]+["a" ,"y"]
        if node_output[-1] == "o":
            return node_output[0:-1]+["a","v"]
        if node_output[-1] == "ai":
            return node_output[0:-1]+["aa","y"]
        if node_output[-1] == "au":
            return node_output[0:-1]+["aa","v"]
            
        return node_output


class aadguNnaH_6010840:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
    def __call__(self,node, anga_node):
        node_output= node.get_output()
        if not node.get_output():
            return node.get_output()                
        if anga_node.get_output():
            suffix_first_letter = node.get_output()[0]
            if anga_node.get_output()[-1] in ('a','aa') and suffix_first_letter  in pratyaahaara('i','k'):
                
                return guna_letters_for_aat(node.get_output()[0])+node.get_output()[1:]
            
            
        return node_output


class aadguNnaH_6010841:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
    def __call__(self,node, suffix_node):
        
        node_output= node.get_output()
        if not node.get_output():
            return node.get_output()
        if isinstance(suffix_node._data,Suffix) and suffix_node.get_output():         
            # no check on suffix_node because that is expected to have changed after the call
            if node_output[-1]  in ('a','aa') and list_past_rules_applied(suffix_node) and list_past_rules_applied(suffix_node)[-1]==6010840:
                return node_output[:-1] #skip because the other instance of the rule uses the diirgha
        return node_output


class vRiddhirechi_6010850:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
    def __call__(self,node, anga_node):
        node_output= node.get_output()
        if not node.get_output():
            return node.get_output()                
        if anga_node.get_output():
            suffix_first_letter = node.get_output()[0]
            if anga_node.get_output()[-1] in ('a','aa') and suffix_first_letter  in pratyaahaara('e','ch'):
                
                return [vriddhi(node.get_output()[0])]+node.get_output()[1:]
            
            
        return node_output


class vRiddhirechi_6010851:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
    def __call__(self,node, suffix_node):
        node_output= node.get_output()
        if not node.get_output():
            return node.get_output()
        if isinstance(suffix_node._data,Suffix) and suffix_node.get_output():                  
            if node_output[-1]  in ('a','aa') and suffix_node.get_output()[0] in pratyaahaara('e','ch'):
                return node_output[:-1] #skip because the other instance of the rule uses the diirgha
        return node_output


"""
class eNGipararuupam_6010910:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
    def __call__(self,node, anga_node):
        node_output= node.get_output()
        if not node.get_output():
            return node.get_output()
        
        if isinstance(anga_node._data,Suffix):
            pratyaya =''.join(anga_node._data._suffix)
            if pratyaya in sup_pratyayaaH() :
                # suffix is sup
                    if node.get_output()[-1] in pratyaahaara('e','Ng'):
                        return node_output[1:]
            
        return node_output

"""


class akaHsavarNnediirghaH_6010970:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
    def __call__(self,node, anga_node):
        node_output= node.get_output()
        if not node.get_output():
            return node.get_output()
        
        if isinstance(node._data,Suffix):
            
            pratyaya  = ''.join(node._data._suffix)
            if node.get_output() [0] in pratyaahaara('a','k') and anga_node.get_output():
                puurva_varNna=anga_node.get_output()[-1]
                if node.get_output()[0] == puurva_varNna:
                    # ami puurvaH
                    if pratyaya in sup_pratyayaaH() and pratyaya == 'am':
                        return node_output
                    else:
                        return node_output[1:] #skip because the other instance of the rule uses the diirgha
            
        return node_output

class akaHsavarNnediirghaH_6010971:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
    def __call__(self,node, suffix_node):
        node_output= node.get_output()
        if not node.get_output():
            return node.get_output()
        
        if isinstance(suffix_node._data,Suffix):                        
            # rule last applied was the other half of the rule where suffix's first
            # letter has been erased
            suffix_state = suffix_node._output[-1]['inputs']['state_output'] if list_past_rules_applied(suffix_node) and list_past_rules_applied(suffix_node)[-1] == 6010970 else suffix_node.get_output()
            #suffix_node._output[-1]['inputs']['state_output']
            if node_output[-1] in pratyaahaara('a','k') :
                suffix_string = ''.join(suffix_node._data._suffix)
                puurva_varNna=node_output[-1]
                if suffix_state[0] == puurva_varNna:
                    if suffix_string in sup_pratyayaaH() and suffix_string == 'am':
                        return node_output
                    else:
                        return [make_diirgha(node_output[0])] + node_output[1:]
            
        return node_output

class prathamayoHpuurvasavarNnaH_6010980:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
    def __call__(self,node, anga_node):
        node_output= node.get_output()
        if not node.get_output():
            return node.get_output()
        
        if isinstance(node._data,Suffix):
            pratyaya =''.join(node._data._suffix)
            if pratyaya in sup_pratyayaaH()[0:6] : # only prathhama and dvitiiyaa considered
                # suffix is sup
                    if anga_node.get_output() and anga_node.get_output()[-1] in ach() and node.get_output()[0] in ach():
                       if node.get_output()[0] not in pratyaahaara('i', 'ch'): ## naadichi
                           return node_output[1:]
            
        return node_output

class tasmaatchhasonaHpuMsi_6010990:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
        
    def __call__(self,node):
        node_output= node.get_output()
        if not node.get_output():
            return node.get_output()
        
        
        if isinstance(node._data,Suffix):
            pratyaya =''.join(node._data._suffix)
            if pratyaya in sup_pratyayaaH()[0:6] and pratyaya=='shas' : # only shas considered
                #last_rule_applied = list_past_rules_applied(node._prev)[-1]
                # TODO: ensure that this happens only just after 6010980 (prathamayoHpuurvasavarNnaH) is applied
                if node._data._linga == 1 :
                    return node.get_output()[0:-1] + ['n']
        
        
        return node_output

"""
class amipuurvaH_6011030:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':['literal']}
    def __call__(self,node, anga_node):
        node_output= node.get_output()
        if not node.get_output():
            return node.get_output()
        
        if isinstance(anga_node._data,Suffix):
            pratyaya =''.join(anga_node._data._suffix)
            if pratyaya in sup_pratyayaaH() and pratyaya == 'am':
                # suffix is sup
                    if node.get_output()[-1] in pratyaahaara('a','k'):
                        return node_output[1:]
            
        return node_output

"""

class luNglaNglRiNgkShvaXdudaattaH_6040710:
    def __init__(self):
        self._types={'dhaatu_node':[Dhaatu],'suffix_node':[Suffix,'literal','lakaara']}
        self._ruletype=['prepend']
        
    def __call__(self,dhaatu_node,suffix_node):
        e1=find_eldest_parent1_of_condition(suffix_node,lambda x : isinstance(x ,Node) and isinstance(x._data,Suffix) and x._data._lakaara in ('luNg','laNg','lRiNg') )
        e2=find_eldest_parent2_of_condition(suffix_node,lambda x : isinstance(x ,Node) and isinstance(x._data,Suffix) and x._data._lakaara in ('luNg','laNg','lRiNg') )

        if e1 is None:
            if e2 is None :
                return []
            else :
                effective_suffix_node = e2
        else:
            effective_suffix_node = e1
            
        if isinstance(dhaatu_node ._data,Dhaatu) and \
            isinstance(effective_suffix_node._data,Suffix) :
                if effective_suffix_node._data._lakaara in ('luNg','laNg','lRiNg') :
                    past_rules_applied = [int(x['rule'].__name__.split("_")[-1]) for x in dhaatu_node._output if 'rule' in x]
                    if 6040710 not in past_rules_applied  :
                        #raise ValueError("aXt needs to be checked in dhaatu-nodes modification (while prepending)")
                        return Suffix("aXt")
        return []
  
class bhuvovugluNgliXtoH_6040880:
    def __init__(self):
        self._types={'node':[Dhaatu],'suffix_node':[Suffix,'literal','lakaara']}
        
    def __call__(self,node,suffix_node):   
        if not isinstance(suffix_node,Node):
            raise    ValueError ("suffix_node must be of type Node")
        if not isinstance(node,Node):
            raise    ValueError ("node must be of type Node")
    
        if isinstance(node._data,Dhaatu) and node.get_output()==["bh","uu"] \
            and ( suffix_node._data._lakaara in ('luNg','liXt') or suffix_node._data._suffix[0] in ach()):
            return ['bh','uu','v']            
    
            
        return  node.get_output()

class ataekahalmadhyeanaadeshaaderliXti_6041200:
    def __init__(self):
        self._types={'node':['stateupdate'],'suffix_node':[Suffix,'literal','lakaara']}
    def __call__(self,node,suffix_node):
        if not isinstance(suffix_node,Node):
            raise    ValueError ("suffix_node must be of type Node")
        if not isinstance(node,Node):
            raise    ValueError ("node must be of type Node")
        if suffix_node._data._lakaara == 'liXt':
            applied_rules= [int(x['rule'].__name__.split('_')[-1]) for x in node._output if 'rule' in x]
            if 601008 in applied_rules :
                convert_e = lambda x : x if x not in ('a','aa',) else 'e'
                if len([x['output'] for x in node._output if 'new' in x and x['new']]) == 1: # there has been no adesha to anga
                    if node.get_output()[-1] not in ach() and node.get_output()[-2] in ach() : # asaMyoga => kit
                        return node.get_output()[:-2] + [convert_e(node.get_output()[-2])] + node.get_output()[-1:]   
            
        return node.get_output()


class yasyeticha_6041480:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':[Suffix,'stateupdate','literal']}
        
    def __call__(self,node,suffix_node):
    
        suffix = suffix_node._data
        anga_str=node.get_output()
        if not isinstance(suffix ,Suffix):
            raise ValueError("suffix must be of Suffix type")
        
        #pick last value
        suffix_data=[x['output'] for x in suffix_node._output if 'new' in x and x['new']][-1]
        if False: # TODO: re-enable
            if suffix.is_taddhita or suffix_data[0] in ('i','ii'):
                 return anga_str[0:-1]
        return anga_str
