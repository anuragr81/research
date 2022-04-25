import re
from ..common_definitions import pratyaahaara, ach, Node, Suffix
from ..common_definitions import get_aadesha_sutras, get_vriddhi_sutras


class sasajuXshoruH_8020660:
    def __init__(self):
        self._types={'node':['literal']}
        
    def __call__(self,node):
        
        if not isinstance(node,Node):
            raise ValueError("node must of type Node")
    
        pada = node.get_output()
        return pada
        if pada[-1]=="s":
            return pada[0:-1] + ["r"]
    
        if pada == "sajuXsh":
            raise ValueError("sajuXsh not supported yet")
        return pada

class kharavasaanayorvisarjaniiyaH_8010150:
    def __init__(self):
        self._types={'node':['literal']}
        
    def __call__(self,node):
        # must be used in avasaana
        if not isinstance(node,Node):
            raise ValueError("node must of type Node")
        pada = node.get_output()
        return pada
        khar = pratyaahaara('kh','r')
        if pada[-1]=="r" and (pada[-2] in khar or pada[-2] in ach()):
            return pada[0:-1] + ['H']
        return pada




class aadeshapratyayoH_8030059:
    def __init__(self):
        self._types={'anga_node':[],'node':[Suffix ,'literal']}
        
    def __call__(self,anga_node ,node):        
        if not isinstance(node,Node):
            raise ValueError("suffix must of type Node")
        
        if not isinstance(node._data,Suffix):
            raise ValueError("suffix must of type Suffix")
        
        
        if not isinstance(anga_node,Node):
            raise ValueError("anga_node must of type Node")
        
        past_rules = [(x['rule']) for x in node._output if 'rule' in x]
        past_rule_ids = [ float(re.search('([0-9]+_*[0-9]*)$',x.__name__).group(1)) for x in past_rules ]
        aadesha_instances = set([float(j) for j in get_aadesha_sutras()]).intersection(set(past_rule_ids))
        if aadesha_instances:
            ##it is an aadesha sutra
            if node.get_output()[0] == 's':
                if anga_node.get_output()[-1] in ('i','ii','u','uu') :
                    return ['Xsh'] + node.get_output()[1:]
                
                past_anga_rules = [(x['rule']) for x in anga_node._output if 'rule' in x]
                past_anga_rule_ids= [ float(re.search('([0-9]+_*[0-9]*)$',x.__name__).group(1)) for x in past_anga_rules ]
                
                if set([float(j) for j in get_vriddhi_sutras()]).intersection(set(past_anga_rule_ids)):
                    if anga_node.get_output()[-1] in ('ai','au','aa'):
                        return ['Xsh'] + node.get_output()[1:]
                
                
        return node.get_output()
        