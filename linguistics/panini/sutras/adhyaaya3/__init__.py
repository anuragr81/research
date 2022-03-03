from ..common_definitions import Suffix, Node, Dhaatu,tiNg_pratyayaaH, ach


def kartarishap_3010680(dhaatu_node,suffix_node):
    if isinstance(dhaatu_node._data,Dhaatu) and \
        isinstance(suffix_node._data,Suffix) and \
            suffix_node._data.is_saarvadhaatuka() and \
                suffix_node._data._lakaara not in ('liXt',) and \
                ''.join(suffix_node._data._suffix) != 'shap':
             return Suffix("shap")
    return []
        

def syataasiilRiluXtoH_3010330(dhaatu_node,suffix_node):
    if isinstance(dhaatu_node._data,Dhaatu) and \
        isinstance(suffix_node._data,Suffix) :
            
            if suffix_node._data._lakaara in ('lRiXt',) and \
                ''.join(suffix_node._data._suffix) != 'sya':
                return Suffix("sya")
            if suffix_node._data._lakaara in ('luXt',) and \
                ''.join(suffix_node._data._suffix) != 'taas':                    
                return Suffix("taas")
    return []


def XdityabhasyaapianubandhakaraNnasaamarthyaat_3010331(node,suffix_node):
    
    if isinstance(node._data,Suffix) and isinstance(suffix_node._data,Suffix):
        first_suffix_data=[x['output'] for x in node._output if 'new' in x and x['new']][-1]
        second_suffix_data=[x['output'] for x in suffix_node._output if 'new' in x and x['new']][-1]
        if second_suffix_data[0] == 'Xd':
            ach_indices_in_output = [ i for i,x in enumerate(node.get_output()) if x in ach()]
            if ach_indices_in_output :                
                # picking the last ach onwards (Xti bhaaga)
                return node.get_output()[:ach_indices_in_output[-1]]
        
    return node.get_output()
    

    
def parasmaipadaanaaMNnalatususXthalaXthusaNnalvamaaH_3040820(node,anga_node):
    if not isinstance(node,Node):
        raise ValueError("node must be of Node type")
    if not isinstance(anga_node,Node):
        raise ValueError("anga_node must be of Node type")
    if isinstance(node._data,Suffix) :
        
        suffix_data=[x['output'] for x in node._output if 'new' in x and x['new']][-1]
        suffix_name =''.join(suffix_data)
        mapping= {'tip':['Nn','a','l'], 'tas':['a','t','u','s'], 'jhi':['u','s'], 
                      'sip':['Xth','a','l'], 'Xthas':['a','Xth','u','s'],'Xtha':['a'], 
                      'mip':['Nn','a','l'], 'vas':['v','a'], 'mas':['m','a']}
        if suffix_name in tiNg_pratyayaaH() and suffix_name in tiNg_pratyayaaH() in mapping: 
            return {'output':mapping[suffix_name],'mutate':True}
    return node.get_output()