from ..common_definitions import Suffix, sup_pratyayaaH,Node

def supodhaatupraatipadikayoH_2040710(state):
    suffixes = [ {'pos':i,'data':st} for i,st in enumerate(state) if isinstance(st,Suffix)]
    if len(suffixes)>1:
        # get rid of the second last suffix if it's a sup
        suffix_string = suffixes[-2]['data']  if isinstance(suffixes[-2]['data'],str) else ''.join(suffixes[-2]['data'].get_suffix())
        if suffix_string in sup_pratyayaaH():
            return state[0:suffixes[-2]['pos']] + state[(suffixes[-2]['pos']+1) :]
    return state
    

def luXtaHprathamasyaXdaaraurasaH_2040850(node):
    """
    Transformation sutra

    """
    if not isinstance(node,Node):
        raise ValueError("node must be of Node type")

    if isinstance(node._data,Suffix) :
        
        suffix_data=[x['output'] for x in node._output if 'new' in x and x['new']][-1]
        suffix_name =''.join(suffix_data)
        if suffix_name in ("tip","tas","jhi",) and node._data._lakaara == 'luXt':
            mapping= {'tip':['Xd','aa'], 'tas':['r','au'], 'jhi':['r','a','s']}        
            return {'output':mapping[suffix_name],'mutate':True}
    return node.get_output()