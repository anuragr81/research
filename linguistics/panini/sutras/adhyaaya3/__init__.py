from ..common_definitions import Suffix, Node, Dhaatu,tiNg_pratyayaaH


def kartarishap_301068(dhaatu_node,suffix_node):
    if isinstance(dhaatu_node._data,Dhaatu) and \
        isinstance(suffix_node._data,Suffix) and \
            suffix_node._data.is_saarvadhaatuka() and \
                suffix_node._data._lakaara not in ('liXt',) and \
                ''.join(suffix_node._data._suffix) != 'shap':
             return Suffix("shap")
    return []
        
def parasmaipadaanaaMNnalatususXthalaXthusaNnalvamaaH_304082(node,anga_node):
    if not isinstance(node,Node):
        raise ValueError("node must be of Node type")
    if not isinstance(anga_node,Node):
        raise ValueError("anga_node must be of Node type")
    if isinstance(node._data,Suffix) :
        suffix_name =''.join(node._data._suffix)
        if suffix_name in tiNg_pratyayaaH():
            mapping= {'tip':['Nn','a','l'], 'tas':['a','t','u','s'], 'jhi':['u','s'], 
                      'sip':['Xth','a','l'], 'Xthas':['a','Xth','u','s'],'Xtha':['a'], 
                      'mip':['Nn','a','l'], 'vas':['v','a'], 'mas':['m','a']}
        
            return mapping[suffix_name], _data
    return node.get_output()