from ..common_definitions import Suffix, Node, Dhaatu


def kartarishap_301068(dhaatu_node,suffix_node):
    if isinstance(dhaatu_node._data,Dhaatu) and \
        isinstance(suffix_node._data,Suffix) and \
            suffix_node._data.is_saarvadhaatuka() and \
                suffix_node._data._lakaara not in ('liXt',) and \
                ''.join(suffix_node._data._suffix) != 'shap':
             return Suffix("shap")
    return []
        
def parasmaipadaanaaMNnalatususXthalaXthusaNnalvamaaH_304072(node,anga_node):
    if not isinstance(node,Node):
        raise ValueError("node must be of Node type")
    if not isinstance(anga_node,Node):
        raise ValueError("anga_node must be of Node type")
    if not isinstance(suffix_node,Node):
        raise ValueError("anga_node must be of Node type")
        
    {'tip':'Nnal', 'tas':'atus', 'jhi':'us', 'sip':'Xthal', 'Xthas':'aXthus','Xtha':'a', 'mip':'Nnal', 'vas':'va', 'mas':'ma'}