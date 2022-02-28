from ..common_definitions import Suffix, Node, Dhaatu


def kartarishap_301068(dhaatu_node,suffix_node):
    if isinstance(dhaatu_node._data,Dhaatu) and \
        isinstance(suffix_node._data,Suffix) and \
            suffix_node._data.is_saarvadhaatuka() and \
                suffix_node._data._lakaara not in ('liXt',) and \
                ''.join(suffix_node._data._suffix) != 'shap':
             return Suffix("shap")
    return []
        