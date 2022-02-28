from ..common_definitions import Suffix,Node, Dhaatu, ach

def NnonaH_601063(node,suffix_node):
    if not isinstance   (node,Node):
        raise    ValueError ("node must be of type Node")
    if not isinstance   (node._data,Dhaatu):
        return node.get_output()
    dhaatu_string=node.get_output()
    if dhaatu_string[0] == "Nn":
        return ['n']+dhaatu_string[1:]
    else:
        return dhaatu_string

def echoayavaayaavaH_601075(node, suffix_node):
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


def yasyeticha_604148(node,suffix_node):
    suffix = suffix_node._data
    anga_str=node.get_output()
    if not isinstance(suffix ,Suffix):
        raise ValueError("suffix must be of Suffix type")
    if suffix.is_taddhita or suffix.get_suffix()[0] in ('i','ii'):
        return anga_str[0:-1]
    return anga_str