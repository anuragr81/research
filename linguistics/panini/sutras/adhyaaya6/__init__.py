from ..common_definitions import Suffix,Node, Dhaatu

def NnonaH_601063(node,suffix_node):
    if not isinstance   (node,Node):
        raise    ValueError ("node must be of type Node")
    if not isinstance   (node._data,Dhaatu):
        raise    ValueError ("node-data must be of type Dhaatu")
    dhaatu=node._data._data
    if dhaatu[0] == "Nn":
        return ['n']+dhaatu[1:]
    else:
        return dhaatu

def echoayavaayaavaH_601075(node):
    node_output=node.get_output()
    
    if node_output[0] == "e":
        return "ay" + node_output[1:]
    if node_output[0] == "o":
        return "av"+ node_output[1:]
    if node_output[0] == "ai":
        return "aay"+ node_output[1:]
    if node_output[0] == "au":
        return "aav"+ node_output[1:]
        
    return node_output


def yasyeticha_604148(node,suffix_node):
    suffix = suffix_node._data
    anga_str=node.get_output()
    if not isinstance(suffix ,Suffix):
        raise ValueError("suffix must be of Suffix type")
    if suffix.is_taddhita or suffix.get_suffix()[0] in ('i','ii'):
        return anga_str[0:-1]
    return anga_str