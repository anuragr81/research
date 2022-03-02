from ..common_definitions import Suffix,Node, Dhaatu, ach, hal

def _604120():
    
def liXtidhaatoranabhyaasasya_601008(node,suffix_node):
    if not isinstance(suffix_node,Node):
        raise    ValueError ("suffix_node must be of type Node")
    if not isinstance(node,Node):
        raise    ValueError ("node must be of type Node")
    if isinstance   (node._data,Dhaatu):
        if suffix_node._data._lakaara == 'liXt':
            applied_rules= [int(x['rule'].__name__.split('_')[-1]) for x in node._output if 'rule' in x]
            if 601008 not in applied_rules :
                hals = [i for i,x in enumerate(node.get_output()) if x in hal() and i>0]
                if hals:
                    # ignore hals after second
                    # 6.4.120 if there is asaMyoga (and liXt) then it's treated as kit and if anga does 
                    # not have any adesha in the aadi (history to be checked) then there would no dvitva and 
                    # the akaar between the aadi hal and the one after aadi would become e
                    # 
                    _604120()
                    return node.get_output()[:hals[0]]+node.get_output()
    return node.get_output()

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

def bhuvovugluNgliXtoH_604088(node,suffix_node):
    if not isinstance(suffix_node,Node):
        raise    ValueError ("suffix_node must be of type Node")
    if not isinstance(node,Node):
        raise    ValueError ("node must be of type Node")

    if isinstance(node._data,Dhaatu) and node.get_output()==["bh","uu"] \
        and ( suffix_node._data._lakaara in ('luNg','liXt') or suffix_node._data._suffix[0] in ach()):
        return ['bh','uu','v']
        

        
    return  node.get_output()
        
def yasyeticha_604148(node,suffix_node):
    suffix = suffix_node._data
    anga_str=node.get_output()
    if not isinstance(suffix ,Suffix):
        raise ValueError("suffix must be of Suffix type")
    
    #pick last value
    suffix_data=[x['output'] for x in suffix_node._output if 'new' in x and x['new']][-1]
    if suffix.is_taddhita or suffix_data[0] in ('i','ii'):
        return anga_str[0:-1]
    return anga_str


