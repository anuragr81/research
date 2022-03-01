from ..common_definitions import vriddhi,upadhaa, ach, Suffix, Node ,pratyaahaara, guNna


def yuvoranaakau_701001(anga_node,node):
    if not isinstance(node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
        
    if not isinstance(anga_node,Node):
        raise ValueError("anga_node must of type Node")
    suffix_string= node.get_output()
    
    if suffix_string[-2:] == ["y","u"]:
        return suffix_string[0:-2] + ["a","n","a"]
    if suffix_string[-2:] == ["v","u"]:
        return suffix_string[0:-2] + ["a","k","a"]

    return suffix_string




def aayaneyiiniiyiyaH_phaXdhakhachchhaghaaM_pratyayaadiinaaM_701002(anga_node ,node):
    
    if not isinstance(node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
    
    
    if not isinstance(anga_node,Node):
        raise ValueError("anga_node must of type Node")
    pratyaya=node.get_output()
    letter = [0]
    if letter== "ph":
        return ["aa","y","a","n"] + pratyaya[1:]
    elif letter == "Xdh":
        return ["e","y"]+ pratyaya[1:]
    elif letter == "kh":
        return ["ii","n"]+ pratyaya[1:]
    elif letter == "chh":
        return  ["ii","y"]+ pratyaya[1:]
    elif letter == "gh":
        return ["i","y"]+ pratyaya[1:]
    else:
        return pratyaya
    

        
def ataupadhaayaaH_702116(node,suffix_node):
    """
    the it characters are used  to decide if upadha is implied in the anga_node or not
    """
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
    
    if not isinstance(node,Node):
        raise ValueError("anga_node must of type Node")
        
    suffix_data=[x['output'] for x in suffix_node._output if 'new' in x and x['new']][-1]
    
    it_chars = (suffix_data[0],suffix_data[-1])
    if 'Nc' in it_chars or 'Nn' in it_chars:
        #Ncit or Nnit

        anga_string = node.get_output()
        upadhaa_pos = upadhaa(anga_string )
        return anga_string [0:upadhaa_pos ]+[vriddhi(anga_string[upadhaa_pos])] + anga_string [upadhaa_pos+1:]
        
    else:
        return node.get_output()

def acho_NcNniti_702115(node,suffix_node):
    if not isinstance(node,Node):
        raise ValueError("anga_node must of type Node")
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
    suffix_data=[x['output'] for x in suffix_node._output if 'new' in x and x['new']][-1]
    anga_string= node.get_output()
    if anga_string[-1] in ach() and (suffix_data[-1] in ('Nc','Nn') or suffix_data[0] in ('Nc','Nn')):
        return anga_string[0:-1] + [vriddhi(anga_string[-1])]
    return node.get_output() 


def chajoHkughiNnNnyatoH_703052(node,suffix_node):
    if not isinstance(node,Node):
        raise ValueError("anga_node must of type Node")
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
    
    suffix_data=[x['output'] for x in suffix_node._output if 'new' in x and x['new']][-1]
    chakaar_to_ku = lambda y : 'k' if y=='ch' else y
    jakaar_to_ku = lambda y : 'g' if y=='j' else y
    if suffix_data[0] in ('gh',) or suffix_data[-1] in ('gh',) or ''.join(suffix_data) == "Nnyat":
        #return ''.join(jakaar_to_ku(chakaar_to_ku(j)) for j in x)
        return [jakaar_to_ku(chakaar_to_ku(j)) for j in node.get_output()]
    
    return node.get_output()


def saarvadhaatukaardhadhaatukayoH_703084(node,suffix_node):
    if not isinstance(node,Node):
        raise ValueError("anga_node must of type Node")
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix must of type Node")    
    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
        
    if isinstance(node._data,Suffix):
        if node._data._suffix[-1] == node.get_output()[-1]:
            anga_string= node.get_output()
            if node._data._suffix[-1] in pratyaahaara('i','k'):
                return anga_string[0:-1]+[guNna(anga_string[-1])]
    else:
        if node._data._data[-1] == node.get_output()[-1]:
            anga_string= node.get_output()
            if node._data._data[-1] in pratyaahaara('i','k'):
                return anga_string[0:-1]+[guNna(anga_string[-1])]
    
    return node.get_output()
    
def aardhadhaatukasyeXdvalaadeH_704114(node,suffix_node):    
    if not isinstance(node,Node):
        raise ValueError("node must of type Node")
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix_node must of type Node")
        
    if not suffix_node._data.is_saarvadhaatuka() and suffix_node._data._suffix[0] in pratyaahaara('v','l') and False:
        return node.get_output() + ["i","Xt"]
    else:
        return node.get_output()
    
    
    
def atodiirghoyaNci_703101(node,suffix_node):
    if not isinstance(node,Node):
        raise ValueError("node must of type Node")
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix_node must of type Node")

    if node.get_output()[-1] == 'a':
        if suffix_node._data.is_saarvadhaatuka() and suffix_node._data._suffix[0] in pratyaahaara('y','Nc') :
            return node.get_output()[0:-1]+['aa']
    return node.get_output()