from ..common_definitions import vriddhi,upadhaa, ach, Suffix, Node


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
        
    suffix=suffix_node._data

    it_chars = suffix.get_itchars()
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
    suffix=suffix_node._data
    anga_string= node.get_output()
    if anga_string[-1] in ach() and (suffix.get_suffix()[-1] in ('Nc','Nn') or suffix.get_suffix()[0] in ('Nc','Nn')):
        return anga_string[0:-1] + [vriddhi(anga_string[-1])]
    return node.get_output() 


def chajoHkughiNnNnyatoH_703052(node,suffix_node):
    if not isinstance(node,Node):
        raise ValueError("anga_node must of type Node")
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
    suffix=suffix_node._data
    chakaar_to_ku = lambda y : 'k' if y=='ch' else y
    jakaar_to_ku = lambda y : 'g' if y=='j' else y
    if suffix.get_suffix()[0] in ('gh',) or suffix.get_suffix()[-1] in ('gh',) or str(suffix) == "Nyat":
        #return ''.join(jakaar_to_ku(chakaar_to_ku(j)) for j in x)
        return [jakaar_to_ku(chakaar_to_ku(j)) for j in node.get_output()]
    
    return node.get_output()


def aardhadhaatukasyeXdvalaadeH_704114(node,suffix_node,is_aardhadhaatuka=False):    
    if not isinstance(node,Node):
        raise ValueError("node must of type Node")
    if is_aardhadhaatuka:
        return node+ ["i","Xt"]
    else:
        return node.get_output()
    
    
    