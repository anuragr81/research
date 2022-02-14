from ..common_definitions import vriddhi,upadhaa, ach, Suffix, Node


def yuvoranaakau_701001(anga,suffix_node):
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
        
    if not isinstance(anga,Node):
        raise ValueError("anga must of type Node")
    suffix_string= suffix_node.get_output()
    
    if suffix_string[-2:] == ["y","u"]:
        return anga, suffix_string[0:-2] + ["a","n","a"]
    if suffix_string[-2:] == ["v","u"]:
        return anga, suffix_string[0:-2] + ["a","k","a"]

    return anga , suffix_string




def aayaneyiiniiyiyaH_phaXdhakhachchhaghaaM_pratyayaadiinaaM_701002(anga ,suffix_node):
    
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
    
    
    if not isinstance(anga,Node):
        raise ValueError("anga must of type Node")
    pratyaya=suffix_node.get_output()
    letter = [0]
    if letter== "ph":
        return anga , ["aa","y","a","n"] + pratyaya[1:]
    elif letter == "Xdh":
        return anga , ["e","y"]+ pratyaya[1:]
    elif letter == "kh":
        return anga , ["ii","n"]+ pratyaya[1:]
    elif letter == "chh":
        return anga , ["ii","y"]+ pratyaya[1:]
    elif letter == "gh":
        return anga , ["i","y"]+ pratyaya[1:]
    else:
        return anga , pratyaya
    

        
def ataupadhaayaaH_702116(anga,suffix_node):
    """
    the it characters are used  to decide if upadha is implied in the anga or not
    """
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
    
    if not isinstance(anga,Node):
        raise ValueError("anga must of type Node")
        
    suffix=suffix_node._data

    it_chars = suffix.get_itchars()
    if 'Nc' in it_chars or 'Nn' in it_chars:
        #Ncit or Nnit
        upadhaa_pos = upadhaa(anga)
        return (anga[0:upadhaa_pos ]+[{'op':vriddhi, 'input' : anga[upadhaa_pos]}] + anga[upadhaa_pos+1:]), suffix
        
    else:
        return anga, suffix

def acho_NcNniti_702115(anga,suffix_node):
    if not isinstance(anga,Node):
        raise ValueError("anga must of type Node")
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
    suffix=suffix_node._data
    
    if anga[-1] in ach() and (suffix._data[-1] in ('Nc','Nn') or suffix._data[0] in ('Nc','Nn')):
        return anga[0:-1] + [vriddhi(anga[-1])], suffix
    return anga, suffix


def chajoHkughiNnNnyatoH_703052(anga,suffix_node):
    if not isinstance(anga,Node):
        raise ValueError("anga must of type Node")
    if not isinstance(suffix_node,Node):
        raise ValueError("suffix must of type Node")
    
    if not isinstance(suffix_node._data,Suffix):
        raise ValueError("suffix must of type Suffix")
    suffix=suffix_node._data
    chakaar_to_ku = lambda y : 'k' if y=='ch' else y
    jakaar_to_ku = lambda y : 'g' if y=='j' else y
    if suffix._data[0] in ('gh',) or suffix._data[-1] in ('gh',) or str(suffix) == "Nyat":
        #return ''.join(jakaar_to_ku(chakaar_to_ku(j)) for j in x)
        return anga, [jakaar_to_ku(chakaar_to_ku(j)) for j in anga]
    
    return anga, suffix


def aardhadhaatukasyeXdvalaadeH_704114(anga,suffix,is_aardhadhaatuka=False):    
    if not isinstance(anga,Node):
        raise ValueError("anga must of type Node")
    if is_aardhadhaatuka:
        return anga + ["i","Xt"], suffix
    else:
        return anga, suffix
    
    
    