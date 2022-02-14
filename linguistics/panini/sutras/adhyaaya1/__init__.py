from ..common_definitions import anunaasika, Suffix, ach, hal, chu, Xtu, Node

def uraNnraparaH_101050(a, b):
    if a[-1] == "Ri" :
        if b[0] == "a":
            return a[0:-1] + ["aa","r"]+ b
        if b[0] == "aa":
            return a[0:-1] + ["aa","r"]+ b
    return a + b


def halantyam_103003(upadesha):
    
    antyam = upadesha.get_output()[-1]
    if antyam in hal():
        return upadesha.get_output()[:-1]
    else:
        return upadesha.get_output()

def aadirNciXtuXdavaH_103005(upadesha_node):
    if not isinstance(upadesha_node,Node):
        raise ValueError("Must be Node")
    if not isinstance(upadesha_node._data,Suffix):
        raise ValueError("Must be Suffix")
    if upadesha_node.get_output()[0:2] in  (["Nc","i"],["Xt","u"],["Xd","u"]):
        return upadesha_node.get_output()[2:]
    else:
        return upadesha_node.get_output()

def chuXtuu_10307(suffix_node):    
    if not isinstance(suffix_node,Node):
        raise ValueError("Must be Node")
    suffix=suffix_node._data
    if not isinstance(suffix,Suffix):
        raise ValueError("Must be Suffix")
     
    if suffix_node.get_output() [0] in  chu() or suffix_node.get_output()[0] in Xtu():
        return suffix_node.get_output()[1:]
    
    return suffix_node.get_output()

    
def lashakvataddhite_103008(suffix_node):
    
    if not isinstance(suffix_node,Node):
        raise ValueError("Must be of Node type")
    suffix = suffix_node._data
    if not isinstance(suffix,Suffix):
        raise ValueError("Must be of Suffix type")
    if not suffix.is_taddhita and suffix_node.get_output()[0] in ("l","sh","k","kh","g","gh","Nc"):
        return suffix_node.get_output()[1:]

    return suffix_node.get_output()


def upadesheajanunaasikait_103002(aadesha):
    
    apply_rule = lambda x: x[0:-1] if x[-1] in anunaasika() or x[-1] in ach()  else x   

    return apply_rule(aadesha)




def yachibham_104018(suffix_str):
    return suffix_str[0] in ach() or suffix_str[0] == 'y' 
    
    