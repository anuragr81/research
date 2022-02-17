from ..common_definitions import anunaasika, Suffix, ach, hal, chu, Xtu, Node

def uraNnraparaH_101050(a, b):
    if a[-1] == "Ri" :
        if b[0] == "a":
            return a[0:-1] + ["aa","r"]+ b
        if b[0] == "aa":
            return a[0:-1] + ["aa","r"]+ b
    return a + b


def halantyam_103003(node):
    
    antyam = node.get_output()[-1]
    if antyam in hal():
        return node.get_output()[:-1]
    else:
        return node.get_output()

def aadirNciXtuXdavaH_103005(node):
    if not isinstance(node,Node):
        raise ValueError("Must be Node")
    if not isinstance(node._data,Suffix):
        raise ValueError("Must be Suffix")
    if node.get_output()[0:2] in  (["Nc","i"],["Xt","u"],["Xd","u"]):
        return node.get_output()[2:]
    else:
        return node.get_output()

def chuXtuu_10307(node):    
    if not isinstance(node,Node):
        raise ValueError("Must be Node")
    suffix=node._data
    if not isinstance(suffix,Suffix):
        raise ValueError("Must be Suffix")
     
    if node.get_output() [0] in  chu() or node.get_output()[0] in Xtu():
        return node.get_output()[1:]
    
    return node.get_output()

    
def lashakvataddhite_103008(node):
    
    if not isinstance(node,Node):
        raise ValueError("Must be of Node type")
    suffix = node._data
    if not isinstance(suffix,Suffix):
        raise ValueError("Must be of Suffix type")
    if not suffix.is_taddhita and node.get_output()[0] in ("l","sh","k","kh","g","gh","Nc"):
        return node.get_output()[1:]

    return node.get_output()


def upadesheajanunaasikait_103002(aadesha):
    
    apply_rule = lambda x: x[0:-1] if x[-1] in anunaasika() or x[-1] in ach()  else x   

    return apply_rule(aadesha)




def yachibham_104018(suffix_str):
    return suffix_str[0] in ach() or suffix_str[0] == 'y' 
    
    