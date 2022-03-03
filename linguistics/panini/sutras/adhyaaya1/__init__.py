from ..common_definitions import anunaasika, Suffix, ach, hal, chu, Xtu, Node, Dhaatu,tiNg_pratyayaaH,sup_pratyayaaH

def uraNnraparaH_1010500(a, b):
    if a[-1] == "Ri" :
        if b[0] == "a":
            return a[0:-1] + ["aa","r"]+ b
        if b[0] == "aa":
            return a[0:-1] + ["aa","r"]+ b
    return a + b


def halantyam_1030030(node):
    #Check isinstance(node._data,Dhaatu) if necessary
    antyam = node.get_output()[-1]
    #works only once - not after the output has been modified with the call
    #if isinstance(node._data,Suffix):
    #    if ''.join(node._data._suffix) in tiNg_pratyayaaH() or ''.join(node._data._suffix) in sup_pratyayaaH():
    #        return node.get_output()
    
    
    node_data=[x['output'] for x in node._output if 'new' in x and x['new']][-1]
    if antyam in hal() and node_data[-1] == antyam:
        return node.get_output()[:-1]
    else:
        return node.get_output()

def aadirNciXtuXdavaH_1030050(node):
    if not isinstance(node,Node):
        raise ValueError("Must be Node")
    if not isinstance(node._data,Suffix):
        raise ValueError("Must be Suffix")
    if node.get_output()[0:2] in  (["Nc","i"],["Xt","u"],["Xd","u"]):
        return node.get_output()[2:]
    else:
        return node.get_output()

def chuXtuu_103070(node):    
    if not isinstance(node,Node):
        raise ValueError("Must be Node")
    suffix=node._data
    if not isinstance(suffix,Suffix):
        raise ValueError("Must be Suffix")
     
    if node.get_output() [0] in  chu() or node.get_output()[0] in Xtu():
        return node.get_output()[1:]
    
    return node.get_output()

    
def lashakvataddhite_1030080(node):
    
    if not isinstance(node,Node):
        raise ValueError("Must be of Node type")
    suffix = node._data
    if not isinstance(suffix,Suffix):
        raise ValueError("Must be of Suffix type")
    if not suffix.is_taddhita and node.get_output()[0] in ("l","sh","k","kh","g","gh","Nc"):
        return node.get_output()[1:]

    return node.get_output()


def upadesheajanunaasikait_1030020(aadesha):
    
    apply_rule = lambda x: x[0:-1] if x[-1] in anunaasika() or x[-1] in ach()  else x   

    return apply_rule(aadesha)




def yachibham_1040180(suffix_str):
    return suffix_str[0] in ach() or suffix_str[0] == 'y' 
    
    