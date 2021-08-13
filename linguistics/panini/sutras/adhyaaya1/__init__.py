from ..common_definitions import anunaasika, Suffix, ach, hal, chu, Xtu

def uraNnraparaH_101050(a, b):
    if a[-1] == "Ri" :
        if b[0] == "a":
            return a[0:-1] + ["aa","r"]+ b
        if b[0] == "aa":
            return a[0:-1] + ["aa","r"]+ b
    return a + b

def halantyam_103003(upadesha):
    
    antyam = upadesha[len(upadesha)-1]
    if antyam in hal():
        return len(upadesha)-1
    else:
        return None

def aadirNciXtuXdavaH_103005(upadesha):
    if upadesha[0:2] in  (["Nc","i"],["Xt","u"],["Xd","u"]):
        return [0,1]
    else:
        return []

def chuXtuu_10307(suffix):
    if not isinstance   (suffix,Suffix  ):
        raise ValueError    ("suffix must be of Suffix type")
    if suffix.get_suffix()[0] in  chu() or suffix.get_suffix()[0] in Xtu():
        return 0
    else:
        return None

    
def lashakvataddhite_103008(suffix):
    """ 
    returns the position of it
    """
    if not isinstance(suffix,Suffix):
        raise ValueError("Invalid Suffix")
    if not suffix.is_taddhit and suffix.get_suffix()[0] in ("l","sh","k","kh","g","gh","Nc"):
        return 0
    else:
        return None



def upadesheajanunaasikait_103002(aadesha):
    
    apply_rule = lambda x: x[0:-1] if x[-1] in anunaasika() or x[-1] in ach()  else x
    prev_iter = aadesha
    print("aadesha="+str(aadesha))
    next_iter = apply_rule(aadesha)

    #for l in aadesha:
    #    if next_iter == prev_iter:
    #        return prev_iter
    #    else:
    #        prev_iter = next_iter
    #        next_iter = apply_rule(next_iter)

    return next_iter