from ..common_definitions import vriddhi,upadhaa, ach


def yuvoranaakau_701001(suffix_string):
    if not isinstance(suffix_string,list):
        raise ValueError("suffix_string must of type list")
        
    if suffix_string[-2:] == ["y","u"]:
        return suffix_string[0:-2] + ["a","n","a"]
    if suffix_string[-2:] == ["v","u"]:
        return suffix_string[0:-2] + ["a","k","a"]

    return suffix_string



def aayaneyiiniiyiyaH_phaXdhakhachchhaghaaM_pratyayaadiinaaM_701002(pratyaya):
    letter = pratyaya[0]
    if letter== "ph":
        return "aayan"
    elif letter == "Xdh":
        return "ey"
    elif letter == "kh":
        return "iin"
    elif letter == "chh":
        return "iiy"
    elif letter == "gh":
        return "iy"
    

        
def ataupadhaayaaH_702116(anga,it_chars,suffix):
    """
    the it characters are used  to decide if upadha is implied in the anga or not
    """
    if 'Nc' in it_chars or 'Nn' in it_chars:
        #Ncit or Nnit
        upadhaa_pos = upadhaa(anga)
        return (anga[0:upadhaa_pos ]+[{'op':vriddhi, 'input' : anga[upadhaa_pos]}] + anga[upadhaa_pos+1:])
        
    else:
        return anga

def acho_NcNniti_702115(anga,suffix):
    if not isinstance(suffix,list):
        raise ValueError("suffix must be a list")
    if not isinstance(anga,list):
        raise ValueError("anga must be a list")

    if anga[-1] in ach() and (suffix[-1] in ('Nc','Nn') or suffix[0] in ('Nc','Nn')):
        return anga[0:-1] + [vriddhi(anga[-1])]
    return anga


def chajoHkughiNnNnyatoH_703052(x,suffix):
    chakaar_to_ku = lambda y : 'k' if y=='ch' else y
    jakaar_to_ku = lambda y : 'g' if y=='j' else y
    if suffix.get_suffix()[0] in ('gh',) or suffix.get_suffix()[-1] in ('gh',) or suffix.get_suffix() == "Nyat":
        #return ''.join(jakaar_to_ku(chakaar_to_ku(j)) for j in x)
        return [jakaar_to_ku(chakaar_to_ku(j)) for j in x]
    
    return x

