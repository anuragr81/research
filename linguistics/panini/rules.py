import re

def ach():
    return ("aa","ai","au","a","ii","i","uu","u","lRi","Ri","e","o")

def hal():
    return ("kh","k","gh","g","Ng","Nc","Nn","chh","ch","jh","j",
            "Xth","Xt","Xdh","Xd","Xsh","Xn","th","t","dh","d","n",
            "ph","p","bh","b","m","y","r","l","v","sh",
            "s","h")
    
def saOmyogaantasyalopaH(x):
    
    if not isinstance(x,list):
        raise ValueError("input must be a list")
    if x:
        if len(x)>2:
            if x[-1] in hal() and x[-2] in hal():
                return (True,x[0:(len(x)-1)])
    return (False,x)

def san_pratyayaaH():
    return ("san","kyach","kaamyach","kyaNg","kyaXsh",
            "kvip","Xnich","yaNg","yak","aaya",
            "iiyaNg","XniNg")

def tiNg_pratyayaaH():
    return ("tip","tas","jhi","sip","thas","tha","mip",
            "vas","mas","ta","aataam","jha","thaas",
            "aathaam","dhvam","iXt","vahi","mahiNg")

def kRit_pratyayaaH():
    return ("Nvul","lyuXt","aniiyar","kta","ktavatu",
            "tavyat","tumun","tRich","ktvaa","Xnmul",
            "lyap","yat","Xnyat","kyap","ghaNc","ach",
            "ap","ktin","a","yuch","u","shatRi","shaanach",
            "aXn","ka","Xnini","kvip")

def upasargaaH():
    return ("abhi","prati","pari","upa","pra","apa",
            "sam","anu","ava","nis","nir","dus",
            "dur","vi","aaNg","ni","adhi","api",
            "ati","su","ut")

def strii_pratyayaaH():
    return ("Xtaap","Xdaap","chaap","Ngiip","NgiiXsh",
            "Xniin","uuNg","ti","XshyaNg","Xshpha")

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
    
def parse_string(input_str):
    """
    build a list of aksharas from the string - unknown letters are ignored
    """
    match_re = "("+'|'.join(hal() + ach())+")" + "(.*)"
    matches = True
    x_str = input_str
    output=[]
    while matches and x_str:
        res = re.search(match_re, x_str)
        if res:
            output.append(res.group(1))
            x_str = res.group(2)
        else:
            matches =False
    return output
    
class Prefix:
    def __init__(self,prefix):
        self.prefix = prefix


class Suffix:
    def __init__(self,suffix):
        if isinstance(suffix,str):
            self._suffix = parse_string(suffix)
        elif isinstance(suffix,list) and all(isinstance(j,str) for j in suffix):
            self._suffix= suffix
        else:
            raise ValueError("suffix must be a string")
        taddhits = []
        self.is_taddhit = suffix in taddhits
        
    def get_suffix(self):
        return self._suffix
    
    def __str__(self):
        return str(self._suffix)

    def __repr__(self):
        return str(self._suffix)
    
def apply_it(self,pos,suffix_str):
    return [val for index,val in enumerate(self._suffix) if index!=pos]

def get_suffix():
    return Suffix("ghaNc")

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

def halantyam_103003(suffix):
    if not isinstance(suffix,Suffix):
        raise ValueError("Invalid Suffix")    
    antyam = suffix.get_suffix()[len(suffix.get_suffix())-1]
    if antyam in hal():
        return len(suffix.get_suffix())-1
    else:
        return None
 
    
def suffix_it(suffix):
    if not isinstance(suffix,Suffix):
        raise ValueError("Invalid Suffix")
    it_position1 = lashakvataddhite_103008(suffix)
    it_position2 = halantyam_103003(suffix)
    return {'suffix':suffix,'it':(it_position1 ,it_position2)}
    
    return root
if __name__ =="__main__":
    input_str= "bhaj"
    input_data = (parse_string(input_str))
    
    #parse_string("bhaj"), 
    print(suffix_it(get_suffix()))
    