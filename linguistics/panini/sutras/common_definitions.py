import re

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

def ach():
    return ("aa","ii","uu","Rii") + pratyaahaara('a','ch')

def pratyaahaara(start,end):
    """   
    Parameters
    ----------
    start : character
        The starting letter of the  pratyaahaara
    end : character
        The ending letter of the  pratyaahaara

    Raises
    ------
    ValueError
    RuntimeError

    Returns
    -------
    list
        characters in the pratyaahaara.

    """
    
    plist= ({'letters':("a","i","u",),'marker':'Nn'},{'letters':('Ri','lRi'),'marker':'k'},
        {'letters':('e','o',),'marker':'Ng'},{'letters':('ai','au'),'marker':'ch'},{'letters':('h','y','v','r',),'marker':'Xt'},{'letters':('l',),'marker':'N'},
        {'letters':('Nc','m','Ng','Nn','n'),'marker':'m'},{'letters':('jh','bh'),'marker':'Nc'},{'letters':('gh','Xdh','dh'),'marker':'Xsh'},
        {'letters':('j','b','g','Xd','d',),'marker':'sh'},{'letters':('kh','ph','chh','Xth','th','ch','Xt','t',),'marker':'v'},
        {'letters':('k','p',),'marker':'y'},{'letters':('sh','Xsh','s',),'marker':'r'},{'letters':('h',),'marker':'l'},)
    markers = [p['marker'] for p in plist]
    if end not in markers:
        raise ValueError("Unkown marker: %s" % end)
    entries = [(i,j) for i,j in enumerate(plist) if end==j['marker']]
    if len(entries)>1:
        raise RuntimeError("Multiple entries not supported")
    else:
        not_found = True
        for i,entry in enumerate(plist):
            if start in entry['letters']:
                not_found = False
                results = plist[i:(entries[0][0]+1)]
                output = []
                for res_entry in results:
                    output = output+list(res_entry['letters'])
                return tuple(output)
                
        if not_found:
            raise ValueError("Unknown starting letter: %s" % start)
        

def hal():
    return ("kh","k","gh","g","Ng","Nc","Nn","chh","ch","jh","j",
            "Xth","Xt","Xdh","Xd","Xsh","th","t","dh","d","n",
            "ph","p","bh","b","m","y","r","l","v","sh",
            "s","h")
def anunaasika():
    return ("Ng","Nc","Nn","M","m")

def chu():
    return ("ch","chh","j","jh","Nc")

def Xtu():
    return ("Xt","Xth","Xd","Xdh","Nn")


def san_pratyayaaH():
    return ("san","kyach","kaamyach","kyaNg","kyaXsh",
            "kvip","Nnich","yaNg","yak","aaya",
            "iiyaNg","NniNg")

def tiNg_pratyayaaH():
    return ("tip","tas","jhi","sip","thas","tha","mip",
            "vas","mas","ta","aataam","jha","thaas",
            "aathaam","dhvam","iXt","vahi","mahiNg")

def kRit_pratyayaaH():
    return ("Nnvul","lyuXt","aniiyar","kta","ktavatu",
            "tavyat","tumun","tRich","ktvaa","Nnmul",
            "lyap","yat","Nnyat","kyap","ghaNc","ach",
            "ap","ktin","a","yuch","u","shatRi","shaanach",
            "aNn","ka","Nnini","kvip")

def upasargaaH():
    return ("abhi","prati","pari","upa","pra","apa",
            "sam","anu","ava","nis","nir","dus",
            "dur","vi","aaNg","ni","adhi","api",
            "ati","su","ut")

def strii_pratyayaaH():
    return ("Xtaap","Xdaap","chaap","Ngiip","NgiiXsh",
            "Nniin","uuNg","ti","XshyaNg","Xshpha")

def sup_pratyayaaH():
    return ('su', 'au','jas','am','auT','shas','Taa','bhyaam','bhis','Nge','bhyaam',
        'bhyas','Ngasi','bhyaam','bhyas','Ngas','os','aam','Ngi','os','sup')
        
def upadhaa(x):    
    if not isinstance(x,list) or not all(isinstance(j,str) for j in x):
        raise ValueError("invalid input: %s" % x)
    if len(x)>=2:
        return len(x)-2
    else:
        raise ValueError("Insufficient length for upadhaa")


def vriddhi(x):
    if x =="a":
        return "aa"
    elif x == "i":
        return "ii"
    elif x == "e":
        return "ay"
    elif x == 'ii':
        return 'ai'
    else:
        return x
    


