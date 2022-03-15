import re


if False:    
    # Usage : Add @sutra_precondition(a,b,c) before the function
    def sutra_precondition(**kwargs):
        def wrap(func):
            def wrapped_sutra(*args):
                print("wrapped_f: kwargs="+str(kwargs))
                if 't' in kwargs and kwargs['t']=="literal":
                    print("Using literal invocatoin")
                func(*args)
            return wrapped_sutra
        return wrap

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

class Group:
    def __init__(self,data):
        self._data = data
        
    def has_vRiddhi(self):
        achs = [ j for j in self._data if j in ach() ]
        if achs and achs[0] in ('aa','ai','au',):
            return True
        return False
    
    def data(self):
        return self._data



class Anga(Group):
    def __init__(self,anga,is_dhaatu=False):
        self._anga = Group(anga)
        self._is_dhaatu=is_dhaatu
        
    def is_dhaatu(self):
        return self._is_dhaatu
    
    def get_anga(self):
        return self._anga.data()
        
    def __str__(self):
        return str(self._anga )

    def __repr__(self):
        return str(self._anga )

def insertion_rule(x):
    return x

class Suffix:
    def __init__(self,suffix,lakaara=None):
        
        if lakaara is not None:
            if lakaara not in ('laXt','loXt','lRiXt','laNg','luNg','lRiNg','liNg1','liNg2','liXt','luXt'):
                raise ValueError("Unknown lakaara")            
        self._lakaara=lakaara

        
        if isinstance(suffix,str):
            self._suffix = parse_string(suffix)
        elif isinstance(suffix,list) and all(isinstance(j,str) for j in suffix):
            self._suffix= suffix
        else:
            raise ValueError("suffix must be a string")
        
        all_pratyayaaH = kRit_pratyayaaH()+tiNg_pratyayaaH()+san_pratyayaaH()+strii_pratyayaaH()+sup_pratyayaaH()+taddhita_prayayaaH() + unclassified_pratyayaaH()
        if ''.join(self._suffix) not in all_pratyayaaH:
            raise ValueError("Unknown suffix %s" % ''.join(self._suffix))            
        
        self.is_taddhita = ''.join(self._suffix) in taddhita_prayayaaH()
        
    def get_data(self):
        return self._suffix
        
    
    def apply_reduction(self,functor, **kwargs):
        self.reduced = functor(**kwargs)
        
    def is_saarvadhaatuka(self):
        if ''.join(self._suffix) in tiNg_pratyayaaH() or self._suffix[0]=='sh':
            return True
        else:
            return False

    def __str__(self):
        return ''.join(self._suffix)

    def __repr__(self):
        return str(self._suffix)


class It:
    def __init__(self,data):
        self._data= data

class Dhaatu:
    def __init__(self,data):
        self._data= data
        
    def get_data(self):
        return self._data
    



class Node:
    def __init__(self,data,parent):
        if all ( not isinstance(data,x) for x in list(get_supported_types()) + [list] ):
            raise ValueError("Unsupported type %s" % type(data))
        if parent is not None:
            if isinstance(parent,list):
                # temporary type (list etc.) so go to the first typed parent
                self.parent = find_known_parent(parent)
            elif all ( not isinstance(parent,x) for x in get_supported_types() ):
                raise ValueError("Unsupported parent type %s" % type(parent))
            else:
                self.parent = parent
        else:
            self.parent = None
        
        self._data =data
        self._output = [{'output':self._data.get_data(),'new':True}]
        
    def set_output(self,rule,**kwargs):
        old_output = self.get_output()

        new_output = rule()(node=self,**kwargs)
        
        if isinstance(new_output,dict):
            if 'mutate' in new_output :
                if new_output['output'] != old_output :
                    if new_output ['mutate']:
                        self._output.append({'rule':rule,'inputs':{**{'state':old_output} , **kwargs},'output':new_output['output'] ,'new' :True})
                    else:
                        self._output.append({'rule':rule,'inputs':{**{'state':old_output} , **kwargs},'output':new_output['output']})
                
            
        else:
           if new_output != old_output:
              self._output.append({'rule':rule,'inputs':{**{'state':old_output} , **kwargs},'output':new_output })
        
    def get_output(self):
        return self._output[-1]['output']
    

def is_known_type(nd):
    return any ( isinstance(nd,x) for x in get_supported_types())


def get_supported_types ():
    return (Suffix,It,Dhaatu)

def find_known_parent(nd):
    # return parent of Node that is not of a known type
    if nd is None:
        return None
    
    if is_known_type(nd):
        return nd
    else:
        return find_known_parent(nd.parent)




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
    
    plist= ({'letters':("a","i",'ii',"u",'uu',),'marker':'Nn'},{'letters':('Ri','Rii','lRi'),'marker':'k'},
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
                output = list(results[0]['letters'][results[0]['letters'].index(start):])
                for res_entry in results[1:]:
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



def unclassified_pratyayaaH():
    return ('sich','chli','shap','taas','sya')
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


def taddhita_prayayaaH ():
    return ('chha','iiy')

def upadhaa(x):    
    if not isinstance(x,list) or not all(isinstance(j,str) for j in x):
        raise ValueError("invalid input: %s" % x)
    if len(x)>=2:
        return len(x)-2
    else:
        raise ValueError("Insufficient length for upadhaa")


def diirgha(x):
    if x =="a":
        return "aa"
    elif x == "i":
        return "ii"
    elif x == "u":
        return "uu"
    else:
        return x
    
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
    

def guNna(x):
    if x =="i" or x=="i":
        return "e"
    elif x == "u" or x=="uu":
        return "o"
    else:
        return x
        

