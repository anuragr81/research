import re, sys
import pandas as pd
from functools import reduce

def ach():
    return ("aa","ii","uu","Rii") + pratyaahaara('a','ch')

def pratyaahaara(start,end):
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
            "Xth","Xt","Xdh","Xd","Xsh","Xn","th","t","dh","d","n",
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

def sup_pratyayaaH():
    return ('su', 'au','jas','am','auT','shas','Taa','bhyaam','bhis','Nge','bhyaam',
        'bhyas','Ngasi','bhyaam','bhyas','Ngas','os','aam','Ngi','os','sup')
    
def saOmyogaantasyalopaH(x):
    
    if not isinstance(x,list):
        raise ValueError("input must be a list")
    if x:
        if len(x)>2:
            if x[-1] in hal() and x[-2] in hal():
                return (True,x[0:(len(x)-1)])
    return (False,x)

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

def get_relevant_suffix(sense):
    sense2suffix = {'bhaava':Suffix("ghaNc"), 'kartaa':Suffix('Nnvul')}
    return sense2suffix[sense]
    

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

def halantyam_103003(upadesha):
    
    antyam = upadesha[len(upadesha)-1]
    if antyam in hal():
        return len(upadesha)-1
    else:
        return None

def chuXtuu_10307(suffix):
    if not isinstance   (suffix,Suffix  ):
        raise ValueError    ("suffix must be of Suffix type")
    if suffix.get_suffix()[0] in  chu() or suffix.get_suffix()[0] in Xtu():
        return 0
    else:
        return None
    
def suffix_it(suffix):
    if not isinstance(suffix,Suffix):
        raise ValueError("Invalid Suffix")
    it_positions = []
    
    it_pos = lashakvataddhite_103008(suffix)
    if it_pos is not None:
        it_positions.append(it_pos)
    
    it_pos = halantyam_103003(suffix.get_suffix())
    if it_pos is not None:
        it_positions.append(it_pos)
        
    it_pos = chuXtuu_10307(suffix)
    if it_pos is not None:
        it_positions.append(it_pos)

    return {'suffix':suffix,'it':tuple(it_positions), 'it_chars':[suffix.get_suffix()[i] for i in it_positions]}
    
   
def vriddhi(x):
    if x =="a":
        return "aa"
    elif x == "i":
        return "ii"
    elif x == "e":
        return "ay"
    else:
        return x
        
def upadhaa(x):    
    if not isinstance(x,list) or not all(isinstance(j,str) for j in x):
        raise ValueError("invalid input: %s" % x)
    if len(x)>=2:
        return len(x)-2
    else:
        raise ValueError("Insufficient length for upadhaa")
    


def chajoHkughiNnNnyatoH_703052(x,suffix):
    chakaar_to_ku = lambda y : 'k' if y=='ch' else y
    jakaar_to_ku = lambda y : 'g' if y=='j' else y
    if suffix.get_suffix()[0] in ('gh',) or suffix.get_suffix()[-1] in ('gh',) or suffix.get_suffix() == "Nyat":
        #return ''.join(jakaar_to_ku(chakaar_to_ku(j)) for j in x)
        return [jakaar_to_ku(chakaar_to_ku(j)) for j in x]
    
    return x
        
def ataupadhaayaaH_702116(anga,it_chars):
    """
    the it characters are used  to decide if upadha is implied in the anga or not
    """
    if 'Nc' in it_chars or 'Nn' in it_chars:
        #Ncit or Nnit
        upadhaa_pos = upadhaa(anga) 
        print("anga="+str(anga)+"upadhaa_pos="+str(upadhaa_pos))
        return (anga[0:upadhaa_pos ]+[{'op':vriddhi, 'input' : anga[upadhaa_pos]}] + anga[upadhaa_pos+1:])
        
    else:
        return anga

def NnonaH_601063(dhaatu):
    if not isinstance   (dhaatu,list):
        raise    ValueError ("dhaatu must be a list of characters  ")
    if dhaatu[0] == "Nn":
        return ['n']+dhaatu[1:]
    else:
        return dhaatu

def sasajuXshoruH_802066(pada):
    if not isinstance(pada,list):
        raise ValueError("input must be a list of characters")
    if pada[-1]=="s":
        return pada[0:-1] + ["r"]

    if ''.join(pada) == "sajuXsh":
        raise ValueError("sajuXsh not supported yet")

def kharavasaanayorvisarjaniiyaH_801015(pada):
    # must be used in avasaana
    if not isinstance(pada,list):
        raise ValueError("Input must be a list of characters")
    khar = pratyaahaara('kh','r')
    if pada[-1]=="r" and (pada[-2] in khar or pada[-2] in ach()):
        return pada[0:-1] + ['H']
    return pada


def yuvoranaakau_701001(suffix_string):
    if not isinstance(suffix_string,list):
        raise ValueError("suffix_string must of type list")
        
    if suffix_string[-2:] == ["y","u"]:
        return suffix_string[0:-2] + ["ana"]
    if suffix_string[-2:] == ["v","u"]:
        return suffix_string[0:-2] + ["aka"]

    return suffix_string



def is_praatipadika_by_suffix(suffix):
    if not isinstance(suffix,Suffix):
        raise ValueError("Must be type:Suffix")
    suffix_str = ''.join(suffix.get_suffix())
    if suffix_str in kRit_pratyayaaH():
        return True
    return False

def form_pada(sup,is_sup,index_x,index_y):
    if is_sup:
        sup_suffix = upadesheajanunaasikait_103002(sup_pratyayaaH()[index_x*3+index_y])
        return sup + [sup_suffix]
    else:
        raise RuntimeError("non-sup not supported")

def declense(input_str,index_x,index_y,sense,is_dhaatu):
    input_data = (parse_string(input_str))
    if is_dhaatu:
        input_data = upadesheajanunaasikait_103002(input_data)
        input_data = NnonaH_601063(input_data)
    
    relevant_suffix=get_relevant_suffix(sense)
    it_results = suffix_it(relevant_suffix)
    it_chars= (it_results['it_chars'])
    print("relevant_suffix="+str(relevant_suffix)+"it_chars="+str(it_chars))
    possible_anga_vriddhi = (ataupadhaayaaH_702116(anga=input_data,it_chars=it_chars))
    apply_vriddhi = lambda k: k['op'](k['input']) if isinstance(k,dict) and 'op' in k else k
    post_vriddhi_anga = [apply_vriddhi (x) for x in possible_anga_vriddhi ]
    
    post_ku_vriddhi_anga=chajoHkughiNnNnyatoH_703052(post_vriddhi_anga,relevant_suffix)

    post_it_suffix = [v for i,v in enumerate(relevant_suffix.get_suffix()) if i not in it_results['it']]
    post_it_suffix = yuvoranaakau_701001(post_it_suffix)
    sup = post_ku_vriddhi_anga+post_it_suffix
    print("post_ku_vriddhi_anga="+str(post_ku_vriddhi_anga)+" post_it_suffix="+str(post_it_suffix))
    if is_praatipadika_by_suffix(relevant_suffix):
        pada = form_pada(sup=sup,is_sup=True,index_x=0,index_y=0)
        final_pada=(kharavasaanayorvisarjaniiyaH_801015(sasajuXshoruH_802066(pada)))

        return(final_pada)
    else:
        raise ValueError("Not a praatipadika")
    

if __name__ =="__main__":
    #input_str= "bhaj"

    #assert(''.join(declense("bhaja",0,0,sense="bhaava",is_dhaatu=True))=="bhaagaH")
    print(''.join(declense("NniiNc",0,0,sense="kartaa",is_dhaatu=True)))

    sys.exit(0)

    #a =pd.read_csv('dhaatupaatha.csv')
    #fh=open('dhaatu_list.txt',encoding="utf-8") 
    dhaatulist=[]
    with open('dhaatu_list.txt',encoding="utf-8") as fh:
        x=fh.read()
        l=x.split("\n")
        for i in l:
            dhaatulist.append(bytes(i,'unicode_escape'))
    dhaatulist_unicode =[]
    for dhaatu in dhaatulist:
        bsz=6
        nletters_dhatu = int(len(dhaatu)/bsz)
        dhaatu_unicode=[]
        for i in range(nletters_dhatu):
            dhaatu_unicode.append(''.join([chr(k) for k in dhaatu[bsz*i:bsz*(i+1)]]))
        dhaatulist_unicode.append(dhaatu_unicode)
    
    with open('devnag_map.txt',encoding="utf-8") as fh:
        dm = fh.read()