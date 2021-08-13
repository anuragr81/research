import re, sys
import pandas as pd
from functools import reduce
from sutras.common_definitions import *

from sutras.adhyaaya1 import *
from sutras.adhyaaya6 import *
from sutras.adhyaaya7 import *
from sutras.adhyaaya8 import *

    
def saOmyogaantasyalopaH(x):   
    if not isinstance(x,list):
        raise ValueError("input must be a list")
    if x:
        if len(x)>2:
            if x[-1] in hal() and x[-2] in hal():
                return (True,x[0:(len(x)-1)])
    return (False,x)


    
class Prefix:
    def __init__(self,prefix):
        self.prefix = prefix


    
def apply_it(self,pos,suffix_str):
    return [val for index,val in enumerate(self._suffix) if index!=pos]

def get_relevant_suffix(sense):
    sense2suffix = {'bhaava':Suffix("ghaNc"), 'kartaa':Suffix('Nnvul'),
                    'bhava':Suffix('aNn')}
    return sense2suffix[sense]
    
    
def suffix_it(suffix):
    if not isinstance(suffix,Suffix):
        raise ValueError("Invalid Suffix")
    it_positions = []
    
    it_pos = lashakvataddhite_103008(suffix)
    if it_pos is not None:
        it_positions.append(it_pos)

    it_pos_list = aadirNciXtuXdavaH_103005(suffix.get_suffix())
    if it_pos is not None:
        it_positions = it_positions + it_pos_list
    
    it_pos = halantyam_103003(suffix.get_suffix())
    if it_pos is not None:
        it_positions.append(it_pos)
        
    it_pos = chuXtuu_10307(suffix)
    if it_pos is not None:
        it_positions.append(it_pos)

    return {'suffix':suffix,'it':tuple(it_positions), 'it_chars':[suffix.get_suffix()[i] for i in it_positions]}
    

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

def use_saMhitaa(anga,suffix):
    if not isinstance(anga,list):
        raise ValueError("Cannot have non-list anga")
    if not isinstance(suffix,list):
        raise ValueError("Cannot have non-list suffix")
    if anga[-1] in ach() and suffix[0] in ach(): # self-definition of ach (not using paraHsannikarXshaHsaMhitaa_104108)
        return anga[0:-1]+[echoayavaayaavaH_601075(anga[-1])], suffix

    return anga,suffix


def declense(input_str,index_x,index_y,sense,is_dhaatu):

    input_data = (parse_string(input_str))
    if is_dhaatu:
        input_data = upadesheajanunaasikait_103002(input_data)
        input_data = NnonaH_601063(input_data)
        it_pos_list = aadirNciXtuXdavaH_103005(input_data)
        input_data = [x for i,x in enumerate(input_data) if i not in it_pos_list]
    
    #TODO: vriddha saMjNcaa
    relevant_suffix=get_relevant_suffix(sense)
    it_results = suffix_it(relevant_suffix)
    it_chars= (it_results['it_chars'])
    print("relevant_suffix="+str(relevant_suffix)+"it_chars="+str(it_chars))
    possible_anga_vriddhi = (ataupadhaayaaH_702116(anga=input_data,it_chars=it_chars, suffix=relevant_suffix))
    apply_vriddhi = lambda k: k['op'](k['input']) if isinstance(k,dict) and 'op' in k else k
    post_vriddhi_anga = [apply_vriddhi (x) for x in possible_anga_vriddhi ]

    
    post_ku_vriddhi_anga1=chajoHkughiNnNnyatoH_703052(post_vriddhi_anga,relevant_suffix)
    post_ku_vriddhi_anga2 = acho_NcNniti_702115(post_ku_vriddhi_anga1,relevant_suffix.get_suffix())
    #non_it_letters = [x for x in suffix if x not in it_chars ]
    post_it_suffix = [v for i,v in enumerate(relevant_suffix.get_suffix()) if i not in it_results['it']]
    post_it_suffix = yuvoranaakau_701001(post_it_suffix)

    post_ku_vriddhi_anga3, post_it_suffix3 = use_saMhitaa(post_ku_vriddhi_anga2,post_it_suffix)
    sup = uraNnraparaH_101050(post_ku_vriddhi_anga3,post_it_suffix3)
    
    
    print("post_ku_vriddhi_anga3="+str(post_ku_vriddhi_anga3)+" post_it_suffix3="+str(post_it_suffix3))
    if is_praatipadika_by_suffix(relevant_suffix):
        pada = form_pada(sup=sup,is_sup=True,index_x=index_x,index_y=index_y)
        final_pada=(kharavasaanayorvisarjaniiyaH_801015(sasajuXshoruH_802066(pada)))

        return(final_pada)
    else:
        raise ValueError("Not a praatipadika")
    

if __name__ =="__main__":
    #input_str= "bhaj"
    if False:
        assert(''.join(declense("bhaja",0,0,sense="bhaava",is_dhaatu=True))=="bhaagaH")
        assert(''.join(declense("NniiNc",0,0,sense="kartaa",is_dhaatu=True))=="naayakaH")
        #print(''.join(declense("chiNc",0,0,sense="kartaa",is_dhaatu=True))) # chiNc -> chaayaka? (XshXtuNc se dhaatvaadeH XshaH saH)
        assert(''.join(declense("XdukRiNc",0,0,sense="kartaa",is_dhaatu=True))=="kaarakaH") 


    print(''.join(declense("shaalaa",6,0,sense="bhava",is_dhaatu=False))) # 4.2.91 - sheXshe (explain)
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