import re, sys
import pandas as pd
from functools import reduce
from sutras.common_definitions import *

from sutras.adhyaaya1 import *
from sutras.adhyaaya2 import *
from sutras.adhyaaya4 import *
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


def get_dhaatu_lakaara(dhaatu, lakaara):
    if lakaara=="luNg":
        #via chleHsich
        return Suffix("sich")
    raise ValueError("Unknown lakaara")
def get_relevant_suffix(sense,dhaatu=None):
    sense2suffix = {'bhaava':Suffix("ghaNc"), 'kartaa':Suffix('Nnvul'),
                    'bhava':Suffix("aNn"),
                    'luNg':get_dhaatu_lakaara(dhaatu=sense,lakaara='luNg')}
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

def apply_centered_rule(state,pos):
    """ Use all of state to reduce based on rules centered on pos """
    print("Rule applied on pos=%d" % pos)
    if len(state) == 1:
        return state
    elif len(state)== 2:
        # combine the current and next whenever possible
        return [state[pos]]
    else:
        # return the remainder apart from the reduced value form (pos,pos+1) 
        # whenever possible
        return state[0:pos] + [state[pos]] + state[(pos+2):]
    
def reduce_state_for_each(state):
    
    if not isinstance(state,list):
        raise ValueError("state must be a list")
    if state:
        if len(state) == 1:
            return state
        else:            
            i = 0
            while i < len(state):
                next_state = apply_centered_rule(state,i)
                if next_state == state:
                    # move to the next position
                    i = i + 1
                else:
                    i = 0
                    state = next_state
                    
    return state
    

def add_agamas(state):
    if not isinstance(state,list):
        raise ValueError("state must be a list")
    # all agamas must be listed here
    to_add=[]
    for i, st in enumerate(state):
        if isinstance(st,Group):
            if vRiddhaachchhaH_402113(st):
                # chha is added to the end
                to_add.append((len(state), Suffix("chha")))
            
    # not add agamas to all states       
    while to_add:
       entry = to_add[0]
       state.insert(entry [0],entry [1])
       # re-adjust to_add to reflect added state
       to_add = [(pos+1,dat) for pos,dat in to_add[1:]]
    return state
    
    
   

def add_suffix(anga,suffix):
    if not isinstance(suffix,Suffix):
        raise ValueError("suffix must be of type Suffix")
        
   
    if not isinstance(anga,Anga):
        raise ValueError("first element of state must of type Anga")
    it_results = suffix_it(suffix)
    it_chars= (it_results['it_chars'])
    
    possible_anga_vriddhi = (ataupadhaayaaH_702116(anga=anga.get_anga(),it_chars=it_chars, suffix=suffix))
    apply_vriddhi = lambda k: k['op'](k['input']) if isinstance(k,dict) and 'op' in k else k
    post_vriddhi_anga = [apply_vriddhi (x) for x in possible_anga_vriddhi ]
    #print("suffix="+str(suffix)+"it_chars="+str(it_chars))
    post_ku_vriddhi_anga1 = chajoHkughiNnNnyatoH_703052(post_vriddhi_anga,suffix)
    post_ku_vriddhi_anga2 = acho_NcNniti_702115(post_ku_vriddhi_anga1,suffix.get_suffix())
    if anga.is_dhaatu():
        forced_aardhadhaatuka = True
        # 0703096- astisicho apRikte - would enable iT otherwise 
        # prevented with 0702010 - ekaacha upadeshe anudaattaat
        post_ku_vriddhi_anga2 = aardhadhaatukasyeXdvalaadeH_704114(post_ku_vriddhi_anga2,forced_aardhadhaatuka,suffix.get_suffix())
    #non_it_letters = [x for x in suffix if x not in it_chars ]
    #TODO: ask why chuXtuu does not apply for chh -> 701002
    if ''.join(suffix.get_suffix())=="chha":
        post_it_suffix  = suffix.get_suffix()
    else:     
        post_it_suffix = [v for i,v in enumerate(suffix.get_suffix()) if i not in it_results['it']]
        post_it_suffix = yuvoranaakau_701001(post_it_suffix)
    print ("Using substitution")
    
    post_it_suffix = aayaneyiiniiyiyaH_phaXdhakhachchhaghaaM_pratyayaadiinaaM_701002(post_it_suffix)
    
    post_ku_vriddhi_anga3, post_it_suffix3 = use_saMhitaa(post_ku_vriddhi_anga2,post_it_suffix)
    #TODO: use aagama aadesha again(?)
    if yachibham_104018(post_it_suffix3):
        post_ku_vriddhi_anga4, post_it_suffix4 = yasyeticha_604148(post_ku_vriddhi_anga3, Suffix(post_it_suffix3))
    else :
        post_ku_vriddhi_anga4, post_it_suffix4 = post_ku_vriddhi_anga3, post_it_suffix3
    sup = uraNnraparaH_101050(post_ku_vriddhi_anga4,post_it_suffix4)
    print("post_ku_vriddhi_anga4="+str(post_ku_vriddhi_anga4)+" post_it_suffix4="+str(post_it_suffix4) + " sup=" + str(sup))
    return sup



def declense(state,index_x,index_y,sense,is_dhaatu):
    
    
    
    if isinstance(state,Anga):
        input_data = state.get_anga()
        if is_dhaatu:
            relevant_suffix=get_relevant_suffix(sense,dhaatu=input_data)
            input_data = upadesheajanunaasikait_103002(input_data)
            input_data = NnonaH_601063(input_data)
            it_pos_list = aadirNciXtuXdavaH_103005(input_data)
            input_data = [x for i,x in enumerate(input_data) if i not in it_pos_list]   
        else:
            relevant_suffix=get_relevant_suffix(sense)
        
        sup = add_suffix(Anga(input_data,is_dhaatu),relevant_suffix)
        
        if is_praatipadika_by_suffix(relevant_suffix):
            pada = form_pada(sup=sup,is_sup=True,index_x=index_x,index_y=index_y)
            final_pada= kharavasaanayorvisarjaniiyaH_801015(sasajuXshoruH_802066(pada))
            return final_pada
        else:
            raise ValueError("Not a praatipadika")    
             
    else:
        relevant_suffix=get_relevant_suffix(sense)
        state_after_agama = add_agamas(state)
        if isinstance(state_after_agama[-1],Suffix):
            if state_after_agama[-1].is_taddhita:
                # treat as praatipaadika
                state_after_agama= supodhaatupraatipadikayoH_204071(state_after_agama)
        if len(state_after_agama) == 2:
            #TODO: Can we be sure that more than state-expansion through
            # aagama aadesha would not be implied from within adding/appending
            # of suffixes
            
            #TODO:  force Anga for now - but find out why did 1.4.13 - yasmaatpratyayavidhi... 
            # not apply much i.e. in shaala Ngi itself (before we do the lopa)
            output = add_suffix(Anga(state_after_agama[0].data()),state_after_agama[1])
            return output
    
    
    

if __name__ =="__main__":
    #input_str= "bhaj"
    print("Result="+str(declense(Anga(parse_string("NniiNc")),0,0,sense="kartaa",is_dhaatu=True)))
    #assert(''.join(declense(Anga(parse_string("bhaja")),0,0,sense="bhaava",is_dhaatu=True))=="bhaagaH")
    if False:
        assert(''.join(declense(Anga(parse_string("bhaja")),0,0,sense="bhaava",is_dhaatu=True))=="bhaagaH")
        assert(''.join(declense(Anga(parse_string("NniiNc")),0,0,sense="kartaa",is_dhaatu=True))=="naayakaH")        
        assert(''.join(declense(Anga(parse_string("XdukRiNc")),0,0,sense="kartaa",is_dhaatu=True))=="kaarakaH") 
        assert ''.join(declense([Group(parse_string("shaalaa")),Suffix(sup_pratyayaaH()[6*3+0])],0,0,sense="bhava",is_dhaatu=False)) == "shaaliiya"
    #TODO:
    #print(''.join(declense(parse_string("chiNc"),0,0,sense="kartaa",is_dhaatu=True))) # chiNc -> chaayaka? (XshXtuNc se dhaatvaadeH XshaH saH)
    
    #print(declense(Anga(parse_string("chiNc")),0,0,sense="luNg",is_dhaatu=True))
    
    
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