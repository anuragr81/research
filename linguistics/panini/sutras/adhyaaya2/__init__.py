from ..common_definitions import Suffix, sup_pratyayaaH

def supodhaatupraatipadikayoH_204071(state):
    suffixes = [ {'pos':i,'data':st} for i,st in enumerate(state) if isinstance(st,Suffix)]
    if len(suffixes)>1:
        # get rid of the second last suffix if it's a sup
        suffix_string = suffixes[-2]['data']  if isinstance(suffixes[-2]['data'],str) else ''.join(suffixes[-2]['data'].get_suffix())
        if suffix_string in sup_pratyayaaH():
            return state[0:suffixes[-2]['pos']] + state[(suffixes[-2]['pos']+1) :]
    return state
    