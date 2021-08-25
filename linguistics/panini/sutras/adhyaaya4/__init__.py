from ..common_definitions import Suffix

def vRiddhaachchhaH_402113(x):
    if x.has_vRiddhi():
        return [x,Suffix('chha')]
    return [x]
    
def tatrabhavaH_403053(suffix,sense):
    if sense == "bhava":
        return Suffix("aNn")
    else:
        return suffix
        
    