from ..common_definitions import Suffix

def vRiddhaachchhaH_4021130(x):
    if x.has_vRiddhi():
        return [x,Suffix('chha')]
    return [x]
    
def tatrabhavaH_4030530(suffix,sense):
    if sense == "bhava":
        return Suffix("aNn")
    else:
        return suffix
        
    