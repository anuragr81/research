from ..common_definitions import Suffix

class vRiddhaachchhaH_4021130:
    def __init__(self):
        self._types={'x':['literal']}
    def __call__(self,x):
        if x.has_vRiddhi():
            return [x,Suffix('chha')]
        return [x]

class tatrabhavaH_4030530:
    def __init__(self):
        self._types={'suffix':[Suffix,'sense']}
    def __call__(self,suffix):
        if suffix.sense == "bhava":
            return Suffix("aNn")
        else:
            return suffix
        
    