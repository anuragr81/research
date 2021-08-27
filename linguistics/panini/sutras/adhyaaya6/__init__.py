from ..common_definitions import Suffix

def NnonaH_601063(dhaatu):
    if not isinstance   (dhaatu,list):
        raise    ValueError ("dhaatu must be a list of characters  ")
    if dhaatu[0] == "Nn":
        return ['n']+dhaatu[1:]
    else:
        return dhaatu

def echoayavaayaavaH_601075(x):
    if x == "e":
        return "ay"
    if x == "o":
        return "av"
    if x == "ai":
        return "aay"
    if x == "au":
        return "aav"
        
    return x


def yasyeticha_604148(anga_str,suffix):
    if not isinstance(suffix,Suffix):
        raise ValueError("suffix must be of Suffix type")
    if suffix.is_taddhita or suffix.get_suffix()[0] in ('i','ii'):
        return anga_str[0:-1], suffix.get_suffix()
    return anga_str, suffix.get_suffix()