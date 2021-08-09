
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