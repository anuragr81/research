from ..common_definitions import pratyaahaara, ach

def sasajuXshoruH_802066(pada):
    if not isinstance(pada,list):
        raise ValueError("input must be a list of characters")
    if pada[-1]=="s":
        return pada[0:-1] + ["r"]

    if ''.join(pada) == "sajuXsh":
        raise ValueError("sajuXsh not supported yet")
    return pada


def kharavasaanayorvisarjaniiyaH_801015(pada):
    # must be used in avasaana
    if not isinstance(pada,list):
        raise ValueError("Input must be a list of characters")
    khar = pratyaahaara('kh','r')
    if pada[-1]=="r" and (pada[-2] in khar or pada[-2] in ach()):
        return pada[0:-1] + ['H']
    return pada
