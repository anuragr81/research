from ..common_definitions import pratyaahaara, ach, Node

def sasajuXshoruH_802066(node):
    if not isinstance(node,Node):
        raise ValueError("node must of type Node")

    pada = node.get_output()
    if pada[-1]=="s":
        return pada[0:-1] + ["r"]

    if pada == "sajuXsh":
        raise ValueError("sajuXsh not supported yet")
    return pada


def kharavasaanayorvisarjaniiyaH_801015(node):
    # must be used in avasaana
    if not isinstance(node,Node):
        raise ValueError("node must of type Node")
    pada = node.get_output()
    khar = pratyaahaara('kh','r')
    if pada[-1]=="r" and (pada[-2] in khar or pada[-2] in ach()):
        return pada[0:-1] + ['H']
    return pada
