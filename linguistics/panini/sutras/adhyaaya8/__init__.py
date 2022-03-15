from ..common_definitions import pratyaahaara, ach, Node

class sasajuXshoruH_8020660:
    def __init__(self):
        self._types={'node':['literal']}
        
    def __call__(self,node):
        
        if not isinstance(node,Node):
            raise ValueError("node must of type Node")
    
        pada = node.get_output()
        return pada
        if pada[-1]=="s":
            return pada[0:-1] + ["r"]
    
        if pada == "sajuXsh":
            raise ValueError("sajuXsh not supported yet")
        return pada

class kharavasaanayorvisarjaniiyaH_8010150:
    def __init__(self):
        self._types={'node':['literal']}
        
    def __call__(self,node):
        # must be used in avasaana
        if not isinstance(node,Node):
            raise ValueError("node must of type Node")
        pada = node.get_output()
        return pada
        khar = pratyaahaara('kh','r')
        if pada[-1]=="r" and (pada[-2] in khar or pada[-2] in ach()):
            return pada[0:-1] + ['H']
        return pada
