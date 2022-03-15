from ..common_definitions import vriddhi,upadhaa, ach, Suffix, Node ,pratyaahaara, guNna, Dhaatu

class yuvoranaakau_7010010:
    def __init__(self):
        self._types={'anga_node':[],'node':[Suffix,'literal']}
    def __call__(self,anga_node,node):
        if not isinstance(node,Node):
            raise ValueError("suffix must of type Node")
        
        if not isinstance(node._data,Suffix):
            raise ValueError("suffix must of type Suffix")
            
        if not isinstance(anga_node,Node):
            raise ValueError("anga_node must of type Node")
        suffix_string= node.get_output()
        
        if suffix_string[-2:] == ["y","u"]:
            return suffix_string[0:-2] + ["a","n","a"]
        if suffix_string[-2:] == ["v","u"]:
            return suffix_string[0:-2] + ["a","k","a"]
    
        return suffix_string

#raise ValueError("taas from syataasiilRiluXtoH_301033 would be modified with XdityabhasyaapianubandhakaraNnasaamarthyaat_m604146")


class aayaneyiiniiyiyaH_phaXdhakhachchhaghaaM_pratyayaadiinaaM_7010020:
    def __init__(self):
        self._types={'anga_node':[],'node':[Suffix ,'literal']}
        
    def __call__(self,anga_node ,node):        
        if not isinstance(node,Node):
            raise ValueError("suffix must of type Node")
        
        if not isinstance(node._data,Suffix):
            raise ValueError("suffix must of type Suffix")
        
        
        if not isinstance(anga_node,Node):
            raise ValueError("anga_node must of type Node")
        pratyaya=node.get_output()
        letter = [0]
        if letter== "ph":
            return ["aa","y","a","n"] + pratyaya[1:]
        elif letter == "Xdh":
            return ["e","y"]+ pratyaya[1:]
        elif letter == "kh":
            return ["ii","n"]+ pratyaya[1:]
        elif letter == "chh":
            return  ["ii","y"]+ pratyaya[1:]
        elif letter == "gh":
            return ["i","y"]+ pratyaya[1:]
        else:
            return pratyaya
    
class ataupadhaayaaH_7021160:
    def __init__(self):
        self._types={'node':[],'suffix_node':[Suffix,'literal','stateupdate']}
        
    def __call__(self,node,suffix_node):   
            
        if not isinstance(suffix_node,Node):
            raise ValueError("suffix must of type Node")
        
        if not isinstance(suffix_node._data,Suffix):
            raise ValueError("suffix must of type Suffix")
        
        if not isinstance(node,Node):
            raise ValueError("anga_node must of type Node")
            
        suffix_data=[x['output'] for x in suffix_node._output if 'new' in x and x['new']][-1]
        
        it_chars = (suffix_data[0],suffix_data[-1])
        if 'Nc' in it_chars or 'Nn' in it_chars:
            #Ncit or Nnit
            anga_string = node.get_output()
            upadhaa_pos = upadhaa(anga_string )
            return anga_string [0:upadhaa_pos ]+[vriddhi(anga_string[upadhaa_pos])] + anga_string [upadhaa_pos+1:]
            
        else:
            return node.get_output()

class acho_NcNniti_7021150:
    def __init__(self):
        self._types={'node':[],'suffix_node':[Suffix,'stateupdate','literal']}
        
    def __call__(self,node,suffix_node):    
        if not isinstance(node,Node):
            raise ValueError("anga_node must of type Node")
        if not isinstance(suffix_node,Node):
            raise ValueError("suffix must of type Node")
        
        if not isinstance(suffix_node._data,Suffix):
            raise ValueError("suffix must of type Suffix")
        suffix_data=[x['output'] for x in suffix_node._output if 'new' in x and x['new']][-1]
        anga_string= node.get_output()
        if anga_string and suffix_data:
            if anga_string[-1] in ach() and (suffix_data[-1] in ('Nc','Nn') or suffix_data[0] in ('Nc','Nn')):
                return anga_string[0:-1] + [vriddhi(anga_string[-1])]
        return node.get_output() 

class chajoHkughiNnNnyatoH_7030520:
    def __init__(self):
        self._types={'node':[],'suffix_node':['stateupdate',Suffix,'literal']}
        
    def __call__(self,node,suffix_node):
        if not isinstance(node,Node):
            raise ValueError("anga_node must of type Node")
        if not isinstance(suffix_node,Node):
            raise ValueError("suffix must of type Node")
        
        if not isinstance(suffix_node._data,Suffix):
            raise ValueError("suffix must of type Suffix")
        
        suffix_data=[x['output'] for x in suffix_node._output if 'new' in x and x['new']][-1]
        chakaar_to_ku = lambda y : 'k' if y=='ch' else y
        jakaar_to_ku = lambda y : 'g' if y=='j' else y
        if suffix_data[0] in ('gh',) or suffix_data[-1] in ('gh',) or ''.join(suffix_data) == "Nnyat":
            #return ''.join(jakaar_to_ku(chakaar_to_ku(j)) for j in x)
            return [jakaar_to_ku(chakaar_to_ku(j)) for j in node.get_output()]
        
        return node.get_output()

class saarvadhaatukaardhadhaatukayoH_7030840:
    def __init__(self):
        self._types={'node':[Suffix,'literal'],'suffix_node':[Suffix]}
    def __call__(self,node,suffix_node):
    
        if not isinstance(node,Node):
            raise ValueError("anga_node must of type Node")
        if not isinstance(suffix_node,Node):
            raise ValueError("suffix must of type Node")    
        if not isinstance(suffix_node._data,Suffix):
            raise ValueError("suffix must of type Suffix")
        
        if isinstance(node._data,Suffix):
    
            if not node.get_output():
                raise ValueError("Unexpected sarvaahaarii")
            if node._data._suffix[-1] == node.get_output()[-1]:
                anga_string= node.get_output()
                if node._data._suffix[-1] in pratyaahaara('i','k'):
                    return anga_string[0:-1]+[guNna(anga_string[-1])]
        else:
            if node._data._data[-1] == node.get_output()[-1]:
                anga_string= node.get_output()
                if node._data._data[-1] in pratyaahaara('i','k'):
                    return anga_string[0:-1]+[guNna(anga_string[-1])]
        
        return node.get_output()
    
class Functor:
    def __init__(self):
        self._types={'a':['literal']}
    def __call__(self,a):
        return 0
def iXt_not_allowed(suffix_node_data):
    # TODO : Remove hack
    return ''.join(suffix_node_data) in ('ghaNc','Nnvul')

class aardhadhaatukasyeXdvalaadeH_7041140:
    def __init__(self):
        self._types={'dhaatu_node':[Dhaatu],'suffix_node':[Suffix,'literal']}
        self._ruletype = ['insertion']
        
    def __call__(self,dhaatu_node,suffix_node):
        """
        insertion rule
        """
        if not isinstance(dhaatu_node,Node):
            raise ValueError("dhaatu_node must of type Node")
        if not isinstance(suffix_node,Node):
            raise ValueError("suffix_node must of type Node")
            
        
        if isinstance(dhaatu_node._data,Dhaatu) and \
            isinstance(suffix_node._data,Suffix) :
                #dhaatu_node_data=[x['output'] for x in dhaatu_node._output if 'new' in x and x['new']][-1]
                suffix_node_data=[x['output'] for x in suffix_node._output if 'new' in x and x['new']][-1]            
                if not iXt_not_allowed(suffix_node_data) and not suffix_node._data.is_saarvadhaatuka() and suffix_node_data[0] in pratyaahaara('v','l')  and ''.join(suffix_node_data) != 'iXt':
                    return Suffix("iXt")
    
        return []
    
    
class atodiirghoyaNci_7031010:
    def __init__(self):
        self._types={'node':['literal'],'suffix_node':[Suffix,'literal']}
        
    def __call__(self,node,suffix_node):
    
        if not isinstance(node,Node):
            raise ValueError("node must of type Node")
        if not isinstance(suffix_node,Node):
            raise ValueError("suffix_node must of type Node")
            
        if not isinstance(suffix_node._data, Suffix):
            raise ValueError("Must be suffix")
    
        if node.get_output()[-1] == 'a':
            if suffix_node._data.is_saarvadhaatuka() and suffix_node._data._suffix[0] in pratyaahaara('y','Nc') :
                return node.get_output()[0:-1]+['aa']
        return node.get_output()