from lxml import etree
from functools import reduce
import jsonpath_ng
import copy

def compare_dict(x,y):
    e1 = etree.fromstring(x)
    return e1


def get_end_nodes(root: dict, path =None):
    if isinstance(root,dict):
        path = "" if path is None else path + "."
        return reduce(lambda x,y : x + y , [get_end_nodes(path=path+k,root=root[k]) for k in root],[])
    elif isinstance(root,(list,set)):
        path = '' if path is None else path
        return reduce(lambda x,y : x + y, [get_end_nodes(path=path+ "[" + str(i) + "]",root=k) for i,k in enumerate(root)],[])
    else:
        return [path]
    
    
def vector_add(x,y):
    if isinstance(x,list) or isinstance(y,tuple):
        if isinstance(y,list) or isinstance(y,tuple):
            return x + y
        else:
            return x + [y]
    else:
        if isinstance(y,list) or isinstance(y,tuple):
            return [x] + y
        else:
            return [x] + [y]
        
def update_dict (x ,y ):
    a = copy.deepcopy(x)
    common_keys = set(x.keys()).intersection(set(y.keys()))
    
    if common_keys:
        common_key_dict = dict( (ck,vector_add(x[ck],y[ck])) for ck in common_keys)
        uncommon_key_dict_x = dict ( (nckx,x[nckx]) for nckx in x if nckx not in common_keys)
        uncommon_key_dict_y = dict ( (ncky,x[ncky]) for ncky in y if ncky not in common_keys)
        a = common_key_dict
        a.update(uncommon_key_dict_x)
        a.update(uncommon_key_dict_y)
        return a
    else:
        a.update(y)
    
    return a
    
def create_dict(et):
    result ={}    
    for i in et:
        if len(i):
            result = update_dict(result, create_dict(i))
        else:
            items = dict( (k,v) for k,v in i.items())

            if i.text and items:
                result = update_dict(result,update_dict({i_tag:i.text},{i.tag:items}))
            elif i.text:
                result = update_dict(result,{i.tag: i.text})
            elif items:
                result = update_dict(result,{i.tag: items})
              
            
    return {et.tag: result}

a = '<xml> <a attr1="hello1"> <aa attr2="namaste"></aa> </a><b name="Alpha" /><b name="beta" /> </xml>'
b = '<xml> <a attr1="hello2"><aa>K</aa> </a> </xml>'

#print(compare_dict(a,b))


dictionary = create_dict(etree.fromstring(a))
end_nodes = get_end_nodes(dictionary)
print(end_nodes)
#for e in end_nodes :
#    node_obj = jsonpath_ng.parse(e)
#    print(node_obj)
print("RESULT:")
print(dictionary)

