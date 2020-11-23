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
        return reduce(lambda x,y : x + y, [get_end_nodes(path=path,root=k) for k in root],[])
    else:
        return [path]
    
    
def update_dict (x ,y ):
    a = copy.deepcopy(x)
    a.update(y)
    return a
    
def create_dict(et):
    result ={}
    print (et.tag + " has %d elements" % len(et))
    for i in et:
        if len(i):
            result = update_dict(result, {i.tag : create_dict(i) })
            print(result)
            print("<<RESULT")
        else:
            items = dict( (k,v) for k,v in i.items())
#            print ("Element - dict:%s text=%s" % (str(items), i.text) )
            if i.text and items:
                raise ValueError("Both value and attributes are not supported")
            if i.text:
                result = update_dict(result,{i.tag: i.text})
            if items:
                result = update_dict(result,{i.tag: items})
                
            print(result)
            print("<< NODE RESULT")
            
    return {et.tag: result}

a = '<xml> <a attr1="hello1"> <aa attr2="namaste"></aa> </a><b name="Alpha" /> </xml>'
b = '<xml> <a attr1="hello2"><aa>K</aa> </a> </xml>'

#print(compare_dict(a,b))


dictionary = create_dict(etree.fromstring(a))
#end_nodes = get_end_nodes(dictionary)
#for e in end_nodes :
#    node_obj = jsonpath_ng.parse(e)
#    print(node_obj)
print("RESULT:")
print(dictionary)

