import pandas as pd
from copy import deepcopy
from pprint import pprint
import os,sys
from collections import OrderedDict

"""
There can be no dangling inserts. The only new nodes are at roots. A new L3 goes to L2 (not anything before L2). Therefore, Ln is inserted only in L(n-1).
Further insertion of Ln is recursive so the insert only happens after L(n+1)...L(N) have been parsed. A practical way is to start from LN where N is the
    of columns, fail the insertion if nothing is found at n-1 (n=N, N-1,... 2) and stop when empty is found.
"""
def find_first_non_na(dat):
	if "L1" in dat and not pd.isnull(dat.L1):
		return 1
	if "L2" in dat and not pd.isnull(dat.L2):
		return 2
	if "L3" in dat and not pd.isnull(dat.L3):
		return 3
	if "L4" in dat and not pd.isnull(dat.L4):
		return 4
	if "L5" in dat and not pd.isnull(dat.L5):
		return 5
	if "L6" in dat and not pd.isnull(dat.L6):
		return 6
	if "L7" in dat and not pd.isnull(dat.L7):
		return 7
	if "L8" in dat and not pd.isnull(dat.L8):
		return 8
	if "L9" in dat and not pd.isnull(dat.L9):
		return 9
	if "L10" in dat and not pd.isnull(dat.L10):
		return 10
	return None

def find_first_non_na_backwards(dat):
    if "L10" in dat and not pd.isnull(dat.L10):
        return 10
    if "L9" in dat and not pd.isnull(dat.L9):
        return 9
    if "L8" in dat and not pd.isnull(dat.L8):
        return 8
    if "L7" in dat and not pd.isnull(dat.L7):
        return 7
    if "L6" in dat and not pd.isnull(dat.L6):
        return 6
    if "L5" in dat and not pd.isnull(dat.L5):
        return 5
    if "L4" in dat and not pd.isnull(dat.L4):
        return 4
    if "L3" in dat and not pd.isnull(dat.L3):
        return 3
    if "L2" in dat and not pd.isnull(dat.L2):
        return 2
    if "L1" in dat and not pd.isnull(dat.L1):
        return 1
    return None

	

def is_null_col(dat,col):
    if col==0:
        return pd.isnull(dat.L1)
    if col==1:        
        return pd.isnull(dat.L2)
    if col==2:        
        return pd.isnull(dat.L3)
    if col==3:        
        return pd.isnull(dat.L4)
    if col==4:        
        return pd.isnull(dat.L5)
    if col==5:        
        return pd.isnull(dat.L6)
    if col==6:        
        return pd.isnull(dat.L7)
    if col==7:        
        return pd.isnull(dat.L8)
    if col==8:        
        return pd.isnull(dat.L9)
    if col==9:        
        return pd.isnull(dat.L10)
    
    raise ValueError("Number of columns higher than what is supported")

def is_list_a_tree(x):
    if not isinstance(x,list):
        return False
    elif not x:
        return False
    else:
        if len(x) == 2:
            if not isinstance(x[0],list) and isinstance(x[1],list):
                return True
        else:
            return False
    
    
def add_at_nth_column_failed(st,n_index,data):
    print("data=",data,",n_index=",n_index)
    if not st:
        # initialising
        return ['root',[data]]    


    if n_index ==0:
        st[1].append(data)
    else:
        pos=st
        for i in range(0,n_index):
            pos = pos[-1]
            if i== (n_index-1):
               pos = add_node(pos,data)

    return st
    
def get_index(x,i):
    if i == 0:
        return x[-1]
    elif i == 1:
        return x[-1][-1]
    elif i == 2:
        return x[-1][-1][-1]
    elif i == 3:
        return x[-1][-1][-1][-1]
    elif i == 4:
        return x[-1][-1][-1][-1][-1]
    elif i == 5:
        return x[-1][-1][-1][-1][-1][-1]
    elif i == 6:
        return x[-1][-1][-1][-1][-1][-1][-1]
    else:
        raise ValueError("Not supported")



def set_index(x,i,dat):
    if i == 0:
        x[-1]= dat
        
    elif i == 1:
        x[-1][-1]=dat
    elif i == 2:
        x[-1][-1][-1]= dat
    elif i == 3:
        x[-1][-1][-1][-1]= dat
    elif i == 4:
        x[-1][-1][-1][-1][-1]= dat
    elif i == 5:
        x[-1][-1][-1][-1][-1][-1]= dat
    elif i == 6:
        x[-1][-1][-1][-1][-1][-1][-1]= dat
    else:
        raise ValueError("Not supported")

def append_index(x,i,dat):
    if i == 0:
        x[-1].append(dat)
    elif i == 1:
        x[-1][-1].append(dat)
    elif i == 2:
        x[-1][-1][-1].append(dat)
    elif i == 3:
        x[-1][-1][-1][-1].append(dat)
    elif i == 4:
        x[-1][-1][-1][-1][-1].append(dat)
    elif i == 5:
        x[-1][-1][-1][-1][-1][-1].append(dat)
    elif i == 6:
        x[-1][-1][-1][-1][-1][-1][-1].append(dat)
    else:
        raise ValueError("Not supported")

class ColumnCache:
    
    def __init__(self):
        self.col=None
    def set_col(self,j):
        self.col = j
    def get_col(self):
        return self.col

def get_index_dict(x,i):
    if i == 0:
        return x
    if i == 1:
        return x[[y for y in x.keys()][-1]]        
    if i==2 :
        return x['root'][-1][[y for y in x['root'][-1].keys()][0]]
    if i == 3:
        n_2 = x['root'][-1][[y for y in x['root'][-1].keys()][0]]
        return n_2[-1][[j for j in n_2[-1].keys()][-1]]
    if i == 4 :
        n_3 = get_index_dict(x, 3)
        return n_3[-1][[j for j in n_3[-1].keys()][-1]]
    if i == 5:
        n_4 = get_index_dict(x, 4)
        return n_4[-1][[j for j in n_4[-1].keys()][-1]]
    if i == 6:
        n = get_index_dict(x, 5)
        return n[-1][[j for j in n[-1].keys()][-1]]
    if i == 7:
        n = get_index_dict(x, 6)
        return n[-1][[j for j in n[-1].keys()][-1]]
    
    if i == 8:
        n = get_index_dict(x, 7)
        return n[-1][[j for j in n[-1].keys()][-1]]
    if i == 9:
        n = get_index_dict(x, 8)
        return n[-1][[j for j in n[-1].keys()][-1]]
    if i == 10:
        n = get_index_dict(x, 9)
        return n[-1][[j for j in n[-1].keys()][-1]]
    if i == 11:
        n = get_index_dict(x, 10)
        return n[-1][[j for j in n[-1].keys()][-1]]
    if i == 12:
        n = get_index_dict(x, 11)
        return n[-1][[j for j in n[-1].keys()][-1]]
    if i == 13:
        n = get_index_dict(x, 12)
        return n[-1][[j for j in n[-1].keys()][-1]]
    if i == 14:
        n = get_index_dict(x, 13)
        return n[-1][[j for j in n[-1].keys()][-1]]
    if i == 15:
        n = get_index_dict(x, 14)
        return n[-1][[j for j in n[-1].keys()][-1]]
    
    raise ValueError("Not supported")


        
def add_at_nth_column(cc,st,n_index,data):
    """
    find the n_index and then call add_node
    """
    print("data=",data,",n_index=",n_index)
    if not st:
        return OrderedDict({'root':[data]})
    
    old_data = get_index_dict(st,n_index)
    if not isinstance(old_data,OrderedDict):
        # must be a list
        if not isinstance(old_data,list):
            raise ValueError("not a list")
        if isinstance(old_data[-1],OrderedDict):
            last_key = [x for x in old_data[-1].keys()][-1]
            old_data[-1][last_key ].append(data)
        else:
            old_data[-1] = OrderedDict({old_data[-1]: [data]})
    elif isinstance(old_data,OrderedDict):
        last_key = [x for x in old_data.keys()][-1]
        old_data[last_key].append(OrderedDict({data: []}))
    else:
        raise ValueError("Unimplemented")
    return st
        
def add_at_nth_column_list(cc,st,n_index,data):
    """
    find the n_index and then call add_node
    """
    print("data=",data,",n_index=",n_index)
    if not st:
        st = add_node(st,data)
        return st
    
    old_data = get_index(st,n_index)
    
    if isinstance(old_data,list):
        
        if cc.get_col() is not None:
            if n_index > cc.get_col():
                new_root = [deepcopy(old_data[-1]), [data]]
                new_data = old_data[0:-1]+ [new_root]
                set_index(st,n_index,new_data )
            else:
                append_index(st,n_index,data)
        else:
            append_index(st,n_index+1,data)
    else:    
        set_index(st,n_index,[old_data ,[data] ])
    cc.set_col(n_index)
    return st
       


    
def add_node(st,data):
    """
    adds a node at the DFS       
    """    
    if not st:
        # initialising
        return ['root',[deepcopy(data)]]

    if isinstance(st,list):
    
        if isinstance(st[-1],list):   
            st [-1] = add_node(st[-1],data)
        else:
            #create a new tree form the last node
            st[-1]=[deepcopy(st[-1]),[data]]
            
        return st
        
    else:        
        return [deepcopy(st),[data]]

def parse_list(st):
    res= []
    for x  in st:
        res.append(parse_struct(x))
    return res

def parse_struct(st):
    if st:
        if not isinstance(st,list):
            return st
        
        if len(st)>2:
            if isinstance(st,list):
                return parse_list(st)
            else:
                raise ValueError("Not a tree")
                
        if len(st)==1:
            return parse_struct(st[0])

        if isinstance(st[0],list):
            raise ValueError("Must be a scalar")
            
        if isinstance(st[1],list):
            return OrderedDict({st[0]:parse_list(st[1])})
        else:
            return st
    else:
        return st
    
def color_for_index(ci):
    general_dict = {0:'Black',1:'DarkRed',2:'Crimson',3:'Chocolate',
            4:'DarkOliveGreen',5:'Green',6:'DeekSkyBlue',
            7:'DodgerBlue',8:'MediumBlue',9:'Navy',10:'Violet',11:'Purple',
            12:'Magenta'}
    narrow_dict = {0:'Black',1:'Black',2:'Crimson',3:'Chocolate',
            4:'Green',5:'DeekSkyBlue',
            6:'DodgerBlue',7:'Navy',8:'Violet',9:'Purple',
            10:'Magenta'}
    return narrow_dict[ci]
    
    
    

def text_in_colours(text,colorIndex,prefix=''):
    if colorIndex <= 1:
        prefix = "<br>"#"&nbsp;&nbsp;"
    return prefix+'<span style="color:'+color_for_index(colorIndex)+'">'+text+'</span>'

def colored_html(st,colorIndex=0,paths=[]):
    if st:
        if isinstance(st,list):
            for x in st:
                paths = colored_html(x,colorIndex,paths=paths)
            return paths
        elif isinstance(st,OrderedDict):
            for k,v in st.items():
                paths.append(text_in_colours(text=k,colorIndex=colorIndex))
                paths = colored_html(v,colorIndex+1,paths=paths)
            return paths
        else:
            if isinstance(st,dict):
                raise ValueError("Unsupported type")
            paths.append(text_in_colours(text=st,colorIndex=colorIndex))
            return paths
    else:
        return paths


def test_add_at_nth_columns():
    struct=[]
    struct=add_at_nth_column(struct,0,"X")
    struct=add_at_nth_column(struct,1,"Y")
    
    struct=add_at_nth_column(struct,2,"Z")
    struct=add_at_nth_column(struct,2,"A")
    struct=add_at_nth_column(struct,2,"B")
    struct=add_at_nth_column(struct,1,"C")
    struct=add_at_nth_column(struct,0,"D")
    
    dict_struct =parse_struct(['root',struct])
    assert dict_struct==OrderedDict([('root', [[OrderedDict([('X', [['Y', 'Z', 'A', 'B'], 'C'])]), 'D']])])
    x=colored_html(dict_struct)
    colored_text_to_write=', '.join(x)
    assert colored_text_to_write=='<span style="color:Black">root</span>, <span style="color:DarkRed">X</span>, <span style="color:Crimson">Y</span>, <span style="color:Crimson">Z</span>, <span style="color:Crimson">A</span>, <span style="color:Crimson">B</span>, <span style="color:Crimson">C</span>, <span style="color:DarkRed">D</span>'

def write_into_file(text):
    with open('text_output.html','w',encoding='utf-8') as fh:
        fh.write("<!DOCTYPE html> \n <html> \n <body> \n <p>"+text+"</p> </body>\n </html>")

#test_add_at_nth_columns()
#sys.exit(0)
b = pd.read_excel('c:/temp/test.xlsx')
#b = pd.read_excel('c:/temp/test3.xlsx')
#b = pd.read_excel('C:/Users/anura/OneDrive/Documents/sanskrit/ashtadhyayi_chapter1_2.xlsx')
nrows = b.shape[0]
N=b.shape[1]


if N>10:
    raise ValueError("Number of columns higher than what is supported")

cc = ColumnCache()
struct=[]
results =[]
for i in range(nrows):
    for j in range(N):
        row= (b.loc[i])
        if not is_null_col(row,j):
            data_to_add=row["L"+str(j+1)]
            results = results + [(j+1,d) for d in str(data_to_add).split(',')]
            for d in str(data_to_add).split(','):
                struct=add_at_nth_column(cc ,struct,j,d)
pprint(struct)



dict_struct =parse_struct(struct); text=colored_html(dict_struct);  write_into_file(', '.join(text))
#print(text)

