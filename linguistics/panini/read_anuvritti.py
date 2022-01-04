import pandas as pd
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
		   
    
def add_at_nth_column(st,n_index,data,allow_append =True):
    """
    Go to the index'th column - i.e. level of the hierarchy and add
      the element at the level
    The function generates the tree of the form [Root,[Child1],[Child2],...,]
    The elements of the form [A,B,C] are nodes - but the [A,[B,C],C] - represents
       a tree
    """    
    print(data)
    if n_index>0:
        st [-1] =  add_at_nth_column(st[-1], n_index-1,data)
        return st

    if isinstance(st,list):
        
        if st :
            if isinstance(st[-1],list):           
                st[-1].append(data)
            else:
                #create a new tree form the last node
                old_data_to_be_nested=st[-1]
                st[-1]=[old_data_to_be_nested,[data]]
        else:
            # initialising
            st=['root',[data]]
    else:
        raise ValueError("Invalid Type")
        
    return st
    #    raise ValueError("Invalid insert")

def parse_struct(st):
    """
    Build a structure of form {key:[value1,value2]} where value1 could be
     another structure of the same form.
    """
    if st:
        if isinstance(st,list):
            if len(st)>1:
                # first node in the list is the root of the sub-tree to be followed
                if not isinstance(st[0],list):
                    new_root = OrderedDict({st[0]:[]})
                    for x in st[1:]:
                        
                        if all(not isinstance(y,list) for y in x):
                            # the following step treates scalars as nodes
                            # rather than subtrees
                            new_root[st[0]].append(x)
                        else:
                            result=parse_struct(st=x)
                            #print("Adding (%s,%s)"% (st[0],result))
                            new_root[st[0]].append(result)
                  
                    return new_root
                else:
                    #raise ValueError("Can't be a root")
                    # treat them as list of sub-trees
                    return [parse_struct(x) for x in st]
            else:#st==1              
                result=parse_struct(st[0])
                return result
        else:
            #nodes
            #print("node=%s"%st)
            return st
    else:        
        return None


def color_for_index(ci):
    return {0:'Black',1:'DarkRed',2:'Crimson',3:'Chocolate',
            4:'DarkOliveGreen',5:'Green',6:'DeekSkyBlue',
            7:'DodgerBlue',8:'MediumBlue',9:'Navy',10:'Violet',11:'Purple',
            12:'Magenta'}[ci]

def text_in_colours(text,colorIndex,prefix=''):
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
#b = pd.read_excel('C:/Users/anura/OneDrive/Documents/sanskrit/ashtadhyayi_chapter1_2.xlsx')
nrows = b.shape[0]
N=b.shape[1]


if N>10:
    raise ValueError("Number of columns higher than what is supported")

struct=[]
results =[]
for i in range(nrows):
    for j in range(N):
        row= (b.loc[i])
        if not is_null_col(row,j):
            data_to_add=row["L"+str(j+1)]
            results = results + [(j+1,d) for d in str(data_to_add).split(',')]
            for d in str(data_to_add).split(','):
                struct=add_at_nth_column(struct,j,d)
pprint(struct)

dict_struct =parse_struct(['root',struct])
pprint(dict(dict_struct))
text=colored_html(dict_struct)
print(text)
write_into_file(', '.join(text))
