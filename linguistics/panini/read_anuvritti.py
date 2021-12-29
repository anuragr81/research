import pandas as pd
import os,sys

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
                   

class Struct:
	def __init__(self):
		self.struct =[[]]
		
		

	def add(self,level,data):
		if not isinstance(level,int):
			raise ValueError("Not in: %s" % level)
		self.struct[-1][level-1].append(data)


		

b = pd.read_excel('c:/temp/test.xlsx')
nrows = b.shape[0]

N=b.shape[1]

if N>10:
    raise ValueError("Number of columns higher than what is supported")

#struct = [ [ [    [ [[    [[[     []         ]]]       ]]]  ] ] ]
    
struct=[]

    
def add_at_nth_column(st,n_index,data):
    """
    Go to the index'th column - i.e. level of the hierarchy and add
      the element at the level
    The function generates the tree of the form [Root,[Child1],[Child2],...,]
    The elements of the form [A,B,C] are nodes - but the [A,[B,C],C] - represents
       a tree
    """    
    cur_pos = len(st)-1
    
    if n_index>0:
        st [cur_pos] =  add_at_nth_column(st[cur_pos], n_index-1,data)
        return st
    if isinstance(st,list):
        if st and isinstance(st[-1],list):
            st[-1].append(data)
        else:
            st.append([data])
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
                    new_root = {st[0]:[]}
                    for x in st[1:]:
                        result=parse_struct(st=x)
                        print(result)
                        new_root[st[0]].append(result)
                  
                    return new_root
                else:
                    raise ValueError("Can't be a root")
            else:#st==1
                
                result=parse_struct(st[0])
                print(result)
                return result
        else:
            #nodes
            print("node=%s"%st)
            return st
    else:        
        return None

struct=add_at_nth_column(struct,0,"X")
struct=add_at_nth_column(struct,1,"Y")
if True:
    struct=add_at_nth_column(struct,2,"Z")
    struct=add_at_nth_column(struct,2,"A")
    struct=add_at_nth_column(struct,1,"B")
    struct=add_at_nth_column(struct,0,"C")

print(struct)
parse_struct(struct)
sys.exit(0)
for i in range(nrows):
    current_level = N
    while current_level>1:
        row= (b.loc[i])
        maxCol = (find_first_non_na_backwards(row))
        



