import pandas as pd
import sys
from itertools import product


if __name__ == "__main__":
    x = pd.read_csv(sys.argv[1])
    ho2014= pd.read_csv(sys.argv[1])
    dat=ho2014[['hhid','age']]
    res = pd.DataFrame({'hhid':[],'YOB_array':[]})
    for j in sorted(list(set(dat.hhid))):
    	print("Adding:"+str(j))
    	r = [[2014-j,2014-j-1] for j in dat[dat.hhid==j].to_dict()['age'].values()]
    	rows = [ sorted(k) for k in product(*r) ]
    	res = res.append(pd.DataFrame({'hhid':[j]*len(rows),'YOB_array':rows}))
    res.to_csv('c:/temp/res.csv')