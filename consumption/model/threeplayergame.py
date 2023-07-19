# -*- coding: utf-8 -*-
"""
Created on Tue Jul 18 11:31:14 2023

@author: anura
"""
import numpy as np
import pandas as pd
from scipy.optimize import minimize_scalar

import matplotlib.pyplot as plt
import numpy as np


#Using example from https://docs.scipy.org/doc/scipy/tutorial/optimize.html#constrained-minimization-of-multivariate-scalar-functions-minimize




def mul(rowa,rowb):
  return np.matmul(np.asmatrix(rowa).T ,np.asmatrix(np.array(rowb)))

def c(x):
  return 1 - 1 / (1+x)**.5

def find_poor_probability(df,nu1, nu2):
    if not (isinstance(nu2,float) or isinstance(nu2,int)) or isinstance(nu2,list) or isinstance(nu2,tuple) or isinstance(nu2,np.ndarray):
        raise ValueError("nu2 must be a scalar")
        
    scalar_apply = lambda  x: df [  ( ( df.r11* mu + (1-mu)*df.s11 *c(x) )  > (df.r12* mu + (1-mu)*df.s12*c(x)) )   & ( (df.r11*mu + (1-mu)*df.s11 * c(x)) > (df.r2* mu + (1-mu)*df.s2 * c(nu2)) )]
    if isinstance(nu1,np.ndarray) or isinstance(nu1,tuple) or isinstance(nu1,list):
        if len(nu1):
            resarr=[]
            for nu1_ in nu1:
              res = scalar_apply(nu1_)
              resarr.append(res.shape[0]/df.shape[0])
              return np.array(resarr)
    else:
        res = scalar_apply(nu1)
        return res.shape[0]/df.shape[0]

def find_rich_probability(df,nu1, nu2):
    if not (isinstance(nu2,float) or isinstance(nu2,int)) or isinstance(nu2,list) or isinstance(nu2,tuple) or isinstance(nu2,np.ndarray):
        raise ValueError("nu2 must be a scalar")
        
    scalar_apply = lambda  x: df [  ( ( df.r11* mu + (1-mu)*df.s11 *c(x) )  > (df.r12* mu + (1-mu)*df.s12*c(x)) )   & ( (df.r11*mu + (1-mu)*df.s11 * c(x)) > (df.r2* mu + (1-mu)*df.s2 * c(nu2)) )]
    if isinstance(nu1,np.ndarray) or isinstance(nu1,tuple) or isinstance(nu1,list):
        if len(nu1):
            resarr=[]
            for nu1_ in nu1:
              res = scalar_apply(nu1_)
              resarr.append(res.shape[0]/df.shape[0])
              return np.array(resarr)
    else:
        res = scalar_apply(nu1)
        return res.shape[0]/df.shape[0]



def utility_poor(p,y1,y2,x,a,d,G): 
  return G*(y1-x)**a + (p*G*y2**a + (1-p)*G*y1**a)/d


def optimise_poor_utility_for_nu2 (nu2,df,y1,y2,a,d,G):
  negative_utility_poor = lambda x : -utility_poor(x=x,p=find_poor_probability(nu1=x, nu2=nu2,df=df),y1=y1,y2=y2,a=a,d=d,G=G)
  res = minimize_scalar(negative_utility_poor, bounds=(0, y1), method='bounded')
  return res

if __name__ == "__main__":
    N  = 1000000
    mu = .2
    y1=10
    y2=100
    a = .2
    G=10
    d=1.1
    
    
    r11=np.random.uniform(0,1,N)
    s11=np.random.uniform(0,1,N)
    
    r12=np.random.uniform(0,1,N)
    s12=np.random.uniform(0,1,N)
    
    r2=np.random.uniform(0,1,N)
    s2=np.random.uniform(0,1,N)
    
    
    df = pd.DataFrame({'r11':r11,'s11':s11,'r12':r12,'s12':s12,'r2':r2,'s2':s2})
    
    ndelta = 20
    nu1arr= np.linspace(0,y1,ndelta )
    nu2arr= np.linspace(0,y2,ndelta )
    
    
    print(optimise_poor_utility_for_nu2(nu2=90,df=df,y1=y1,y2=y2,G=G,a=a,d=d))
    
    
    fig, ax = plt.subplots()
    
    ax.plot(nu1arr, [utility_poor(x=x,p=find_poor_probability(nu1=x, nu2=90,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for x in nu1arr])
    
    ax.set(xlabel='expenditure', ylabel='utility',
           title='poor utility')
    ax.grid()
    
    plt.show()