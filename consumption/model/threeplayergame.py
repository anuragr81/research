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

def find_poor_probability(df,nu1, nu2,mu):
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


def find_rich_probability(df,nu1, nu2,mu):
    if not (isinstance(nu1,float) or isinstance(nu1,int)) or isinstance(nu1,list) or isinstance(nu1,tuple) or isinstance(nu1,np.ndarray):
        raise ValueError("nu1 must be a scalar")
    if not (isinstance(nu2,float) or isinstance(nu2,int)) or isinstance(nu2,list) or isinstance(nu2,tuple) or isinstance(nu2,np.ndarray):
        raise ValueError("nu2 must be a scalar")

    scalar_apply = lambda  x: df [  ( ( df.r2* mu + (1-mu)*df.s2 *c(x) )  > (df.r11* mu + (1-mu)*df.s11*c(nu1)) )   & ( (df.r2*mu + (1-mu)*df.s2 * c(x)) > (df.r12* mu + (1-mu)*df.s12 * c(nu1)) )]
    if isinstance(nu2,np.ndarray) or isinstance(nu2,tuple) or isinstance(nu2,list):
        if len(nu2):
            resarr=[]
            for nu2_ in nu2:
              res = scalar_apply(nu2_)
              resarr.append(res.shape[0]/df.shape[0])
              return np.array(resarr)
    else:
        res = scalar_apply(nu2)
        return res.shape[0]/df.shape[0]
    

def assets_poor(p,y1,y2,x,a,d,G):
  return y1-x + (p*y2 + (1-p)*y1)/d

def assets_rich(p,y1,y2,x,a,d,G):
  return y2-x + (p*y2 + (1-p)*y1)/d

def utility_poor(p,y1,y2,x,a,d,G):
  return G*(y1-x)**a + (p*G*y2**a + (1-p)*G*y1**a)/d

def utility_rich(p,y1,y2,x,a,d,G):
  return G*(y2-x)**a + (p*G*y2**a + (1-p)*G*y1**a)/d




def optimise_poor_utility_for_nu2 (nu2,mu,df,y1,y2,a,d,G):
  negative_utility_poor = lambda x : -utility_poor(x=x,p=find_poor_probability(mu=mu,nu1=x, nu2=nu2,df=df),y1=y1,y2=y2,a=a,d=d,G=G)
  res = minimize_scalar(negative_utility_poor, bounds=(0, y1), method='bounded')
  return res


def optimise_rich_utility_for_nu1 (nu1,df,y1,y2,a,d,G):
  negative_utility_rich = lambda x : -utility_rich(x=x,p=find_rich_probability(nu1=nu1, nu2=x,df=df),y1=y1,y2=y2,a=a,d=d,G=G)
  res = minimize_scalar(negative_utility_rich, bounds=(0, y2), method='bounded')
  return res


def plot_common_utility_over_nu2(df,y1,y2,a,d,G):
    y2s  = np.linspace(0.1,y2,500)

    plt.plot(y2s, [common_utility( df=df,nu2=nu2,y1=y1,y2=y2,G=G,a=a,d=d) for nu2 in y2s])
    
    plt.xlabel('rich-expenditure')
    plt.ylabel('common-utility')
    plt.title(label="Population-weight shared utility")
    plt.show()


def plot_rich_utility (df,y1,y2,a,d,G):
    y2s  = np.linspace(0,y2,100)
    fig, ax = plt.subplots()
    y1arr=[t*y1 for t in (.1,.3,.6,.9,)]
    l1, = ax.plot(y2s, [utility_rich(x=t,p=find_rich_probability(nu1=y1arr[0],nu2=t,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y2s])
    l2, = ax.plot(y2s, [utility_rich(x=t,p=find_rich_probability(nu1=y1arr[1],nu2=t,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y2s])
    l3, = ax.plot(y2s, [utility_rich(x=t,p=find_rich_probability(nu1=y1arr[2],nu2=t,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y2s])
    l4, = ax.plot(y2s, [utility_rich(x=t,p=find_rich_probability(nu1=y1arr[3],nu2=t,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y2s])
    
    ax.legend((l1, l2, l3,l4), tuple('poor-expenditure='+str(y) for y in y1arr), loc='lower left', shadow=True)
    ax.set_xlabel('expenditure')
    ax.set_ylabel('utility')
    ax.set_title('Utility for rich')
    plt.show()



def plot_poor_utility(df,y1,y2,a,d,G):
    y1s  = np.linspace(0,y1,100)
    fig, ax = plt.subplots()
    y2arr=[t*y2 for t in (.1,.3,.6,.9,)]
    l1, = ax.plot(y1s, [utility_poor(x=t,p=find_poor_probability(nu1=t, nu2=y2arr[0],df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y1s])
    l2, = ax.plot(y1s, [utility_poor(x=t,p=find_poor_probability(nu1=t, nu2=y2arr[1],df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y1s])
    l3, = ax.plot(y1s, [utility_poor(x=t,p=find_poor_probability(nu1=t, nu2=y2arr[2],df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y1s])
    l4, = ax.plot(y1s, [utility_poor(x=t,p=find_poor_probability(nu1=t, nu2=y2arr[3],df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y1s])
    
    ax.legend((l1, l2, l3,l4), tuple('rich-expenditure='+str(y) for y in y2arr), loc='lower left', shadow=True)
    ax.set_xlabel('expenditure')
    ax.set_ylabel('poor-utility')
    ax.set_title('Utility for poor')
    plt.show()
    










def common_utility(df,mu,nu2,y1,y2,G,a,d):
  poor_share=.3
  rich_share=1-poor_share
  nu1selectedfornu2 = optimise_poor_utility_for_nu2(nu2=nu2,mu=mu,df=df,y1=y1,y2=y2,G=G,a=a,d=d).x
  poorutil= utility_poor(p=find_poor_probability(nu1=nu1selectedfornu2, nu2=nu2,df=df,mu=mu),y1=y1,y2=y2,x=nu1selectedfornu2,a=a,d=d,G=G)
  richutil =  utility_rich(p=find_rich_probability(nu1=nu1selectedfornu2, nu2=nu2,df=df,mu=mu),y1=y1,y2=y2,x=nu2,a=a,d=d,G=G)
  return poor_share*poorutil + rich_share* richutil

def maximise_common_utilty_over_nu2 (df,mu,y1,y2,a,d,G):
  negative_common_utility = lambda x : -common_utility(df=df,mu=mu,nu2=x,y1=y1,y2=y2,a=a,d=d,G=G)
  res = minimize_scalar(negative_common_utility, bounds=(0, y2), method='bounded')
  return res


def plot_common_utility_vs_inequality(df,mu,G,a,d):
    inequality_x=np.linspace(.1,.99,20)
    Y = 100
    
    nu2_maxcommonutil_vals = []
    maxutils=[]
    
    for x_ in inequality_x:
      cury1=Y*x_
      cury2=Y*(1-x_)
      maxnu2val = maximise_common_utilty_over_nu2(df=df,mu=mu,y1=cury1,y2=cury2,a=a,d=d,G=G).x
      nu2_maxcommonutil_vals.append(maxnu2val)
      maxutil = common_utility( df=df,mu=mu,nu2=maxnu2val,y1=cury1,y2=cury2,G=G,a=a,d=d)
      maxutils.append(maxutil)
      
    plt.plot(inequality_x,maxutils)

    plt.xlabel('inequality')
    plt.ylabel('common-utility')
    
    plt.show()


def plot_common_utility_vs_inequality_over_mus(df,G,a,d):
    inequality_x=np.linspace(.1,.99,20)
    Y = 100
    
    mus = [.2,.3,.4]
    
    fig, ax = plt.subplots()
    plot_handles=[]

    for mu in mus:
        nu2_maxcommonutil_vals = []
        maxutils=[]
        
        for x_ in inequality_x:
          cury1=Y*x_
          cury2=Y*(1-x_)
          maxnu2val = maximise_common_utilty_over_nu2(mu=mu,df=df,y1=cury1,y2=cury2,a=a,d=d,G=G).x
          nu2_maxcommonutil_vals.append(maxnu2val)
          maxutil = common_utility( mu=mu,df=df,nu2=maxnu2val,y1=cury1,y2=cury2,G=G,a=a,d=d)
          maxutils.append(maxutil)
      
        l, = ax.plot(inequality_x,maxutils)
        plot_handles.append(l)        
        


    ax.legend(tuple(plot_handles), tuple('mu='+str(y) for y in mus), loc='lower right', shadow=True)
    ax.set_xlabel('inequality')
    ax.set_ylabel('common-utility')
    ax.set_title('common-utility vs inequality')

    plt.show()
    
if __name__ == "__main__":
    N  = 100000
    #mu = .2
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
    #plot_rich_utility(df=df, y1=y1, y2=y2, a=a, d=d, G=G)
    #plot_poor_utility(df=df, y1=y1, y2=y2, a=a, d=d, G=G)
    #plot_common_utility_over_nu2(df=df, y1=y1, y2=y2, a=a, d=d, G=G)
    #plot_common_utility_vs_inequality(df=df,G=G,a=a,d=d)
    plot_common_utility_vs_inequality_over_mus(df=df,G=G,a=a,d=d)