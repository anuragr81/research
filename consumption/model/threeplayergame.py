# -*- coding: utf-8 -*-
"""
Created on Tue Jul 18 11:31:14 2023

@author: anura
"""
import numpy as np
import pandas as pd
import itertools
from scipy.optimize import minimize_scalar
import functools

import matplotlib.pyplot as plt
plt.rcParams['text.usetex'] = True

import numpy as np


#Using example from https://docs.scipy.org/doc/scipy/tutorial/optimize.html#constrained-minimization-of-multivariate-scalar-functions-minimize


def mul(rowa,rowb):
  return np.matmul(np.asmatrix(rowa).T ,np.asmatrix(np.array(rowb)))

def c(x):
  return 1 - 1 / (1+x)**.5


"""
Makes expression from pairs of participants
"""
def make_extraction_expression(base_participant,pairs_rich_or_poor):
    if base_participant == "poor":
        base_expenditure = "nu1"
    elif base_participant == "rich":
        base_expenditure = "nu2"
    else:
        raise ValueError("Unsupported type for rich/poor indicator")
        
    expression=""
    prefix = ""
    for base, other,other_rich_or_poor in pairs_rich_or_poor:
        if other_rich_or_poor  == "poor":
            other_expenditure  = "nu1"
        elif other_rich_or_poor   == "rich":
            other_expenditure  = "nu2"
        else:
            raise ValueError("Unsupported type for rich/poor indicator")
            

        r_base,s_base = base
        r_other,s_other = other
        base_score = (" ( mu*df.%s" % r_base + "+c(" + base_expenditure +  ")*(1-mu)*df.%s ) " %  s_base )
        other_score = (" ( mu*df.%s" % r_other + "+c(" + other_expenditure +")*(1-mu)*df.%s ) " %  s_other)
        expression = expression + ( prefix  + (" ( " + base_score   + " > " + other_score + " ) ") )
        prefix=" & "
    
    return expression

"""
Uniformly distributed normal and uniform variables
"""
def generate_nplayer_unif_df (N,numpoor,numtotal):
    if numtotal <= numpoor:
        raise ValueError("numpoor must be less than or equal to numtotal")
        
    poor_columns_r = ["r1"+str(i+1) for i in range(0,numpoor)]
    poor_columns_s = ["s1"+str(i+1) for i in range(0,numpoor)]   
    
    
    rich_columns_r = ["r2"+str(i+1) for i in range(0,numtotal-numpoor)]
    rich_columns_s = ["s2"+str(i+1) for i in range(0,numtotal-numpoor)]
    
    all_cols = poor_columns_r + poor_columns_s + rich_columns_r  + rich_columns_s
   
    df = pd.DataFrame(dict ( (colname,np.random.uniform(0,1,N)) for colname in all_cols) )
    return df


"""
Calculates poor participant's  win probability. The first-poor participant (r11,s11) is the base-particpant relative
 to whom the probability to win is calculated.
"""
def find_poor_probability_np(df,nu1, nu2,mu,numpoor, numtotal):
    
    if not (isinstance(nu2,float) or isinstance(nu2,int)) or isinstance(nu2,list) or isinstance(nu2,tuple) or isinstance(nu2,np.ndarray):
        raise ValueError("nu2 must be a scalar")
    other_poor_columns_r = ["r1"+str(i+1) for i in range(1,numpoor)]
    other_poor_columns_s = ["s1"+str(i+1) for i in range(1,numpoor)]
    other_poor_tuples = [t for t in zip(other_poor_columns_r ,other_poor_columns_s) ]
    
    
    rich_columns_r = ["r2"+str(i+1) for i in range(0,numtotal-numpoor)]
    rich_columns_s = ["s2"+str(i+1) for i in range(0,numtotal-numpoor)]
    rich_tuples = [t for t in zip(rich_columns_r ,rich_columns_s) ]
    
    pairs_with_rich = [ x + ('rich',) for x in itertools.product([('r11','s11')],rich_tuples)]
    pairs_with_other_poor= [ x + ('poor',) for x in itertools.product([('r11','s11')],other_poor_tuples)]
    
    
    expression = make_extraction_expression("poor",pairs_with_rich +pairs_with_other_poor)
    
    res = df[eval(expression)]
    return res.shape[0]/df.shape[0]
    
"""
Calculates rich participant's  win probability. The first-rich participant (r21,s21) is the base-particpant relative
 to whom the probability to win is calculated.
"""
def find_rich_probability_np(df,nu1, nu2,mu,numpoor,numtotal):
    if not (isinstance(nu1,float) or isinstance(nu1,int)) or isinstance(nu1,list) or isinstance(nu1,tuple) or isinstance(nu1,np.ndarray):
        raise ValueError("nu1 must be a scalar")
    if not (isinstance(nu2,float) or isinstance(nu2,int)) or isinstance(nu2,list) or isinstance(nu2,tuple) or isinstance(nu2,np.ndarray):
        raise ValueError("nu2 must be a scalar")

    other_rich_columns_r = ["r2"+str(i+1) for i in range(1,numtotal-numpoor)]
    other_rich_columns_s = ["s2"+str(i+1) for i in range(1,numtotal-numpoor)]
    other_rich_tuples = [t for t in zip(other_rich_columns_r ,other_rich_columns_s ) ]
    
    
    poor_columns_r = ["r1"+str(i+1) for i in range(0,numpoor)]
    poor_columns_s = ["s1"+str(i+1) for i in range(0,numpoor)]
    poor_tuples = [t for t in zip(poor_columns_r ,poor_columns_s) ]
    
    pairs_with_other_rich = [ x + ('rich',) for x in itertools.product([('r21','s21')],other_rich_tuples)]
    pairs_with_poor= [ x + ('poor',) for x in itertools.product([('r21','s21')],poor_tuples)]
    
    
    expression = make_extraction_expression("rich",pairs_with_poor +pairs_with_other_rich )
    
    res = df[eval(expression)]
    return res.shape[0]/df.shape[0]
    

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


def plot_rich_utility (mu,df,y1,y2,a,d,G,rich_prob_func=None):
    if not rich_prob_func:
        rich_prob_func=find_rich_probability
    y2s  = np.linspace(0,y2,100)
    fig, ax = plt.subplots()
    y1arr=[t*y1 for t in (.1,.3,.6,.9,)]
    l1, = ax.plot(y2s, [utility_rich(x=t,p=rich_prob_func(mu=mu,nu1=y1arr[0],nu2=t,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y2s])
    l2, = ax.plot(y2s, [utility_rich(x=t,p=rich_prob_func(mu=mu,nu1=y1arr[1],nu2=t,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y2s])
    l3, = ax.plot(y2s, [utility_rich(x=t,p=rich_prob_func(mu=mu,nu1=y1arr[2],nu2=t,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y2s])
    l4, = ax.plot(y2s, [utility_rich(x=t,p=rich_prob_func(mu=mu,nu1=y1arr[3],nu2=t,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y2s])
    
    ax.legend((l1, l2, l3,l4), 
              #tuple('poor-expenditure \\nu_1='+str(y) for y in y1arr), 
              tuple(r'$\nu_1$=' +str(y)  for y in y1arr),
              loc='lower left', shadow=True)
    
    ax.set_xlabel(r'Rich Expenditure - $\nu_2$')
    ax.set_ylabel('utility')
    ax.set_title('Utility for rich')
    plt.show()




def plot_rich_utility_over_np(N,mu,y1,y2,a,d,G,df_generator_func=generate_nplayer_unif_df):
    y2s  = np.linspace(0,y2,100)
    fig, ax = plt.subplots()
    
    #nparr=[(2,3),(20,30),(40,60)]
    nparr=[(1,2),(2,3)]
    
    plothandles=[]
    for p,n in nparr:
        df = df_generator_func(N=N,numpoor =p,numtotal=n)
        l, = ax.plot(y2s, [utility_rich(x=t,p=find_rich_probability_np(numpoor=p, numtotal=n,nu1=y1*.2,mu=mu, nu2=t,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y2s])
        plothandles.append(l)
        
    ax.legend(tuple(plothandles), tuple(r'$N$='+str(n)+ " $P$="+str(p) for p,n in nparr), loc='lower left', shadow=True)
    ax.set_xlabel(r'Rich Expenditure - $\nu_2$')
    ax.set_ylabel('rich-utility')
    ax.set_title('Utility for Rich')
    plt.show()
    

def plot_rich_probability_over_np(N,mu,y1,y2,df_generator_func = generate_nplayer_unif_df):
    y2s  = np.linspace(0,y2,100)
    fig, ax = plt.subplots()
    
    
    nparr=[(2,3),(4,6),(6,9)]
    #nparr=[(2,3),(4,6),(20,30),(40,60)]
    
    plothandles=[]
    for p,n in nparr:
        df = df_generator_func(N=N,numpoor =p,numtotal=n)
        l, = ax.plot(y2s, [find_rich_probability_np(numpoor=p, numtotal=n,nu2=t,mu=mu, nu1=y1*.2,df=df) for t in y2s])
        plothandles.append(l)
        
    ax.legend(tuple(plothandles), tuple(r'$N$='+str(n)+ " $P$="+str(p) for p,n in nparr), loc='upper right', shadow=True)
    ax.set_xlabel(r'expenditure')
    ax.set_ylabel('win probability')
    ax.set_title('Rich Winning Probability')
    plt.show()
    
def plot_poor_probability_over_np(N,mu,y1,y2,df_generator_func=generate_nplayer_unif_df):
    y1s  = np.linspace(0,y1,100)
    fig, ax = plt.subplots()
    
    
    nparr=[(2,3),(4,6),(6,9)]
    #nparr=[(2,3),(4,6),(20,30),(40,60)]
    
    plothandles=[]
    for p,n in nparr:
        df = df_generator_func(N=N,numpoor =p,numtotal=n)
        l, = ax.plot(y1s, [find_poor_probability_np(numpoor=p, numtotal=n,nu1=t,mu=mu, nu2=y2*.2,df=df) for t in y1s])
        plothandles.append(l)
        
    ax.legend(tuple(plothandles), tuple(r'$N$='+str(n)+ " $P$="+str(p) for p,n in nparr), loc='upper left', shadow=True)
    ax.set_xlabel(r'expenditure')
    ax.set_ylabel('win probability')
    ax.set_title('Poor Winning Probability')
    plt.show()


def plot_poor_utility_over_np(N,mu,y1,y2,a,d,G,df_generator_func=generate_nplayer_unif_df):
    y1s  = np.linspace(0,y1,100)
    fig, ax = plt.subplots()
    
    
    #nparr=[(2,3),(4,6),(6,9)]
    nparr=[(1,2),(2,3),(4,6),(20,30)]
    
    plothandles=[]
    for p,n in nparr:
        df = df_generator_func(N=N,numpoor =p,numtotal=n)
        l, = ax.plot(y1s, [utility_poor(x=t,p=find_poor_probability_np(numpoor=p, numtotal=n,nu1=t,mu=mu, nu2=y2*.2,df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y1s])
        plothandles.append(l)
        
    ax.legend(tuple(plothandles), tuple(r'$N$='+str(n)+ " $P$="+str(p) for p,n in nparr), loc='lower left', shadow=True)
    ax.set_xlabel(r'Poor Expenditure - $\nu_1$')
    ax.set_ylabel('poor-utility')
    ax.set_title('Utility for poor')
    plt.show()
    

def plot_poor_utility(df,mu,y1,y2,a,d,G,poor_prob_func):
    if not poor_prob_func:
        poor_prob_func = find_poor_probability
    y1s  = np.linspace(0,y1,100)
    fig, ax = plt.subplots()
    y2arr=[t*y2 for t in (.1,.3,.6,.9,)]
    l1, = ax.plot(y1s, [utility_poor(x=t,p=poor_prob_func(nu1=t,mu=mu, nu2=y2arr[0],df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y1s])
    l2, = ax.plot(y1s, [utility_poor(x=t,p=poor_prob_func(nu1=t,mu=mu, nu2=y2arr[1],df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y1s])
    l3, = ax.plot(y1s, [utility_poor(x=t,p=poor_prob_func(nu1=t,mu=mu, nu2=y2arr[2],df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y1s])
    l4, = ax.plot(y1s, [utility_poor(x=t,p=poor_prob_func(nu1=t,mu=mu, nu2=y2arr[3],df=df),y1=y1,y2=y2,a=a,d=d,G=G) for t in y1s])
    
    ax.legend((l1, l2, l3,l4), tuple(r'$\nu_2$='+str(y) for y in y2arr), loc='lower left', shadow=True)
    ax.set_xlabel(r'Poor Expenditure - $\nu_1$')
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
    
    mus = [.2,.49]
    
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
    

def generate_threeplayer_df (N):
    
    r11=np.random.uniform(0,1,N)
    s11=np.random.uniform(0,1,N)
    
    r12=np.random.uniform(0,1,N)
    s12=np.random.uniform(0,1,N)
    
    r2=np.random.uniform(0,1,N)
    s2=np.random.uniform(0,1,N)
    
    
    df = pd.DataFrame({'r11':r11,'s11':s11,'r12':r12,'s12':s12,'r2':r2,'s2':s2})
    return df


def generate_nplayer_norm_df (N,numpoor,numtotal):
    print("Using normal random variables with for N=%f , P=%f" % (numtotal,numpoor))
    if numtotal <= numpoor:
        raise ValueError("numpoor must be less than or equal to numtotal")
        
    poor_columns_r = ["r1"+str(i+1) for i in range(0,numpoor)]
    poor_columns_s = ["s1"+str(i+1) for i in range(0,numpoor)]   
    
    
    rich_columns_r = ["r2"+str(i+1) for i in range(0,numtotal-numpoor)]
    rich_columns_s = ["s2"+str(i+1) for i in range(0,numtotal-numpoor)]
    
    all_cols = poor_columns_r + poor_columns_s + rich_columns_r  + rich_columns_s
   
    df = pd.DataFrame(dict ( (colname,np.random.normal(0,2,N)) for colname in all_cols) )
    return df


def run_n_player_sim():
    N  = 100000
    #mu = .2
    y1=10
    y2=100
    a = .2
    G=10
    d=1.1

    
    
    #df = generate_nplayer_df(N=N,numpoor =2,numtotal=3)
    #df=generate_nplayer_norm_df(1000,2,3)
    #plot_poor_utility_over_np(mu=.2, y1=y1, y2=y2, a=a, d=d, G=G,N=N)
    #plot_poor_probability_over_np(N=N,mu=.2,y1=y1,y2=y2)#,   df_generator_func = generate_nplayer_norm_df)
    plot_rich_probability_over_np(N=N,mu=.2,y1=y1,y2=y2)
    #plot_rich_utility_over_np(mu=.2, y1=y1, y2=y2, a=a, d=d, G=G,N=N)
    #find_poor_probability_np(df=df,nu1=2,nu2=20,mu=.2, numpoor=2,numtotal=3)
    
    
    
def run_three_player_sim():
    N  = 100000
    #mu = .2
    y1=10
    y2=100
    a = .2
    G=10
    d=1.1
    
    #df = generate_threeplayer_df(N)
    
    #plot_rich_utility(mu=.2,df=df, y1=y1, y2=y2, a=a, d=d, G=G)
    #plot_poor_utility(df=df,mu=.2, y1=y1, y2=y2, a=a, d=d, G=G)
    #plot_common_utility_over_nu2(df=df, y1=y1, y2=y2, a=a, d=d, G=G
    #plot_common_utility_vs_inequality(df=df,G=G,a=a,d=d)
    #plot_common_utility_vs_inequality_over_mus(df=df,G=G,a=a,d=d)
    
    #find_poor_probability_np(df=df,nu1=2,nu2=20,mu=.2, numpoor=2,numtotal=3)
    print("DONE")
    
    
    
if __name__ == "__main__":
    #run_three_player_sim()
    run_n_player_sim()
