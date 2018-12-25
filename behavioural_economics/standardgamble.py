

from pprint import pprint,pformat
import numpy as np
#C:\Users\anuragr\Documents\UofR\2019SemI\self.ecm191\code>python
	
lottery = [ (10,.01), (20,.99)]

def check_lottery_validity(lottery,tol=1e-2	):
	ps = [ p for _,p in lottery]
	if any(x<0 for x in ps):
		raise ValueError("Cannot have negative probabilities")
	if abs(sum(ps) - 1)>tol:
		raise ValueError("Cannot have sum of probabilities higher (or lower) than 1 (lottery=%s with sum(ps)=%s)" % (lottery, sum(ps)))

def generate_lotteries_from_p_vec (x_l,x_h,vec_p,prec):
	if x_l >= x_h:
		raise ValueError("Cannot have x_l >= x_h")
	return [ [(x_l,round(p,prec)),(x_h,round(1-p,prec))] for p in vec_p]


def cpt(pvec_pos,pvec_neg,wfunc_pos,wfunc_neg):
	
	if any( not callable(x) for x in [wfunc_pos,wfunc_neg]):
		raise ValueError("wfunc must be a function")
	
	pivec_pos = [ wfunc_pos ( sum (pvec_pos[i:]) ) - wfunc_pos(sum(pvec_pos[i+1:])) for i in range(0,len(pvec_pos))]
	print("===")
	pivec_neg = [ wfunc_neg ( sum (pvec_neg[0:i+1]) ) - wfunc_neg(sum(pvec_neg[0:i])) for i in range(0,len(pvec_neg))]

	return pivec_neg


	
def w (x):
	print "w(x=%s)" % x
	return x

if __name__== "__main__":
	N=10
	prec = 3
	vec_p = [round(k/float(N),prec) for k in range(1,N+1)]
	lotteries = (generate_lotteries_from_p_vec(x_l=10,x_h=20,vec_p=vec_p,prec=prec)) 

	[check_lottery_validity(l) for l in  lotteries]
	#pprint(lotteries)
	pvec_pos = [1./2,1./6,1./6,1./6]
	pvec_neg = [1./6,1./6,1./6,1./2]
	print(pvec_neg)
	print(cpt(pvec_pos=pvec_pos,pvec_neg=pvec_neg,wfunc_pos	=w,wfunc_neg=w))
