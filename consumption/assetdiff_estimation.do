

*Reading data from estimation_df(e = e, a2010=a2010,a2012= a2012, a2014 = a2014, o2010 = o2010, o2012 = o2012, o2014 = o2014, c2010=c2010, c2012=c2012, c2014=c2014) where e <- minimum_needs_cost_per_head(c2010 = c2010, c2012 = c2012, c2014 = c2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)


* for 2012 we run:
** c.ag##c.At can also be tried
tobit dAt At lt1, ll(18200) 


** the effect of c.lt1##c.age is weaker (even At##age is weak)
reg nut1 c.lt1 c.At##c.age 

