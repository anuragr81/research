

*Reading data from estimation_df(e = e, a2010=a2010,a2012= a2012, a2014 = a2014, o2010 = o2010, o2012 = o2012, o2014 = o2014, c2010=c2010, c2012=c2012, c2014=c2014) where e <- minimum_needs_cost_per_head(c2010 = c2010, c2012 = c2012, c2014 = c2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)


* for 2012 we run:
** c.ag##c.At can also be tried
eststo clear

eststo, title (asset purchases): quietly tobit dAt lt1 c.At##c.age, ll(18200) 
eststo, title (quality consumption): quietly reg nut1 c.lt1 c.At##c.age 

esttab using c:/temp/respdf.tex, mtitle no p numbers star nogaps compress title(Tobit results with asset changes \$\Delta A_t \$ as dependent variable \label{tabdAt})
** esttab using c:/temp/dAt.tex, mtitle no p numbers star nogaps compress title(Tobit results with asset changes \$\Delta A_t \$ as dependent variable \label{tabdAt})

**eststo clear
**eststo, title (quality consumption): quietly reg nut1 c.lt1 c.At##c.age 
**esttab using c:/temp/nut.tex, mtitle no p numbers star nogaps compress title(OLS results with quality consumption \$ \nu_{t+1} \$ as dependent variable \label{tabnut})

** the effect of c.lt1##c.age is weaker (even At##age is weak)
reg nut1 c.lt1 c.At##c.age 

