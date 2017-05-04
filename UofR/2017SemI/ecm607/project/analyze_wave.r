library(foreign)
require(plyr)

setwd('C:/Users/anuragr/Documents/UofR/2017SemI/ecm607/project')

test_factors<-function(N){
  m=list()
  m[[1]]="One"
  m[[2]]="Two"
  m[[3]]="Three"
  
  randomIndices <- floor(3*runif(N,0,1))+1
  randomStrings <-unlist(m[randomIndices])
  a = data.frame(x=randomStrings,y=rnorm(N),z=runif(N),xf=as.factor(randomStrings))
  
  return(a)
}

load_data<-function () { 
  dat <- read.dta('US_data6Wv12.dta')
  return(dat)
}

p_pers<-function(dat,valO,valC,valE,valA,valN){
  if (!is.vector(valO)){
    valO<-c(valO)
  }
  if (!is.vector(valC)){
    valC<-c(valC)
  }
  if (!is.vector(valE)){
    valE<-c(valE)
  }
  if (!is.vector(valA)){
    valA<-c(valA)
  }
  if (!is.vector(valN)){
    valN<-c(valN)
  }
  res <- dat[is.element(dat$o,valO) & is.element(dat$c,valC) & is.element(dat$e,valE) & is.element(dat$a,valA) & is.element(dat$n,valN),]
  
  return(dim(res)[1]/dim(dat)[1])
}

all_personality_types<-function(d){
  if (any((sort(intersect(colnames(d),c("o","c","e","a","n")) )!=c("a","c","e","n","o")))) {
    stop("data.frame does not have OCEAN columns")
  }
  s<-expand.grid(o=unique(d$o),c=unique(d$c),e=unique(d$e),a=unique(d$a),n=unique(d$n))
  s$ocean<-paste(s$o,s$c,s$e,s$a,s$n)
  d$ocean<-paste(d$o,d$c,d$e,d$a,d$n)
  skipData<-d[d$o<=-8 &d$c<=-8 &d$e<=-8 &d$a<=-8 &d$n<=-8 , ]
  print(paste("skipping",dim(skipData)[1]," rows"))
  d<-d[!is.element(d$pidp,skipData$pidp),]
  p <- data.frame(pidp=d$pidp,ocean=d$ocean)
  sp<-merge(s,p)
  k<-ddply(sp,.(ocean),summarize,n=length(pidp))
  k$p<-k$n/dim(k)[1]
  k<-k[k$n>10,]
  return (k)
  
}



feedback_test <-function(N) {
  if (missing(N)){
    N<-10
  }
  #x<-2^seq(0,N) 
  x = array();
  x[1]=1
  
  for (i in seq(1,N-1)){
    x[i+1]=x[i]+2
  }
  
  y =array()
  y[1]=1
  for (i in seq(1,N-1)){
    y[i+1]=y[i]/2.0+x[i]
  }
  return(data.frame(x=x,y=y))
}
between<-function(elem,r){
  if (!is.vector(r) || length(r)!=2){
    stop("r must be a vector of size 2")
  }
  return (elem<=r[2] & elem>=r[1])
}


job_matrix <-function(d){
  r<-ddply(d,.(jbsoc00_cc),summarize,num=length(pidp),median_income=quantile(paygu_dv,.8),
           median_openness=quantile(o,.9),
           median_conscientiousness=quantile(c,.9),
           median_extraversion=quantile(e,.9),
           median_agreeableness=quantile(a,.9),
           median_neuroticism=quantile(n,.9))
  r2<-read.csv('jobs.csv')
  j<-merge(r,r2,by=c("jbsoc00_cc","num"))
  return(j)
}

select_sector<-function(sector,varname,replacewith) {
  r<-read.csv('jobs.csv')
  jobs <- r[as.character(r$sector)==sector & !is.na(r$sector),]$jbsoc00_cc
  jobs<-paste(rep("\"",length(jobs)),jobs,rep("\"",length(jobs)),sep="")
  argsList<-as.list(as.character(jobs))
  argsList[["sep"]]=paste("|| ",varname,"==")
  l<-do.call("paste",argsList);
  if (missing(replacewith)){
    res<-paste("generate byte chosen_job_type=1 if(",varname,"==",l,") ; replace chosen_job_type=0 if chosen_job_type!=1")
  } else {
    res<-paste("replace chosen_job_type=",replacewith," if(",varname,"==",l,")")
  }
  return(res)
}

generate_all_sector_getters<-function(fname){
  r<-read.csv('jobs.csv');
  outstr=array()
  i=1
  sectors<-unique(as.character(r$sector));
  for (sector in sectors){
    if (!is.na(sector) && sector!="NA") {
      outstr[i]<-select_sector(sector=sector,varname="job_type",replacewith =paste("\"",as.character(sector),"\"",sep=""))
      i<-i+1
    } # endif
    
  }#end for
  write(fname,x=outstr)
}

select_sector_from_data<-function(d,r,sector){
  jobs <- as.character(r[r$sector==sector & !is.na(r$sector),]$jbsoc00_cc)
  return (d[is.element(d$jbsoc00_cc,jobs),])
}

sector_matrix <-function(d){d
  r2<-read.csv('jobs.csv')
  r2<-data.frame(jbsoc00_cc=r2$jbsoc00_cc,sector=r2$sector,rank=r2$rank)
  j<-merge(d,r2)
  j<-ddply(j,.(sector),summarize,num=length(pidp),median_income=quantile(paygu_dv,.5),
           median_openness=quantile(o,.8),
           median_conscientiousness=quantile(c,.8),
           median_extraversion=quantile(e,.8),
           median_agreeableness=quantile(a,.8),
           median_neuroticism=quantile(n,.8))
  
  return(j)
}

kmeans_centers <-function(d){
  dc <- kmeans(d[,c("o","c","e","a","n")], 10, nstart = 20)
  return(dc)
}

sorted_area_codes<- function(d){
  k<-ddply(d,.(gor_dv),summarize,median_income=median(paygu_dv))
  m<-data.frame(gor_dv=c(1,2,3,4,5,6,7,8,9,10,11,12),name=c("ne","nw",'yrkshr','emid','wmid','eo','ldn','sea','swe','wls','sct','nrth'))
  j<- merge(k,m)
  n<-j[order(j$median_income),]
  n$gor_rank<-seq(dim(n)[1])
  #write(x=paste("replace gor_rank=",j$gor_rank,"if gor_dv==",j$gor_dv),'c:/temp/t.txt')
  return(n)
}

sorted_education_codes <-function(){
  k<-data.frame(hiqual_dv = c(1,2,3,4,5,9), educ_level = c(4,5,3,2,1,0), name = c("degree", "other higher","A level", "GCSE etc.", "other qual", "No qual"))
  j<-k[order(k$educ_level),]
  #write(x=paste("replace educ_level=",j$educ_level,"if hiqual_dv==",j$hiqual_dv),'c:/temp/t.txt')
  
  return(j)
}

sorted_race_codes <- function(d){
  k<-ddply(d,.(racel_dv),summarize,population=length(pidp))
  j<-k[order(k$population,decreasing = TRUE),]
  j$race_rank<-seq(dim(j)[1])
  #write(x=paste("replace race_rank=",j$race_rank," if racel_dv==\"",j$racel_dv,"\"",sep=""),'c:/temp/t.txt')
  
  return(j)
}

run_test<-function(dat) {
  d<-dat[dat$wave==3,]
  
  d$o<-as.integer(as.character(d$big5o_dv))
  d$c<-as.integer(as.character(d$big5c_dv))
  d$e<-as.integer(as.character(d$big5e_dv))
  d$a<-as.integer(as.character(d$big5a_dv))
  d$n<-as.integer(as.character(d$big5n_dv))
  
  
  d<-(d[d$o>-8 & d$c>-8 & d$e>-8 & d$a>-8 & d$n>-8,])
  d<-(d[(as.character(d$jbterm1)=="a permanent job"),])
  d<-d[d$paygu_dv!=-8,]
  d$interviewdate<-as.Date(paste(d$istrtdaty,sprintf("%02d",d$istrtdatm),sprintf("%02d",d$istrtdatd),sep="-"))
  return(d)
  View(ddply(d,.(racel_dv),summarize,o=median(o,.8),c=median(e,.8),e=median(e,.8),a=median(a,.8),n=median(n,.8)))
  
  print ( paste("openness range : ",toString(range(d$o)),"mean=",mean(d$o), "sd=",sd(d$o)))
  print (paste("conscientiousness range : ",toString(range(d$c)),"mean=",mean(d$c), "sd=",sd(d$c)))
  print (paste("extraversion range : ",toString(range(d$e)),"mean=",mean(d$e), "sd=",sd(d$e)))
  print (paste("agreeability range : ",toString(range(d$a)),"mean=",mean(d$a), "sd=",sd(d$a)))
  print (paste("neuroticism range : ",toString(range(d$n)),"mean=",mean(d$n), "sd=",sd(d$n)))
  
  quantile(d$o,probs=seq(0,1,.1))
  quantile(d$c,probs=seq(0,1,.1))
  quantile(d$e,probs=seq(0,1,.1))
  quantile(d$a,probs=seq(0,1,.1))
  quantile(d$n,probs=seq(0,1,.1))
  
  d$ocean<-paste(d$o,d$c,d$e,d$a,d$n)
  
  # only people with a permanent job need be considered
  job<-d[as.character(d$jbterm1)=="a permanent job",]
  nonPerm<-d[as.character(d$jbterm1)!="a permanent job",]
  print(paste(dim(nonPerm)[1]*100/dim(d)[1],"%(",dim(nonPerm)[1],"out of ",dim(d)[1] ,") entries were ignored for the jobs not being permanent"))
  #unique((d[d$o==-9 & d$c==-9 & d$e == -9 & d$a == -9 & d$n == -9,])$jbnssec_dv)
  
  #s<-(d[is.element(d$o,c(4)) & between(d$c,c(-9,5)) & between(d$e,c(-9,3)) & between(d$a,c(4,5)) & between(d$n,c(5,7)),])
  
  selfemp<-dat[as.character(dat$jbsemp)=="self-employed" & dat$wave==3,]
  
  # view self image
  # http://webarchive.nationalarchives.gov.uk/20160105160709/http://www.ons.gov.uk/ons/guide-method/classifications/current-standard-classifications/soc2010/soc2010-volume-3-ns-sec--rebased-on-soc2010--user-manual/index.html
  
}
# there are 60 individuals with -9 score - do we trust these tests too much?

# variables

#pidp	cross-wave person identifier (public release)
#pno	person number in household grid

#big5a_dv	Agreeableness
#big5c_dv	Conscientiousness
#big5e_dv	Extraversion
#big5n_dv	Neuroticism
#big5o_dv	Openness

#fimngrs_dv	total monthly personal income gross
#finfut	subjective financial situation - future
#finnow	subjective financial situation - current
#hhsize	number of people in household
#hidp	household identifier (public release)
#health	long-standing illness or impairment
#hiqual_dv	Highest qualification ever reported
#hsbeds	number of bedrooms
#hsownd	house owned or rented


#j2has	has a second job
#j2hrs	no. of hours worked per month, second job
#j2nssec5_dv	2nd job: NSSEC 5 categories
#j2pay	gross earnings from second jobs last month
#j2semp	employee or self employed, second job
#jb2pay_dv	pay in second job

#jbft_dv	Full or part-time employee
#jbhrs	no. of hours normally worked per week
#jbnssec_dv	Current job: NS-SEC

#jbstat - current economic activity
#jbperfp	pay includes performance related pay

#jbterm1 - permanent or temporary job
#jspl - work location
#jbsat	job satisfaction
#jbsec	job security in next 12 months
#jbsemp	employed or self-employed: current job
#jbsic07_cc	Current job: SIC 2007 (condensed 2-digit version)
#jbttwt	minutes spent travelling to work
#jstypeb	s/emp: nature of employment

#jsworkdis - commuting distance

#livesp_dv - lives with spouse in hh
#nchild_dv	Number of own children in household
#nkids_dv - number of kids in household
#paugu_dv,paunu_dv - usual gross,net pay per month
#racel_dv - race/ethnicity
#scend - school leaving age
#scghq1_dv - Subjective wellbeing
#scghq2_dv - Subjective wellbeing (GHQ): Caseness
#sclfsat1  - satisfaction with health
#sclfsat2  - satisfaction with income
#sclfsat7  - satisfaction with amount of leisure time
#sclfsato  - satisfaction with life overall
#sppid	Cross-wave person identifier of spouse
#sex_cr	sex corrected
#sf1	general health
#wave


