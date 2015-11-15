# In UK expenditure survey
# p51(set43) -> p344(set3)
# set114

# for personal incomes:
# deselecting retired and unoccupied
# dat[as.numeric(dat$V4)<20,]

require(plyr)#for ddply
library(foreign)# for spss
library(micEconAids) # AIDS
# dat=read.spss('HH_SEC_O2.SAV',to.data.frame=TRUE)

calculateSuperCategory <-function (category){
  return (floor(as.numeric(as.character(category))*1e-4));
}

load_expenditure_files<-function (set3_filename, set114_filename,outfile){
  winc = read.table(set3_filename);
  wdiary = read.table(set114_filename);
  winc_cases= as.character(winc$V1);
  wdiary_cases = as.character(wdiary$V1);
  cases = intersect(winc_cases,wdiary_cases);
  
  print(wdiary[1,])
  
  #incomes = as.numeric(as.character(
  #  winc[as.character(winc$V1)==as.character(caseno),]$V8)
  #);   
  winc= winc[is.element(as.character(winc$V1),as.character(cases)) & as.character(winc$V1)!="caseno",];
  
  # saving income for later appends
  incs = ddply(.data=winc,.variables=.(V1),summarize,
               income=as.numeric(as.character(V8)));
  # gathering data for week 1
  dat=wdiary[is.element(as.character(wdiary$V1),as.character(cases)) & as.character(wdiary$V3)=="1",];
  
  # from dat, sum data for every category (use plyr -ddply)
  
  dat$category = calculateSuperCategory(dat$V4);
  dat2 = ddply(.data=dat,.variables=.(V1,V2,V3,V5,V6,category),summarize,
               expenditure=sum(as.numeric(as.character(V7))));
  
  tot_exp = ddply(.data=dat2,.variables=.(V1,V2,V3,V5,V6),summarize,total_expenditure=sum(as.numeric(as.character(expenditure))))
  dat3 = merge(dat2,tot_exp)
  results = merge(dat3,incs);
  write.csv(results,outfile);
  return(results);
}

run_regression<-function(filename,category){
  # for every caseno i, we have a vector of w_i's (i being a commodity) and outlay x
  # The equation w_i = a_i + b_i log(x)
  # would imply running k regressions such that
  # w_1[] = a_1 + b_1 log(x)
  
  # t-values are OK for 1,2,3,4,10,11,12 (but R-sq etc. are far from OK)
  
  dat_all = read.csv(filename);
  dat = dat_all[as.integer(dat_all$category) == as.integer(category),];
  print(paste("Viewed Category:",unique(dat$category)));
  logx = log(dat[dat$income!=0,]$total_expenditure);
  expenditure = dat[dat$income!=0,]$expenditure;
  total_expenditure = dat[dat$income!=0,]$total_expenditure;
  w_i = expenditure/total_expenditure;
  res = lm(w_i~logx);
  plot(logx,w_i);
  title(main=paste("Category=",toString(category)));
  abline(res);
  print(summary(res));
  return(dat);  
}
#
#require(plyr)
#dfx <- data.frame(
#  group = c(rep('A', 8), rep('B', 15), rep('C', 6)),
#  sex = sample(c("M", "F"), size = 29, replace = TRUE),
#  age = runif(n = 29, min = 18, max = 54)
#)


fit_simple_engel<-function (){
  
}

fit_engel <- function(filename){
  dat = read.csv(filename);
  categories = as.vector(unique(dat$category));
  output = list ();
  for ( category in categories ) {
    output[[category]] = 1
    category_cost = sum(dat[ dat$category == category ,]$Cost)
    print (paste("Cost of category - ",category," = ",category_cost))
  }
  #print (output);
}

read_tnz <- function(filename) {
  dat1 = read.spss(filename);
  dat2 = as.data.frame(dat1);
  dat3 = dat2[as.numeric(dat2$y2_hhid)>0,]
  return(dat3);
}

farm_workers <-function (E_filename){
  dat = read_tnz (E_filename);
  x=dat[as.character(dat$hh_e06)=='On your own farm or shamba',];
  x=x[!is.na(x$y2_hhid),]
  return(x);
}


# Uses file:K1
tnz_consumption<-function(K_filename){
  dat = read_tnz (filename=K_filename);
  dat = dat[!is.na(as.numeric(dat$hh_k04)),];
  # selecting fewer fields
  dat = data.frame(y2_hhid=dat$y2_hhid,itemcode=dat$itemcode,hh_k04=dat$hh_k04);
  # calculate total expenditure for every family with ddply
  cdat = ddply(.data=dat,.variables=.(y2_hhid),summarize,
               total_expenditure=sum(as.numeric(as.character(hh_k04))));
  res = merge(cdat,dat);
  
  boozy_families = dat[is.element(as.character(dat$itemcode),
                                  c('Wine and spirits','Bottled beer','Local brews')),];
  boozy_families$category=rep("booze",length(boozy_families$y2_hhid));
  
  
  boozy = merge(res,boozy_families)
  
  booze_expenditure = ddply(.data=boozy,.variables=.(y2_hhid,category),summarize,
                            booze_expenditure=sum(as.numeric(as.character(hh_k04))));
  
  return(merge(booze_expenditure,boozy));
}


micEconTest<-function (){
  
  data( Blanciforti86 );
  dat <- Blanciforti86[ 1:32,];
  estResult <- aidsEst( c( "pFood1", "pFood2", "pFood3", "pFood4" ),
                        c( "wFood1", "wFood2", "wFood3", "wFood4" ), "xFood",
                        data = dat );
  return(dat);
  #return(estResult);
}

