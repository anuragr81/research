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

# remember that we may be passed levels as input:category
assignCategory <-function (category_vector,price_vector){
  
  out_array=floor(as.numeric(as.character(category_vector))*1e-4);
  i=1;
  for (i in seq(length(out_array))){
    
    if (!is.na(out_array[i]) && out_array[i] == 4) {
      #print(paste("out_array[",i,"]=",out_array[i]));
      if (as.numeric(as.character(price_vector[i]))>=10) {
        out_array[i]=401;
      } else {
        out_array[i]=402;
      }
    }
  }
  print("Completed Calculation of Category")
  return(out_array);
}

calculateSuperCategory <-function (category){
  return (floor(as.numeric(as.character(category))*1e-4));
}

#      V1     V2       V3       V4      V5       V6       V7
#1 caseno persno expwk114 ditemcod dqualif dcodecnt ditemamt

#wdiary entry sells 6 yoghurts for 2.72 - 96080  624041	1	2	30203	0	6	2.72
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
  print("created income vector")
  # gathering data for week 1
  dat=wdiary[is.element(as.character(wdiary$V1),as.character(cases)) & as.character(wdiary$V3)=="1",];
  
  # from dat, sum data for every category (use plyr -ddply)
  
  #dat$category = calculateSuperCategory(dat$V4);
  
  dat$category = assignCategory(category_vector=dat$V4,price_vector=dat$V7);
  
  dat2 = ddply(.data=dat,.variables=.(V1,V2,V3,V5,V6,category),summarize,
               expenditure=sum(as.numeric(as.character(V7))));
  
  
  print("Aggregated expenditure data")
  tot_exp = ddply(.data=dat2,.variables=.(V1,V2,V3,V5,V6),summarize,total_expenditure=sum(as.numeric(as.character(expenditure))))
  print("Aggregated total_expenditure data")
  
  dat3 = merge(dat2,tot_exp)
  print("Merged total expenditure data")
  
  results = merge(dat3,incs);
  print("Merged income data")
  
  write.csv(results,outfile);
  print("Completed Writing File")
  
  return(results);
}

load_expenditure_files_agg<-function (set3_filename, set114_filename,outfile){
  winc = read.table(set3_filename);
  wdiary = read.table(set114_filename);
  winc_cases= as.character(winc$V1);
  wdiary_cases = as.character(wdiary$V1);
  cases = intersect(winc_cases,wdiary_cases);
  
  print(wdiary[1,])
  
  winc= winc[is.element(as.character(winc$V1),as.character(cases)) & as.character(winc$V1)!="caseno",];
  
  # saving income for later appends
  incs = ddply(.data=winc,.variables=.(V1),summarize,
               income=as.numeric(as.character(V8)));
  print("created income vector")
  # gathering data for week 1
  dat=wdiary[is.element(as.character(wdiary$V1),as.character(cases)) & as.character(wdiary$V3)=="1",];
  
  # from dat, sum data for every category (use plyr -ddply)
  
  #dat$category = calculateSuperCategory(dat$V4);
  
  dat$category = assignCategory(category_vector=dat$V4,price_vector=dat$V7);
  
  dat2 = ddply(.data=dat,.variables=.(V1,V3,category),summarize,
               expenditure=sum(as.numeric(as.character(V7))));
  
  
  print("Aggregated expenditure data")
  tot_exp = ddply(.data=dat2,.variables=.(V1,V3),summarize,total_expenditure=sum(as.numeric(as.character(expenditure))))
  print("Aggregated total_expenditure data")
  
  dat3 = merge(dat2,tot_exp)
  print("Merged total expenditure data")
  
  results = merge(dat3,incs);
  print("Merged income data")
  
  write.csv(results,outfile);
  print("Completed Writing File")
  
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
  x = dat[dat$income!=0,]$total_expenditure;
  #logx = log(x);
  logx=log(x);
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

fit_simple_engel<-function () {
  
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

si_factor<-function(units){
  if (!is.character(units)){
    stop("units must be of character type")
  }
  factors = array();
  i <- 1; 
  for (str in units){
    factors[i]<-1
    if (!is.na(str)) {
      if (tolower(str)=="gram" ||
            tolower(str)=="grams" ||
            tolower(str)=="g"){
        factors[i]<-1e-3;
      }
      
      if (tolower(str)=="millilitre" ||
            tolower(str)=="milliliter" ||
            tolower(str)=="ml"){
        factors[i]<-1e-6;
      }
      
      if (tolower(str)=="litre" ||
            tolower(str)=="liter" ||
            tolower(str)=="l"){
        factors[i]<-1e-3;
      }
    }
    i <- i + 1;
  }
  return(factors);
}

paramnh_regression<-function(K1_filename)
{
  K1 = read_tnz(K1_filename);
  K1 = K1[!is.na(K1$hh_k02_2),];
  dat=data.frame(y2_hhid=K1$y2_hhid,
                 itemcode=K1$itemcode,
                 lwp_unit=K1$hh_k03_1,
                 lwp=K1$hh_k03_2,
                 expense=K1$hh_k04,
                 own_unit=K1$hh_k05_1,
                 own=K1$hh_k05_2,
                 gift_unit=K1$hh_k06_1,
                 gift=K1$hh_k06_2
  );
  if (!is.numeric(dat$lwp)){
    stop("lwp must be numeric")
  }
  
  if (!is.numeric(dat$expense)){
    stop("expense must be numeric")
  }
  
  nz_lwp = dat[as.double(dat$lwp)>0,];
  lwp_units = si_factor(as.character(nz_lwp$lwp_unit));
  nz_lwp$lwp_si <- nz_lwp$lwp * lwp_units;
  nz_lwp$price <- nz_lwp$expense/nz_lwp$lwp_si;
  #x = ddply(.data=nz_lwp,.variables=.(y2_hhid),summarize,
  #             total_expenditure=sum(as.numeric(as.character(lwp))));
  
  return(nz_lwp);
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

testA<-function(){
  
  
  #"food=3", "utility2=", "alcohol=4", "housing=1","smoking=5","clothes=6","gadgets=8","equipment=7","health=9","car=10","travel=11","TV=12","entertainment=13","finance=14"
  estResult <- aidsEst( c( "food", "utility", "alcohol", "housing","smoking","clothes","gadgets","equipment","services","car","travel","TV","entertainment","finance"),
                        c( "wFood1", "wFood2", "wFood3", "wFood4" ), "xFood",
                        data = dat );
  
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

