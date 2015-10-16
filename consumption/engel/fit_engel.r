# In UK expenditure survey
# p51(set43) -> p344(set3)
# set114

# for personal incomes:
# deselecting retired and unoccupied
# dat[as.numeric(dat$V4)<20,]

require(plyr)#for ddply
library(foreign)# for spss

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
  
  results=data.frame();
  count = 1;
  for (caseno in cases){  
    
    income = as.numeric(as.character(
      winc[as.character(winc$V1)==as.character(caseno),]$V8)
    );
    
    
    if (!is.na(income)){
      print(paste("Weekly income for caseno",caseno," = ",income));
      
      # SKIP: 8074031
      
      # gathering data for week 1
      case_entries=wdiary[as.character(wdiary$V1)==as.character(caseno) & as.character(wdiary$V3)=="1",];
      
      # from case_entries, sum data for every category (use plyr -ddply)
      # the idea would be to i) append the super-category to the frame case_entries 
      
      case_entries$category = calculateSuperCategory(case_entries$V4);
      case_entries$income = rep(income,length(case_entries$V1));
      case_entries$total_expenditure = sum(as.numeric(as.character(case_entries$V7)));
      # and then ii) use ddply to aggregate
      dat = ddply(.data=case_entries,.variables=.(V1,V2,V3,category),summarize,
                  expenditure=sum(as.numeric(as.character(V7))),
                  income=unique(income), # guarantees that multiple income values would fail
                  total_expenditure = unique(total_expenditure) # guarantees that multiple income values would fail
      );
      results= rbind(results,dat);
      #print(results)
      #stop("DONE");
    } #endif na check on income 
  }#end for
  write.csv(results,outfile)
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
  logx = log(dat[dat$income!=0,]$income);
  w_i = dat[dat$income!=0,]$expenditure;
  res = lm(w_i~logx);
  plot(logx,w_i);
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
    return(dat3[!is.na(as.numeric(dat3$hh_k04)),]);
}

