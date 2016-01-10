require(plyr)#for ddply
require(foreign)#spss

#source('elasticities.r');
#dat=read_diary('../../../consumption/UKDA-4071-tab/tab/view114.tab')

read_ages<-function(set41_filename,position)
{
  ages  = read.table(set41_filename);
  ages = ages [ as.character(ages$V1) !='caseno',];
  if (position==5){
    ages = data.frame(caseno=ages$V1,persno=ages$V2,age=as.numeric(ages$V5));
  } else 
  {
    if (position == 6){
      ages = data.frame(caseno=ages$V1,persno=ages$V2,age=as.numeric(ages$V6));
    } else {
      stop('Invalid position for set41 file.')
    }
  }
  return(ages);
  
}

# example
# take all 32 year olds who spend 100-120 altogether
# and see how much they consume
# in Fat categories{
# 30401=Butter,
# 30402=Margarine,
# 30403=Cooking oil and fats,
# 30504=Hame and bacon, 
# 30502=Lamb,
# 30501=Beef,
# 30505=Sausages,
# 30508=Canned Meat,
# 30509=Cold Meat,
# 30510=Pies,
# 305011=Ready Meat, 
# 30512=Meat type not specified}

# c("30401","30402","30403","30504","30502","30501","30505","30508","30509","30510","305011","30512")
write_age_expenditure <- function(diary_csv_filename,
                                  set41_filename,
                                  age_expenditure_csv_filename,
                                  set41_age_position){
  ages = read_ages(set41_filename=set41_filename,position=set41_age_position);
  diary = read.csv(diary_csv_filename);
  m= (merge(diary,ages));
  write.csv(x=m,file=age_expenditure_csv_filename)
  return(m);
}

show_data<-function(m){
  lb = c(300,400);
  ub = c(400,500);
  par(mfrow=c(1,length(lb)));
  for ( i in seq(length(lb))){
    x=m[m$age<35 & m$age>30 & m$total_expenditure<ub[i] & m$total_expenditure>lb[i] & m$is_fat==TRUE,];
    plot(x$expenditure,
         xlab='HighCal Expenditure',
         xlim=c(0,50),
         x$total_expenditure-x$expenditure,
         ylab='Other Expenditure',
         ylim=c(250,500));
    title(paste(lb[i],"< Income <",ub[i]));
  }
}

analyze<-function(age_expenditure_csv_filename){
  
  dat = read.csv(age_expenditure_csv_filename);
  
  total_expenditure= data.frame(caseno=dat$caseno,
                                persno=dat$persno,
                                expwk114=dat$expwk114,
                                total_expenditure=dat$expenditure,
                                age=dat$age);
  
  dat$is_fat = is.element(as.character(dat$ditemcod),
                          c("30401","30402","30403","30504","30502","30501","30505","30508","30509","30510","305011","30512")
  );
  #return(dat);
  fat_expenditures = ddply(.data=dat,.variables=.(caseno,persno,expwk114,is_fat),summarize,
                           expenditure=sum(as.numeric(as.character(diteamt))));
  
  m=merge(total_expenditure,fat_expenditures);
  show_data(m);
  return(m);
}

write_diary <- function(set114_filename,csv_filename,has_xdeamt){
  
  wdiary = read.table(set114_filename);
  # remove the row with names
  dat = wdiary[as.character(wdiary$V1)!="caseno",];
  # choose mapping according to the flag: has_xdeamt
  if (has_xdeamt) {
    wdiary = data.frame(caseno=dat$V1,
                        persno=dat$V2,
                        expwk114=dat$V7,
                        ditemcod=dat$V5,
                        dqualif=dat$V6,
                        dcodecnt=dat$V3,
                        diteamt=dat$V4);
  } 
  else {
    wdiary = data.frame(caseno=dat$V1,
                        persno=dat$V2,
                        expwk114=dat$V3,
                        ditemcod=dat$V4,
                        dqualif=dat$V5,
                        dcodecnt=dat$V6,
                        diteamt=dat$V7);
  }
  # taking week 1 data
  wdiary_wk1=wdiary[as.character(wdiary$expwk114)=="1",];
  
  wdiary_expenditures = ddply(.data=wdiary_wk1,.variables=.(caseno,persno,expwk114),summarize,
                              expenditure=sum(as.numeric(as.character(diteamt))));
  wdiary = merge(wdiary_wk1,wdiary_expenditures);
  
  write.csv(x=wdiary,file=csv_filename);
  return(wdiary);
  
}

