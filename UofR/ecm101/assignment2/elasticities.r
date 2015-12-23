require(plyr)#for ddply


#source('elasticities.r');
#dat=read_diary('../../../consumption/UKDA-4071-tab/tab/view114.tab')

read_ages<-function(set41_filename)
{
  ages  = read.table(set41_filename);
  ages = ages [ as.character(ages$V1) !='caseno',];
  ages = data.frame(caseno=ages$V1,persno=ages$V2,age=as.numeric(ages$V5));
  return(ages);
  
}

analyze <- function(diary_csv_filename,set41_filename){
  ages = read_ages(set41_filename);
  diary = read.csv(diary_csv_filename);
  return (merge(diary,ages));
  
}

write_diary <- function(set114_filename,csv_filename){
  
  wdiary = read.table(set114_filename);
  dat = wdiary[as.character(wdiary$V1)!="caseno",];
  dat = data.frame(caseno=dat$V1,
                   persno=dat$V2,
                   expwk114=dat$V3,
                   ditemcod=dat$V4,
                   dqualif=dat$V5,
                   dcodecnt=dat$V6,
                   diteamt=dat$V7);

  # taking week 1 data
  dat=dat[as.character(dat$expwk114)=="1",];
  
  dat2 = ddply(.data=dat,.variables=.(caseno,persno,expwk114),summarize,
               expenditure=sum(as.numeric(as.character(diteamt))));
  write.csv(x=dat2,file=csv_filename);
  return(dat2);
}

