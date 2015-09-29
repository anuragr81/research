
# need percentage of people in a certain age-group etc.
snapshot <- function (filename,field_code,year_code_regex)
{
  
  dat=read.csv(filename)
  #dat=read.csv('Data_Extract_From_World_Development_Indicators_ALL.csv');
  
  # search a particular field
  xx=dat[dat$Series.Code==field_code,]
  
  #xx=dat[dat$Series.Code=="SP.POP.TOTL",]
  result = grep(year_code_regex,names(xx),value=TRUE);
  if (length(result)==0){
    stop('Invalid year_code_regex')
  }
  
  x<-as.character(xx$Country.Name)
  y<-as.numeric(as.character(xx[,result])) # area in 2000
  barplot(y,names.arg=x,xlab=result)
  return(y);
}

