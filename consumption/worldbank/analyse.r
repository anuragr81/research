

tseries <- function (filename,field_code)
{

 dat=read.csv(filename)
 #dat=read.csv('Data_Extract_From_World_Development_Indicators_ALL.csv');

# search a particular field
xx=dat[dat$Series.Code==field_code,]

#xx=dat[dat$Series.Code=="SP.POP.TOTL",]

x<-as.character(xx$Country.Name)
y<-as.numeric(as.character(xx$X2000..YR2000.)) # area in 2000

barplot(y,names.arg=x)

}

