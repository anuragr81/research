

calculateROE<- function(){
  for ( file in   dir()[grep("*output.csv$",dir())]){
   data=read.csv(file);
   data=data.frame(date=strptime(data$date,"%Y-%m-%d"),shares=data$shares,ebit=data$ebit);
   require(zoo);
   ebit_next=lag(zoo(data$ebit),1)
   ebit_cur=lag(zoo(data$ebit),0)  
   roe=ebit_next/ebit_cur-1
   print(roe)
  }
}
