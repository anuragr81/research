

calculateROE<- function(){
  for ( file in   dir()[grep("*output.csv$",dir())]){
    data=read.csv(file);
    data=data.frame(date=strptime(data$date,"%Y-%m-%d"),shares=data$shares,ebit=data$ebit);
    require(zoo);
    ebit_next=lag(zoo(data$ebit),1)
    ebit_cur=lag(zoo(data$ebit),0)  
    roe=ebit_next/ebit_cur-1   
    dates=(coredata(lag(zoo(data$date),1)))
    phase=4
    for (i in seq(phase,length(roe))){
      coefficients=coef(lm(roe[(i-phase+1):i]~seq(phase)));
      intercept=coefficients[1];
      slope=coefficients[2];
      print(coefficients);
      projection=slope*(phase+1)+intercept;
      y=(c(coredata(roe[(i-phase+1):i]),projection))
      #plot(seq(phase+1),y)
    }
    
  }
}
