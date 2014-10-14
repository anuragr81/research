

calculateROE<- function(){
  
  # WACC needs 
  files=dir()[grep("Analog.*output.csv$",dir())];
  nfiles=length(files)
  
  for ( file in files){
    print(file)
    data=read.csv(file);
    data=data.frame(date=strptime(data$date,"%Y-%m-%d"),
                    shares=data$shares,
                    bve=data$bve,
                    ebit=data$ebit);
    require(zoo);
    ebit_next=lag(zoo(data$ebit),1)
    ebit_cur=lag(zoo(data$ebit),0)  
    roe=ebit_next/ebit_cur-1 
    bve_next=lag(zoo(data$bve),1)
    bve_cur=lag(zoo(data$bve),0)  
    bve_average=(bve_next+bve_cur)/2
    print(bve_average);
    return(0)
    dates=(coredata(lag(zoo(data$date),1)));
    phase=3
    roes=array();
    for (i in seq(phase,length(roe))){
      coefficients=coef(lm(roe[(i-phase+1):i]~seq(phase)));
      intercept=coefficients[1];
      slope=coefficients[2];
      roe_projection=as.double(slope*(phase+1)+intercept);
      roes[i]=roe_projection;
      #y=(c(coredata(roe[(i-phase+1):i]),roe_projection));plot(seq(phase+1),y);readline();
    }
    # evaluate WACC at every stage
  }
}
