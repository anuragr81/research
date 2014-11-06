average_data<-function(dvec){
  d_next=lag(zoo(dvec),1);
  d_cur=lag(zoo(dvec),0);
  davg=(d_next+d_cur)/2;
  return(davg);
}

rochange<-function(dvec){
  d_next=lag(zoo(dvec),1);
  d_cur=lag(zoo(dvec),0);
  roch=d_next/d_cur-1;
  return(roch);
}

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
                    ebit=data$ebit,ltdebt=data$ltdebt);
    require(zoo);
    bve_average=average_data(data$bve);
    ltdebt_average=average_data(data$ltdebt);
    roe=rochange(data$ebit);
    
    print(paste("bve_average:",toString(bve_average)));
    print(paste("ltdebt:",toString(ltdebt_average)));
    print(paste("roe:",toString(roe)));
    
    
    dates=(coredata(lag(zoo(data$date),1)));
    phase=3;
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
