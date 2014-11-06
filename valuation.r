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

projections<-function(dvec,phase){
  pjctns=array();
  for (i in seq(phase,length(dvec))){
    coefficients=coef(lm(dvec[(i-phase+1):i]~seq(phase)));
    intercept=coefficients[1];
    slope=coefficients[2];
    roe_projection=as.double(slope*(phase+1)+intercept);
    pjctns[i]=roe_projection;
    #y=(c(coredata(dvec[(i-phase+1):i]),roe_projection));plot(seq(phase+1),y);readline();
  }
  return(pjctns)
}

calculateROE<- function(){
  
  # WACC needs 
  files=dir()[grep("Analog.*output.csv$",dir())];
  nfiles=length(files)
  tax=.3;
  
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
    noplat=data$ebit*(1-tax);
    roe=rochange(data$ebit);    
    
    dates=(coredata(lag(zoo(data$date),1)));
    phase=3;
    roes=projections(dvec=roe,phase=phase)
    # evaluate WACC at every stage
    print(paste("roes:",toString(roes)));
    
  }
}
