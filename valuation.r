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
  nfiles=length(files);
  tax=.3;
  # TODO: use timeseries dictionaries for storing roes, roic etc.
  
  for ( file in files){
    print(file)
    data=read.csv(file);
    dvec=as.integer(strftime(strptime(data$date,"%Y-%m-%d"),"%Y"));
    
    tsshares=ts(data=data.frame(shares=data$shares),
                start=c(dvec[1],1),end=c(dvec[length(dvec)]));
    tsbve=ts(data=data.frame(bve=data$bve),
             start=c(dvec[1],1),end=c(dvec[length(dvec)]));
    tsebit=ts(data=data.frame(ebit=data$ebit),
              start=c(dvec[1],1),end=c(dvec[length(dvec)]));
    tsltdebt=ts(data=data.frame(ltdebt=data$ltdebt),
                start=c(dvec[1],1),end=c(dvec[length(dvec)]));
    
    require(zoo);
    tsbve_avg=(lag(tsbve,-1)+(tsbve))/2;
    tsltdebt_average=(lag(tsltdebt,-1)+(tsltdebt))/2;
    den=tsltdebt_average+tsbve_avg;
    den=ts.intersect(den,tsebit);
    roic=ts(data=data.frame(roic=as.data.frame(den)$tsebit)/as.data.frame(den)$den,
            start=start(den),end=end(den),
            frequency=frequency(den));
    
    roe=tsebit/lag(tsebit,-1)-1;
    
    phase=3;
    tsroec=roe
    for (lindex in seq(phase-1)) {
      tsroec=ts.intersect(tsroec,lag(roe,-lindex));
    }
    
    return(tsroec)
    roes=projections(dvec=roe,phase=phase);
    # evaluate WACC at every stage
    print(paste("(g):",toString(roes)));
    print(paste("(roic):",toString(roic)));
    #print(paste("g/roic:",toString(roes/roic)));
    
  }
}
