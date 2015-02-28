

# Calculates regression coefficients for a sequence of size(dm)
projection<-function(dvec){
  dvec=as.numeric(dvec);
  phase=length(dvec);
  #print(paste("seq(phase)=",toString(seq(phase)),"dev=",toString(dvec)));
  coefficients=coef(lm(dvec~seq(phase)));
  intercept=coefficients[1];
  slope=coefficients[2];
  projection=as.double(slope*(phase+1)+intercept);
  return(projection);
  #y=(c(coredata(dvec[(i-phase+1):i]),roe_projection));plot(seq(phase+1),y);readline();
}


# searches Market Price for every date (in immediate proximity)
# from a vector of market-prices
searchMarketPrice<-function (mdates,pricedata){
  stringmdates=as.character(mdates);
  mdprices=NULL
  for (stringmdate in stringmdates){
    found_price=NA;
    for (datediff in -seq(0,3)){
      mdate=strptime(stringmdate,"%Y-%m-%d");
      search_date = mdate +as.difftime(datediff,units="days");
      #print(paste("Searching for",search_date," in mdprices"));
      res = pricedata[strftime(pricedata$Date,"%Y-%m-%d") == strftime(search_date,"%Y-%m-%d"),];
      nrows=dim(res)[1]
      if (nrows>0){
        px_bid=as.double(as.character(res$PX_BID))
        px_ask=as.double(as.character(res$PX_ASK))
        found_price=mean(c(px_bid,px_ask));        
        #print (paste("found_price=",found_price))
        break;
      }
    }
    mdprices=c(mdprices,found_price);
  }
  return (data.frame("Date"=mdates,"MidPrice"=mdprices))
}

calculateROE<- function(){
  
  mapFileToTicker=read.csv("mapFileToTicker.csv");
  mapTickerToMDFile=read.csv("marketdatafilenames.csv");
  
  # WACC needs
  files=dir()[grep("Analog.*output.csv$",dir())];
  nfiles=length(files);
  tax=.3;
  # TODO: use timeseries dictionaries for storing roes, roic etc.
  
  for ( file in files){
    
    ticker=toString(mapFileToTicker[mapFileToTicker$filename==file,]$ticker);
    marketdatafile=toString(mapTickerToMDFile[mapTickerToMDFile$ticker==ticker,]$filename);
    #    print(paste("File:",file,"Ticker:",ticker));
    data=read.csv(file);
    pricedata=read.csv(paste("allequities/",marketdatafile,sep=""));
    pricedata=data.frame(Date=strptime(pricedata$Date,"%Y-%m-%d"),
                         PX_BID=pricedata$PX_BID,
                         PX_ASK=pricedata$PX_ASK);
    
    #return(pricedata)
    mdates=strptime(data$date,"%Y-%m-%d")
    
    # dev would be used as the time-vector for all data-timeseries
    dvec=as.integer(strftime(strptime(data$date,"%Y-%m-%d"),"%Y"));
    marketprices=searchMarketPrice(mdates=mdates,pricedata=pricedata)
    myears=as.integer(strftime(marketprices$Date,"%Y"));
    if (any(abs(myears-dvec)>0)){
      stop("Invalid Years-array in marketprices")
    }
    marketprices=ts(data=marketprices$MidPrice,start=head(myears,1),end=tail(myears,1))

    tsshares=ts(data=data.frame(shares=data$shares),
                start=c(dvec[1],1),end=c(dvec[length(dvec)]));
    tsbve=ts(data=data.frame(bve=data$bve),
             start=c(dvec[1],1),end=c(dvec[length(dvec)]));
    tsebit=ts(data=data.frame(ebit=data$ebit),
              start=c(dvec[1],1),end=c(dvec[length(dvec)]));
    tsltdebt=ts(data=data.frame(ltdebt=data$ltdebt),
                start=c(dvec[1],1),end=c(dvec[length(dvec)]));
    tsfcff=ts(data=data.frame(fcff=data$fcff),
                start=c(dvec[1],1),end=c(dvec[length(dvec)]));
    
    require(zoo);
    tsbve_avg=(lag(tsbve,-1)+(tsbve))/2;
    tsltdebt_average=(lag(tsltdebt,-1)+(tsltdebt))/2;
    den=tsltdebt_average+tsbve_avg;
    den=ts.intersect(den,tsebit);
    tsroic=ts(data=data.frame(roic=as.data.frame(den)$tsebit)/as.data.frame(den)$den,
            start=start(den),end=end(den),
            frequency=frequency(den));
    
    roe=tsebit/lag(tsebit,-1)-1;
    phase=3;
    tsroec=roe
    for (lindex in seq(phase-1)) {
      tsroec=ts.intersect(tsroec,lag(roe,-lindex));
    }
    tsroec_data=as.matrix(as.data.frame(tsroec));
    projs=array();
    
    for (index in seq(dim(tsroec_data)[1])){
      projs[index]=projection(dvec=rev(tsroec_data[index,]));
    }
    
    tsprojs=ts(data=projs,start=start(tsroec),end=end(tsroec),frequency=frequency(tsroec));
    rd = read.csv("debtcost/rd10.csv");
    # assumes the data column is "rd10"
    rd = ts(data=rd$rd10,
           start=rd$Year[1],end=rd$Year[length(rd$Year)]);
    # MVE->WACC
    #roe=filterTimeSeries(roe,rd$Year);    
    
    ts_final = ts.intersect(shares=tsshares,ltdebt=tsltdebt,midprice=marketprices,
                                rate=rd,fcff=tsfcff,roic=tsroic,roe=roe);

    findata =  as.data.frame(ts_final);
    E     = findata$shares*findata$midprice;
    D     = findata$ltdebt;
    V     = E+D;
    r_D   = findata$rate;
    r_E   = findata$roe;
    FCFF  = findata$fcff;
    WACC = (1-tax)*(D/V)*r_D+(E/V)*r_E;
    print(WACC);
    #V=(NOPLAT_{t+1}(1−gNOPLAT/RONIC))/(WACC−gNOPLAT)
  } # end for loop
}
