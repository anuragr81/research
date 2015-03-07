

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


processBidAskRow<- function(res){
        px_bid=as.double(as.character(res$PX_BID))
        px_ask=as.double(as.character(res$PX_ASK))
        return (mean(c(px_bid,px_ask)));
}

processOpenRow<-function(res) {
        return (as.double(as.character(res$Open)));
}

loadSNPData <- function(dirname,filename){
    pricedata =read.csv(paste(dirname,filename,sep=""));
    pricedata=data.frame(Date=strptime(pricedata$Date,"%d/%m/%Y"),
                         Open=pricedata$Open);
    return (pricedata);
}

# searches Market Price for every date (in immediate proximity)
# from a vector of market-prices
searchMarketPrice<-function (mdates,ndaysrange,pricedata,processorFunc){
  stringmdates=as.character(mdates);
  mdprices=NULL
  for (stringmdate in stringmdates){
    found_price=NA;
    for (datediff in -seq(0,ndaysrange)){
      mdate=strptime(stringmdate,"%Y-%m-%d");
      search_date = mdate +as.difftime(datediff,units="days");
      #print(paste("Searching for",search_date," in mdprices"));
      res = pricedata[strftime(pricedata$Date,"%Y-%m-%d") == strftime(search_date,"%Y-%m-%d"),];
      nrows=dim(res)[1]
      if (nrows>0){
        #print (paste("found_price=",found_price))
        #found_price=processBidAskRow(res);
        found_price=processorFunc(res);
        #found_price=processHighLowRow(res);
        break;
      }
    }
    mdprices=c(mdprices,found_price);
  }
  return (data.frame("Date"=mdates,price=mdprices))
}

calculateROE<- function(){
  
  mapFileToTicker=read.csv("mapFileToTicker.csv");
  mapTickerToMDFile=read.csv("marketdatafilenames.csv");
  
  # WACC needs
  files=dir()[grep("*output.csv$",dir())];
  
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

    snpprices = loadSNPData("snp/","snp.csv");

    mdates=strptime(data$date,"%Y-%m-%d")
    
    # dev would be used as the time-vector for all data-timeseries
    dvec=as.integer(strftime(strptime(data$date,"%Y-%m-%d"),"%Y"));
    #marketprices=searchMarketPrice(mdates=mdates,pricedata=pricedata)
    marketprices=searchMarketPrice(mdates=mdates,ndaysrange=10,pricedata=pricedata,processorFunc=processBidAskRow);

    snpprices=searchMarketPrice(mdates=mdates,ndaysrange=40,pricedata=snpprices,processorFunc=processOpenRow);

    myears=as.integer(strftime(marketprices$Date,"%Y"));
    if (any(abs(myears-dvec)>0)){
      stop("Invalid Years-array in marketprices")
    }
    marketprices=ts(data=marketprices$price,start=head(myears,1),end=tail(myears,1))

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
    phase=5;
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
    rd = read.csv("debtcost/rd1.csv");
    # assumes the data column is "rd1"
    rd = ts(data=rd$rd1,
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
    tswacc=ts(data=WACC,start=start(ts_final),end=end(ts_final));
    tswf = ts.intersect(tswacc,ts_final);
    plot(tswf);
    tswf=1;
    #V=(NOPLAT_{t+1}(1−gNOPLAT/RONIC))/(WACC−gNOPLAT)
  } # end for loop
}
