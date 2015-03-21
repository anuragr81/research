

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
  mdprices=c()
  searchedDates=c()
  for (stringmdate in stringmdates){
    found_price=NA;
    search_date=c();
    for (datediff in -seq(0,ndaysrange)){
      mdate=strptime(stringmdate,"%Y-%m-%d");
      search_date = mdate +as.difftime(datediff,units="days");
      res = pricedata[strftime(pricedata$Date,"%Y-%m-%d") == strftime(search_date,"%Y-%m-%d"),];
      nrows=dim(res)[1]
      if (nrows>0){
        found_price=processorFunc(res);
        break;
      }
    }
    mdprices=c(mdprices,found_price);
    searchedDates=c(searchedDates,strftime(search_date,"%Y-%m-%d"));
  }
  searchedDates=as.Date(searchedDates);
  return (data.frame("Date"=mdates,"searchedDate"=searchedDates,price=mdprices))
}

getDateVecTimeSeries<- function(dvec,prices,dates)
{
  years=as.integer(strftime(dates,"%Y"));
  if (any(abs(years-dvec)>0)){
    stop("Invalid Years-array in marketprices")
  }
  return (ts(data=prices,start=head(years,1),end=tail(years,1)))
}


# '
# ' @description - Function looks up dates available within the provide range 
# '                from before the curdate and return the range of dates
# ' @return      - a window of time points
# '

getAvailableData <- function(curdate,pricedata,range){

    print (paste("searching prices for curdate=",curdate));
    wstart = curdate - as.difftime(range,units="days");
    wend = curdate;
   
    print (pricedata$Date)
    available_data = pricedata[as.numeric(pricedata$Date-wstart)>0 & as.numeric(pricedata$Date-wend)<0,];
    
    return (available_data);
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
    
    allsnpprices = loadSNPData("snp/","snp.csv");
    
    mdates=strptime(data$date,"%Y-%m-%d")
    
    # dev would be used as the time-vector for all data-timeseries
    dvec=as.integer(strftime(strptime(data$date,"%Y-%m-%d"),"%Y"));
   
    
    # marketprices gets searched market price based on days range
    marketprices=searchMarketPrice(mdates=mdates,ndaysrange=10,pricedata=pricedata,processorFunc=processBidAskRow);
    
    # snpprices gets prices searched from available snpprices (loaded from file)
    snpprices=searchMarketPrice(mdates=mdates,ndaysrange=40,pricedata=allsnpprices,processorFunc=processOpenRow);

    # beta would be calculated only if the size of arrays marketprices 
    # and snpprices are the same (otherwise - some of the days were missed
    # and the window would need to be expanded)
    if (length(marketprices$searchedDate) != length(snpprices$searchedDate) || length(snpprices$searchedDate)!=length(mdates) ){
        stop ('market prices for certain dates not available for beta calculations');
    }
    
    
    # strftime is done only because for iteration over the time denudes
    # the type of its object
    
    curstrdates = strftime(marketprices$searchedDate,"%Y-%m-%d");
    count = 1;
    stockReturns = list()
    for (curstrdate in curstrdates) {
        reconvertedCurdate = strptime(curstrdate,"%Y-%m-%d");
        retStockData = getAvailableData(curdate=reconvertedCurdate,pricedata=pricedata,range=60);
        if (dim(retStockData)[1]>0){ 
           stockReturns[[count]]=lag(ts(lag(retStockData$PX_BID)),0)/lag(ts(retStockData$PX_BID),-1)-1;
        }
        count = count + 1
    }

    curstrdates = strftime(snpprices$searchedDate,"%Y-%m-%d");

    count = 1;
    marketReturns = list();
    for (curstrdate in curstrdates) {
        reconvertedCurdate = strptime(curstrdate,"%Y-%m-%d");
        retMarketData = getAvailableData(curdate=reconvertedCurdate,pricedata=allsnpprices,range=60);
        
        if (dim(retMarketData )[1]>0){ 
           return (retMarketData);
           marketReturns [[count]]=lag(ts(lag(retMarketData $PX_BID)),0)/lag(ts(retMarketData $PX_BID),-1)-1;
        }
        count = count + 1
    }
    
    return(marketReturns)
    
    marketprices=getDateVecTimeSeries(dvec=dvec,prices=marketprices$price,dates=marketprices$Date);
    snpprices=getDateVecTimeSeries(dvec=dvec,prices=snpprices$price,dates=snpprices$Date);
    
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
    
    ts_final = ts.intersect(shares=tsshares,ltdebt=tsltdebt,midprice=marketprices,
                            rate=rd,fcff=tsfcff,roic=tsroic,roe=roe,snp=snpprices);
    
    findata =  as.data.frame(ts_final);
    E     = findata$shares*findata$midprice;
    D     = findata$ltdebt;
    V     = E+D;
    r_D   = findata$rate; # rd1 is just r_f 
    r_E   = findata$roe;  # rE should be calculated as r_E=r_f+(r_m-r_f)*(b_E)
    FCFF  = findata$fcff;
    WACC = (1-tax)*(D/V)*r_D+(E/V)*r_E;
    tswacc=ts(data=WACC,start=start(ts_final),end=end(ts_final));
    tswf = ts.intersect(tswacc,ts_final);
    plot(tswf);
    tswf=1;
    #V=(NOPLAT_{t+1}(1−gNOPLAT/RONIC))/(WACC−gNOPLAT)
  } # end for loop
}
