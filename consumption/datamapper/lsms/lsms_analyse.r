setwd('c:/local_files/research/consumption/datamapper/')
source('translation/frameutils.R');source('lsms/lsms_normalizer.r');source('lsms/lsms_loader.r');ll=lsms_loader(fu=fu,ln=lsms_normalizer)



get_regressed_market_prices <-function (marketpricesdata,ohsdata,diarydata) {
  region_district_consumed_items <- unique(merge(unique(ohsdata[,c("region","district","hhid")]),
                                                 unique(diarydata[,c("hhid","shortname","item")],by=c("hhid")))[,c("region","district","shortname","item")])
  
  print("Obtaining district level prices")
  market_prices_district <- ddply(marketpricesdata,.(region,district,shortname,price),summarise,nprices=length(price[!is.na(price)]),
                                  median_price=median(price[!is.na(price)]), 
                                  reg_price_district=get_regressed_market_price(lwp=lwp,price=price) )[,c("region","district","shortname","median_price","nprices","reg_price_district")]
  print("Marking the missing prices from districts") 
  matched_district_prices <- merge(market_prices_district,region_district_consumed_items,by=c("region","district","shortname"),
                                   all.y=TRUE)
  print("Obtaining regional,national prices")
  market_prices_regional <- ddply(marketpricesdata,.(region,shortname),summarise,reg_price_region=get_regressed_market_price(lwp=lwp,price=price))[,c("region","shortname","reg_price_region")]
  market_prices_national <- ddply(marketpricesdata,.(shortname),summarise,reg_price_nat=get_regressed_market_price(lwp=lwp,price=price))[,c("shortname","reg_price_nat")]
  print("Merging regional, rational prices")
  k  <-merge(market_prices_regional,matched_district_prices,by=c("region","shortname"),all.y=TRUE)
  kk <-merge(market_prices_national,k,by=c("shortname"),all.y=TRUE)
  return(kk)
}


get_regressed_market_price <- function (lwp,price){
  if (length(unique(price))>=3 && any(abs(diff(price))>0) && any(abs(diff(lwp))>0)){
    invsqr         <- 1/lwp**2
    lmres          <- lm(price~ invsqr)
    reg_price      <- (lmres$coefficients[[1]]+lmres$coefficients[[2]])
  } else if (length(price)==0){
    stop("No prices")
  } else if (length(unique(price))<3 || !any(abs(diff(price))>0) || !any(abs(diff(lwp))>0) ){
    return(median(price))
  } else {
    stop("Unhandled block")
  }
  
  if (reg_price <0 || abs(log(median(price)/reg_price,2))>1 ) {
    return(median(price))
  } else {
    return(reg_price)
  }
  
  
}

plot_commodity <- function(sname,data,year)
{
  #print(paste("sname=",sname,"year=",year))
  if (missing(year)){
    plotname <- sname
  }
  else {
    plotname <- paste(sname,"(",year,")")
  }
  
  
  dat <- subset(data,shortname==sname)
  if (dim(dat)[1]>1 && any(abs(diff(dat$price))>0) ){
    dat$invsqr <- with(dat,1/lwp**2)
    lmres <- lm(data=dat,price~ invsqr)
    plot(dat$invsqr,dat$price,xlab = "1/(q*q)", ylab="mp",main=plotname)
    if (any(abs(diff(dat$lwp))!=0)) {
      abline(lmres)
    } else {
      print("Could not plot line")
    }
    
    return(lmres$coefficients[[1]]+lmres$coefficients[[2]])
    #return(lmres)
  }
  
}  
#m2010<-ll@load_market_prices(year = 2010, dirprefix = "../",fu = fu, ln = lsms_normalizer,use_pieces = FALSE)
#regs<-as.character(unique(m2010$region))
#items<-as.character(unique(m2010$shortname))
#iname="beef";par(mfrow=c(5,5));sapply(regs,function(x) { print(x); g=plot_commodity(sname = iname,data=subset(m2010,shortname==iname & region==as.character(x)),year=paste(2010,"- region:",x)) } );
#for (x in regs) {g=plot_commodity(sname = iname,data=subset(m2010,shortname==iname & region==as.character(x)),year=paste(2010,"- region:",x)) }

#region_district_consumed_items <- unique(merge(unique(o2010[,c("region","district","hhid")]),unique(c2010[,c("hhid","shortname","item")],by=c("hhid")))[,c("region","district","shortname","item")])
#market_prices_district <- ddply(m2010,.(region,district,shortname,price),summarise,nprices=length(price[!is.na(price)]),median_price=median(price[!is.na(price)]), reg_price_district=get_regressed_price(lwp=lwp,price=price) )[,c("region","district","shortname","median_price","nprices","reg_price_district")]
#matched_district_prices <- merge(market_prices_district,region_district_consumed_items,by=c("region","district","shortname"),all.y=TRUE)

#market_prices_regional <- ddply(m2010,.(region,shortname),summarise,reg_price_region=get_regressed_price(lwp=lwp,price=price))[,c("region","shortname","reg_price_region")]

#k<-merge(market_prices_regional,matched_district_prices,by=c("region","shortname"),all.y=TRUE)
