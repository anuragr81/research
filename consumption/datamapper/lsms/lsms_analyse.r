setwd('c:/local_files/research/consumption/datamapper/')
source('translation/frameutils.R');source('lsms/lsms_normalizer.r');source('lsms/lsms_loader.r');ll=lsms_loader(fu=fu,ln=lsms_normalizer)



select_market_price <- function (nat,reg,dstt) { 
  if (!is.na(dstt)) {
    return (dstt)
  } else if (!is.na(reg)) {
    return (reg)
  } else if (!is.na(nat)) {
    return(nat)
  }
  stop("Could not find valid price")
}


get_regressed_market_prices <-function (marketpricesdata,ohsdata,diarydata) {
  region_district_consumed_items <- unique(merge(unique(ohsdata[,c("region","district","hhid")]),
                                                 unique(diarydata[,c("hhid","shortname","item")],by=c("hhid")))[,c("region","district","shortname","item")])
  
  print("Obtaining district level prices")
  market_prices_district <- ddply(unique(marketpricesdata[,c("region","district","code","shortname","lwp","price","factor")]),
                                  .(region,district,shortname),summarise,nprices=length(price[!is.na(price)]),
                                  median_price=median(price[!is.na(price)]), 
                                  reg_price_district=get_regressed_market_price(lwp=lwp,price=price) )[,c("region","district","shortname","median_price","nprices","reg_price_district")]
  print("Marking the missing prices from districts") 
  matched_district_prices <- merge(market_prices_district,region_district_consumed_items,by=c("region","district","shortname"),
                                   all.y=TRUE)
  print("Obtaining regional,national prices")
  market_prices_regional <- ddply(marketpricesdata,.(region,shortname),summarise,reg_price_region=get_regressed_market_price(lwp=lwp,price=price))[,c("region","shortname","reg_price_region")]
  market_prices_national <- ddply(marketpricesdata,.(shortname),summarise,reg_price_nat=get_regressed_market_price(lwp=lwp,price=price))[,c("shortname","reg_price_nat")]
  print("Merging regional, rational prices")
  price_reg_dstt  <-merge(market_prices_regional,matched_district_prices,by=c("region","shortname"),all.y=TRUE)
  price_nat_reg_dstt <-merge(market_prices_national,price_reg_dstt,by=c("shortname"),all.y=TRUE)
  
  price_nat_reg_dstt <- subset(price_nat_reg_dstt,item>10000);
  
  natprices <- subset(price_nat_reg_dstt,!is.na(reg_price_nat))
  print(paste("Ignored entries:",(dim(price_nat_reg_dstt)[1]-dim(natprices)[1]),"/",dim(price_nat_reg_dstt)[1],"(", 
              100*(dim(price_nat_reg_dstt)[1]-dim(natprices)[1])/dim(price_nat_reg_dstt)[1],"%)"))
  prices = ddply(natprices,.(shortname,region,district),summarise,price=select_market_price(nat=reg_price_nat,
                                                                                            reg=reg_price_region,
                                                                                            dstt=reg_price_district))
  #k<-merge(c2010,np2010,by=c("region","district","shortname"),all.x=TRUE)
  return(prices)
}

add_market_price_to_diary <- function (marketpricesdata,ohsdata,diarydata){
  ddata      <- subset(diarydata,item>10000)
  prices     <-     get_regressed_market_prices (marketpricesdata,ohsdata,ddata)
  #k<-merge(c2010,np2010,by=c("region","district","shortname"),all.x=TRUE)
  #market_prices_national <- ddply(marketpricesdata,.(shortname),summarise,reg_price_nat=get_regressed_market_price(lwp=lwp,price=price))[,c("shortname","reg_price_nat")]
  diarywithregiondistrict <- merge(ddata,unique(ohsdata[,c("hhid","region","district")]),all.x=TRUE)
  k<-merge(diarywithregiondistrict,prices,all.x=TRUE)
  noregionprice <- subset(k,is.na(region) & is.na(price))
  ignored_items <- as.character(unique(noregionprice$shortname))
  print(paste("Ignoring consumption on:", toString(ignored_items)))
  diary                   <- subset(k,!is.element(shortname,ignored_items))
  print(paste("Total number of entries ignored:",(dim(k)[1]-dim(diary)[1]),"/",dim(k)[1],"(",
              100*(dim(k)[1]-dim(diary)[1])/dim(k)[1],")"))
  
  return(diary)
}

load_market_prices_ <-function(year,dirprefix,fu,ln,use_pieces,aggregation_code){
  
  if (missing(use_pieces)){
    use_pieces <- TRUE
  } else {
    use_pieces <- FALSE
  }
  if (year ==2010) {
    a<-read.dta(paste(dirprefix,'/lsms/TZNPS2COMDTA/COMSEC_CJ.dta',sep=""),convert.factors = FALSE)
    a<-(a[!is.na(a),])
    cj <- fu()@get_translated_frame(dat=a, names=ln()@ohs_seccj_columns_lsms_2010(), m=ln()@ohs_seccj_mapping_lsms_2010())
    item_names<- ln()@items_codes_2010()
  } else if (year == 2012){
    a <- read.dta(paste(dirprefix,'/lsms/tnz2012/TZA_2012_LSMS_v01_M_STATA_English_labels/COM_SEC_CF.dta',sep=""),convert.factors = FALSE)
    a<-(a[!is.na(a),])
    cj <- fu()@get_translated_frame(dat=a, names=ln()@ohs_seccj_columns_lsms_2012(), m=ln()@ohs_seccj_mapping_lsms_2012())
    item_names<- ln()@items_codes_2012()
  } else if (year == 2008){
    a <- read.dta(paste(dirprefix,'/lsms/tnz2008/TZNPS1CMDTA_E/SEC_J2.dta',sep=""),convert.factors = FALSE)
    a<-(a[!is.na(a),])
    cj <- fu()@get_translated_frame(dat=a, names=ln()@ohs_seccj_columns_lsms_2008(), m=ln()@ohs_seccj_mapping_lsms_2008())
    item_names<- ln()@items_codes_2008()
  } else if (year == 2014) {
    a <- read.dta(paste(dirprefix,'/lsms/tnz2014/TZA_2014_NPS-R4_v03_M_v01_A_EXT_STATA11/com_sec_cf.dta',sep=""),convert.factors = FALSE)
    a<-(a[!(is.na(a$cm_f062) & is.na(a$cm_f065)) ,])
    cj <- fu()@get_translated_frame(dat=a, names=ln()@ohs_seccf_columns_lsms_2014(), m=ln()@ohs_seccf_mapping_lsms_2014())
    cj$region <- as.integer(sapply(cj$clusterid,function(x) { strsplit(x,"-")[[1]][1] }))
    cj$district <- as.integer(sapply(cj$clusterid,function(x) { strsplit(x,"-")[[1]][2] }))
    cj$ward <- as.integer(sapply(cj$clusterid,function(x) { strsplit(x,"-")[[1]][3] }))
    cj$village <- as.integer(sapply(cj$clusterid,function(x) { strsplit(x,"-")[[1]][4] }))
    cj$ea <- as.integer(sapply(cj$clusterid,function(x) { strsplit(x,"-")[[1]][5] }))
    
    k1<-subset(cj,!is.na(price1) & is.na(price2))
    k2<-subset(cj,is.na(price1) & !is.na(price2))
    k3<-subset(cj,!is.na(price1) & !is.na(price2))
    res <- NULL
    cols <- c("region","district","ward","village","ea","item","lwp","lwp_unit","price")
    res <- rbind(res,plyr::rename(k1,c("lwp_unit1"="lwp_unit","lwp1"="lwp","price1"="price"))[,cols])[,cols]
    res <- rbind(res,plyr::rename(k2,c("lwp_unit2"="lwp_unit","lwp2"="lwp","price2"="price"))[,cols])[,cols]
    res <- rbind(res,plyr::rename(k3,c("lwp_unit1"="lwp_unit","lwp1"="lwp","price1"="price"))[,cols])[,cols]
    cj  <- unique(res)
    item_names<- ln()@items_codes_2014()
  }
  
  else {
    stop(paste("year:",year,"not supported"))
  }
  
  if (use_pieces==FALSE) {
    cj_new <- subset(cj,lwp_unit!=5)
    print(paste("Ignoring pieces from the market prices (", sprintf("%.2f",100*(1-dim(cj_new)[1]/dim(cj)[1])),"% entries)"))
    cj     <- cj_new
  }
  cj<-(cj[!is.na(cj$price),])
  cj <- subset(cj,price>0)
  cj$factor<-as.integer(cj$lwp_unit==1)+as.integer(cj$lwp_unit==2)/1000.0+as.integer(cj$lwp_unit==3)+as.integer(cj$lwp_unit==4)/1000.0+as.integer(cj$lwp_unit==5)
  
  cj$lwp <-cj$lwp*cj$factor 
  
  cj$price <-cj$price/cj$lwp
  
  cj$code <- sapply(cj$item,function(x) {if (is.na(as.integer(x))) x else as.integer(x)+10000 } )
  
  Lcodes<-unique(as.character(cj$code)[grep("^L",as.character(cj$code))])
  
  cj$code <- sapply(cj$code,function(x) { if (is.element(x,Lcodes)) as.integer(substr(x,2,nchar(x))) else x })
  
  cj <- subset(cj,price!=Inf)
  
  if (missing(aggregation_code)) {
    prices <- cj
  } else {
    if (aggregation_code=="region"){
      prices<-ddply(cj,.(code,region),summarise,mean_price=mean(price[!is.na(price)]),qprices = quantile(names=FALSE, x=price[!is.na(price)],probs=0.5) , pricessd = sd(price[!is.na(price)]) ) 
    } else if (aggregation_code=="district"){
      prices<-ddply(cj,.(code,region,district),summarise,mean_price=mean(price[!is.na(price)]),qprices = quantile(names=FALSE, x=price[!is.na(price)],probs=0.5), pricessd = sd(price[!is.na(price)]))
    } else if (aggregation_code=="all") {
      prices<-ddply(cj,.(code),summarise,mean_price=mean(price[!is.na(price)]),qprices = quantile(names=FALSE, x=price[!is.na(price)],probs=0.5), pricessd = sd(price[!is.na(price)]))
    } else {
      stop(paste("Unrecognised value for",aggregation_code))
    }
    
  }
  prices_merged<-merge(prices,item_names,all.x=TRUE,by=c("code"))
  
  missing_shortnames <- unique(subset(prices_merged,is.na(shortname))$code)
  if (length(missing_shortnames)>0){
    print(paste("Could not find itemnames for:",toString(missing_shortnames)))
  }
  prices_merged <- merge(prices_merged, ddply(prices_merged, .(shortname),summarise,median_price = median(price)) , by = c("shortname"))
  
  n<-10
  prices_merged_removed   <-(subset(prices_merged, abs(log(price/median_price,n))>=1))
  prices_merged_remaining <-(subset(prices_merged, abs(log(price/median_price,n))<1))
  print(paste("removing",dim(prices_merged_removed)[1],"/",dim(prices_merged)[1],"(",100*dim(prices_merged_removed)[1]/dim(prices_merged)[1]
              ,"%) price entries for being ",n,"times away from the national median"))
  
  return(prices_merged_remaining)
}


item_price_trends <- function() {
  
  food_compliments <- c("fish_seafood","coconut","banana_ripe","")
  
  vegetables <- c("cassava_fresh","greens","")
  
  downwards = c ("banana_green",
  "banana_ripe",
  "canned_milk",
  "citrus",
  "coconut",
  "cooking_oil",
  "peanuts",
  "sugar",
  "yam")
  
  upwards = c ("beef",
  "bread",
  "brews",
  "cassava_flour",
  "cassava_fresh",
  "chicken",
  "fish_seafood",
  "fresh_milk",
  "goat",
  "greens",
  "maize_flour",
  "maize_grain",
  "maize_green",
  "mangoes",
  "millet_flour",
  "millet_grain",
  "onion",
  "potatoes",
  "pulses",
  "rice_husked",
  "rice_paddy",
  "sweet_potato",
  "tea")
  
  missing_rising =   c("bunscakes")
  
  missing_falling = c ("charcoal",
  "kerosene",
  "salt",
  "wheat")
  
  straight_down = c("eggs",
  "milling", #ignored
  "othervegstarch")
  
  one_point = c("sugarcane",
                "pasta",
                "dried_canned_fish", #
                "dried_canned_veg",
                "firewood")
  
  print(length(c(downwards,upwards,missing_rising,missing_falling,straight_down,one_point)))
}




plot_price_tseries <-function(fu,market_prices_national2008,market_prices_national2010,market_prices_national2012) {
  f1=fu()@rbind_xy(x = market_prices_national2008,y = market_prices_national2010, tagx=2008, tagy=2010)
  f2=fu()@rbind_xy(x = f1,y = market_prices_national2012, tagy=2012)
  dev.off(); par(mfrow=c(5,9)); 
  for (x in sort(as.character(unique(f2$shortname))) ) {
    g = subset(f2,shortname==x) ; 
    if(dim(g)[1] == 0 ) {print(paste("bad data:",x))} else { plot(g$tag,g$reg_price,main=x) } 
  }
}

get_regressed_market_price <- function (lwp,price){
  isDebug <- FALSE
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
  if (isDebug){
    print(paste("n=",length(price),"median_price=",median(price),"reg_price=",reg_price))
  }
  if (reg_price <0 || is.na(reg_price) || abs(log(median(price)/reg_price,2))>1 ) {
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
