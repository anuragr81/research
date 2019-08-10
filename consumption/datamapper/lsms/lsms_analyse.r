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

item_price_trends <- function() {
  # national average is not the same as regional changes
  # the grouping may in fact need to be different for regions
  # the criteria of separability is more important
  
  #chicken, cooking oil and citrus are similar
  #starch and fruitsveg are largely similar, except fruitsveg falls more sharply
  #coconut stays up so it should not be in fruitsveg
  
  protein   <- c("beef","goat","fish_seafood","chicken",
                 "fresh_milk","canned_milk") # simple upwards
  
  starch    <- c("bread","cassava_flour","maize_flour","maize_green",
                 "maize_grain","millet_flour","millet_grain","pulses",
                 "rice_husked","rice_paddy","sweet_potato","potatoes","wheat",
                 "yam","othervegstarch",
                 "cooking_oil") # convex

  fruitsveg <- c("banana_green","banana_ripe","cassava_fresh","citrus",
                 "greens","peanuts","bunscakes","coconut","eggs","mangoes","onion") # convex
  
  alcohol   <- c("brews") # no quality calculated
  dried_products <- c("sugar","salt","tea") # no quality calculated


  energy    <- c("kerosene","charcoal","firewood")
  
  combined_groups <- c(protein,starch,fruitsveg,alcohol,dried_products,energy)
  
  #######################################################################
  
  downwards = c ("banana_green",
                 "banana_ripe",
                 "bread",
                 "bunscakes",
                 "canned_milk",
                 "citrus",
                 "coconut",
                 "cooking_oil",
                 "peanuts",
                 "sugar",
                 "yam",
                 "kerosene")
  
  upwards = c ("beef",
               "bread",
               "brews",
               "cassava_flour",
               "cassava_fresh",
               "charcoal",
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
               "milling",
               "onion",
               "potatoes",
               "pulses",
               "rice_husked",
               "rice_paddy",
               "sweet_potato",
               "tea")
  
  straight_up = c("firewood")
  
  straight_down = c("eggs",
                    "salt",
                    "wheat",
                    "othervegstarch")
  
  onepoint = c("sugarcane",
                "pasta",
                "dried_canned_fish", #
                "dried_canned_veg"
  )
  
  struct_groups <- c(downwards,upwards,straight_down,straight_up,onepoint)
  A = combined_groups
  B = struct_groups
  print(paste("A:",toString(combined_groups)))
  print(paste("B:",toString(struct_groups)))
  
  print(paste("A-B:",toString(setdiff(A,B))))
  print(paste("B-A:",toString(setdiff(B,A))))
}




plot_price_tseries <-function(row_pair,ignore_items,fu,switch_off,market_prices_national2008,market_prices_national2010,market_prices_national2012,market_prices_national2014) {
  
  if (missing(ignore_items)){
    ignorelist <- c("batteries","cigarettes","matches","dried_canned_fish","dried_canned_veg","pasta","sugarcane","milling")
  }
  f1=fu()@rbind_xy(x = market_prices_national2008,y = market_prices_national2010, tagx=2008, tagy=2010)
  f2=fu()@rbind_xy(x = f1,y = market_prices_national2012, tagy=2012)
  f3=fu()@rbind_xy(x = f2,y = market_prices_national2014, tagy=2014)
  res = f3
  if (!missing(switch_off) && switch_off==TRUE){
    dev.off(); 
  }
  
  par(mfrow=row_pair); 
  for (x in sort(as.character(unique(res$shortname))) ) {
    if (! is.element(x,ignorelist)) {
      g = subset(res,shortname==x) ; 
      if(dim(g)[1] == 0 ) {print(paste("bad data:",x))} else { 
        plot(g$tag,g$reg_price,main=x,xlab="year",ylab="price",xlim=c(2008,2014),type='o') 
        #print(head(g))
      }
      
    } 
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
