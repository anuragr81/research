setwd('c:/local_files/research/consumption/datamapper/')
source('translation/frameutils.R');source('lsms/lsms_normalizer.r');source('lsms/lsms_loader.r');ll=lsms_loader(fu=fu,ln=lsms_normalizer,lgc=lgc)
source('lsms/lsms_group_collect.r')
#assign("last.warning", NULL, envir = baseenv())


add_market_price_to_diary <- function (lgc,marketpricesdata,ohsdata,diarydata){
  ddata      <- subset(diarydata,item>10000)
  prices     <-     get_regressed_market_prices (lgc,marketpricesdata,ohsdata,ddata)
  #k<-merge(c2010,np2010,by=c("region","district","shortname"),all.x=TRUE)
  #market_prices_national <- ddply(marketpricesdata,.(shortname),summarise,reg_price_nat=get_regressed_market_price(lwp=lwp,price=price))[,c("shortname","reg_price_nat")]
  diarywithregiondistrict <- merge(ddata,unique(ohsdata[,c("hhid","region","district")]),all.x=TRUE)
  k<-merge(diarywithregiondistrict,prices,all.x=TRUE)
  noregionprice <- subset(k,is.na(region) & is.na(price))
  ignored_items <- as.character(unique(noregionprice$shortname))
  print(paste("Ignoring consumption on:", toString(ignored_items)))
  diary                   <- subset(k,!is.element(shortname,ignored_items))
  print(paste("Total number of entries ignored:",(dim(k)[1]-dim(diary)[1]),"/",dim(k)[1],"(",
              100*(dim(k)[1]-dim(diary)[1])/dim(k)[1],"%)"))
  print("PENDING: Addition of prices for wheat, bunscakes, beer, othervegstarch and winespirits (marked as beer)")
  
  return(diary)
}



group_collect_ <- function(year,dirprefix,categoryName,fu,ln,lgc,ohs, hh,basis, use_market_prices) {
  
  
  if (year == 2010 || year == 2012) {
    if (year == 2010){
      itemcodes <- ln()@items_codes_2010()
    } else {
      itemcodes <- ln()@items_codes_2012()
    }
    if (basis=="price"){
      print("Price based groups")
      groups      <- subset( ln()@lsms_groups_pricebased_2010_2012(), category == categoryName )
    } else if (basis == "sparseness"){
      print("sparseness based groups")
      groups      <- subset( ln()@lsms_groups_sparsenessbased_2010_2012(), category == categoryName )
    } else if (basis == "quality"){
      print("quality based groups")
      groups      <- subset( ln()@lsms_groups_qualitybased_2010_2012(), category == categoryName )
    } else {
      stop("Invalid basis")
    }
    
    # check if group columns are known
    if (!setequal(colnames(groups),c("shortname","group","category"))){
      stop("groups must strictly have shortname, group, category columns")
    }
    
  } else {
    stop(paste("Year",year,"not supported for group_collect"))
  } 
  
  
  print(paste("Collecting from groups:",toString(unique(groups$group))))
  if (setequal(groups$group,c("quality"))) {
    # get prices for the year
    print ("Loaded prices noted by the respondents (instead of the market recorded prices)")
    
    if ( use_market_prices == FALSE) {
      print ("Using unit-value as prices")
      mp<-match_recorded_prices(year = year, dirprefix = dirprefix,fu = fu, ln=ln,marketPricesOnly = TRUE)
      mpp<-ddply(mp,.(item),summarise,mean_price=mean(recorded_price))
      itemcodesmap <- plyr::rename(itemcodes[,c("shortname","code")],c("code"="item"))
      mpp <- merge(mpp,itemcodesmap)
      hhp <- merge(hh,mpp,by=c("shortname"))
      hhpg <- merge(hhp,groups,by=c("shortname"))
      print("Obtained prices for quality/expenditureonly group")
      minprices <- ddply(hhpg[,c("shortname","mean_price","category")],.(category),summarise,min_price=min(mean_price))
      hhpg$factor<-as.integer(hhpg$lwp_unit==1)+as.integer(hhpg$lwp_unit==2)/1000.0+as.integer(hhpg$lwp_unit==3)+as.integer(hhpg$lwp_unit==4)/1000.0+as.integer(hhpg$lwp_unit==5)
      hhpg$quantity <-with (hhpg,lwp*factor)
      
      hhpg <- merge(minprices,hhpg)
      # get price ratio for every group
      hhpg$price_ratio <- with (hhpg,mean_price/min_price) 
    } else {
      print ("Using survey's market prices")
      mktprices <- ll@load_market_prices(year = year, dirprefix = dirprefix,fu = fu , ln = ln, use_pieces = FALSE)
      hhp <- add_market_price_to_diary (lgc=lgc,marketpricesdata=mktprices,ohsdata=ohs,diarydata=hh)
      hhpg <- merge(hhp,groups,by=c("shortname"))
      minprices <- ddply(hhpg[,c("shortname","price","category","region","district")],.(category,region,district),summarise,min_price=min(price))
      
      hhpg <- merge(minprices,hhpg)
      # get price ratio for every group
      hhpg$price_ratio <- with (hhpg,price/min_price) 
      hhpg <- plyr::rename(hhpg,c("lwp"="quantity"))
    }
    
    # calculate sum of quantity consumed
    totq <- ddply(unique(hhpg[,c("hhid","shortname","category","quantity","price_ratio","min_price")]),.(category,hhid),summarise,totq=sum(quantity), qsum = sum (price_ratio*quantity), min_price=unique(min_price))[,c("hhid","category","qsum","totq","min_price")]
    # calculate the quality ratio -  sum (price/min(price)*quantity) / sum (quantity)
    totq$quality <- with(totq,qsum/totq)
    print(head(totq))
    return(totq[,c("hhid","category","quality","min_price")])
    
  } else if (setequal(groups$group,c("high","low")) || setequal(groups$group,c("low")) || setequal(groups$group,c("high"))) {
    print("Using high-low groups")
    ##Note: cost would not be avalable from the diary if the type of commodity in the group is an asset.
    # if we can get the asset costs populated, then the data-frame can be rbind-ed to vis data-frame.
    #
    
    vis                                  <- ddply(merge(hh,groups) ,.(hhid,group),summarise,group_cost = sum(cost)) 
    noGroupCostHhids                     <- setdiff(unique(hh$hhid),unique(vis$hhid))
    noGroupCostHigh                      <- data.frame(hhid=noGroupCostHhids,group="high",group_cost=rep(0,length(noGroupCostHhids)))
    noGroupCostLow                       <- data.frame(hhid=noGroupCostHhids,group="low" ,group_cost=rep(0,length(noGroupCostHhids)))
    vis                                  <- rbind(vis,noGroupCostHigh)
    vis                                  <- rbind(vis,noGroupCostLow)
    
    vis                                  <- subset(vis,!is.na(group_cost))
    vis                                  <- merge(rename(subset(vis,group=="low"),replace = c("group_cost"="low_cost")),rename(subset(vis,group=="high")[,c("hhid","group_cost")],replace = c("group_cost"="high_cost")),all=TRUE)
    vis$has_high                         <- !is.na(vis$high_cost) & vis$high_cost>0
    if (dim(vis[is.na(vis$high_cost),])[1]>0){
      vis[is.na(vis$high_cost),]$high_cost <- 0
    }
    if (dim(vis[is.na(vis$low_cost),])[1]>0){
      vis[is.na(vis$low_cost),]$low_cost   <- 0
    }
    
    vis$low_cost                         <- vis$low_cost  + 1e-16 # to avoid divide by zero error in the ratio
    vis$high_cost                        <- vis$high_cost + 1e-16
    
    vis$highratio                        <- with(vis,high_cost/(low_cost+high_cost))
    vis$quality                          <- vis$highratio
    vis$ln_tot_categ_exp                 <- log(with(vis,high_cost+low_cost))
    vis$tot_categ_exp                    <- with(vis,high_cost+low_cost)
    vis$high_expenditure                 <- vis$high_cost
    vis$group                            <- NULL
    vis$low_cost                         <- NULL
    vis$high_cost                        <- NULL
    if (length(unique(groups$category))>1){
      stop("Cannot handle more than one category")
    } else {
      vis$category <- unique(groups$category)
    }
    
  } else if (setequal(groups$group,c("asset","expenditure"))){
    print("Using asset-expenditure groups")
    
    assets            <- read_assets_file(year = year, dirprefix = dirprefix,fu = fu, ln = ln)
    if (dim(subset(assets,is.na(shortname) ))[1]) {
      stop(paste("Missing itemcode->shortname mapping for year",year))
    }
    expenditureData   <- merge(hh,groups)
    print(paste("Items relevant for expenditure:", toString(unique(as.character(expenditureData$shortname)))))
    
    print("Running ddply on groups and hhid")
    vis               <- ddply(expenditureData ,.(hhid,group),summarise,group_cost = sum(cost))
    no_vis_hhid       <- setdiff(unique(hh$hhid),unique(vis$hhid))
    print("Assigning zero cost to hhids with missing expenditure")
    no_vis            <- data.frame(hhid=no_vis_hhid,group="expenditure",group_cost=rep(0,length(no_vis_hhid)))
    vis               <- rbind(vis,no_vis)
    print("Filtering NA expenditure out")
    
    vis               <- rename(subset(vis,group=="expenditure"),c("group_cost"="tot_categ_exp"))
    vis               <- subset(vis, !is.na(tot_categ_exp))
    relevantAssets    <- as.character(subset(groups, group== "asset")$shortname)
    
    ady               <- get_asset_score(diaryData = hh,assetsData = assets,assetsList = relevantAssets , 
                                         ln=ln, year = year);
    vis$group         <- NULL
    vis               <- merge(vis,ady,by=c("hhid"))
    
    vis$pe  <- with(vis,tot_categ_exp/log(asset_score+1e-7))
    
    
  } else if (setequal(groups$group,c("assetsonly"))) {
    print("Using assets-only groups")
    print("Only assets to be used as dependent variable")
    assets            <- read_assets_file(year = year, dirprefix = dirprefix,fu = fu, ln = ln)
    if (dim(subset(assets,is.na(shortname) ))[1]) {
      stop(paste("Missing itemcode->shortname mapping for year",year))
    }
    
    vis               <- ddply(merge(hh,groups) ,.(hhid,group),summarise,group_cost = sum(cost))
    no_vis_hhid       <- setdiff(unique(hh$hhid),unique(vis$hhid))
    no_vis            <- data.frame(hhid=no_vis_hhid,group="assetsonly",group_cost=rep(0,length(no_vis_hhid)))
    vis               <- rbind(vis,no_vis)
    
    relevantAssets    <- as.character(subset(groups, group== "assetsonly")$shortname)
    
    print(paste("relevantAssets=",toString(relevantAssets)))
    
    ady               <- get_asset_score(diaryData = hh,assetsData = assets,assetsList = relevantAssets , 
                                         ln=ln, year = year);
    vis$group         <- NULL
    vis               <- merge(vis,ady,by=c("hhid"),all.y=TRUE)
    if (dim (vis[is.na(vis$asset_score),])[1]>0){
      vis[is.na(vis$asset_score),]$asset_score<-0
    }
    if (dim (vis[is.na(vis$group_cost),])[1]>0){
      vis[is.na(vis$group_cost),]$group_cost<-0
    }
    
  } else if (setequal(groups$group,c("expenditureonly") )){
    print("Using expenditure-only groups")
    print("Only expenditure to be used as the dependent variable");
    relevantItems    <- as.character(subset(groups, group== "expenditureonly")$shortname)
    print(paste("Expenditures being added up for items:",toString(relevantItems)))
    
    vis              <- ddply(subset( merge(hh,groups), is.element(shortname, relevantItems) ),.(hhid),summarize,group_cost=sum(cost))
    no_vis_hhid      <- setdiff(unique(hh$hhid),unique(vis$hhid))
    no_vis           <- data.frame(hhid=no_vis_hhid,group_cost=rep(0,length(no_vis_hhid)))
    vis              <- rbind(vis,no_vis)
    vis$quality      <- log(vis$group_cost+1e-7)
    if (length(unique(groups$category))>1){
      stop("Cannot handle more than one category")
    } else {
      vis$category <- unique(groups$category)
    }
  } else {
    stop( paste ( "Unknown row elements in groups frame",toString(unique(groups$group))))
  }
  print("Collected group expenditures")
  return(vis)
}

get_household_head_info <- function (ohs) {
  
  heads<-ohs[as.integer(ohs$household_status)==1,]
  print(paste("Number of houesehold heads = ",length(unique(heads$hhid))))
  if (!is.element("y2_hhid",colnames(heads)) ){
    heads$y2_hhid<-rep(NA,dim(heads)[1])
  }
  
  heads<-data.frame(hhid=heads$hhid,
                    y2_hhid = heads$y2_hhid,
                    highest_educ=heads$highest_educ,
                    age=heads$age,
                    region=heads$region,
                    expensiveregion=heads$expensiveregion,
                    popdensity =heads$popdensity,
                    district=heads$district,
                    ward=heads$ward,
                    ea=heads$ea,
                    personid=heads$personid,
                    litlang=heads$litlang,
                    isrural=heads$isrural,
                    isurbanp=heads$isurbanp,
                    accessiblemarket=heads$accessiblemarket,
                    travelcost=heads$travelcost,
                    schoolowner=heads$schoolowner,
                    occupation = heads$occupation,
                    occupation_rank = heads$occupation_rank,
                    years_community=heads$years_community,
                    housingstatus=heads$housingstatus,
                    roomsnum=heads$roomsnum,
                    roofmaterial=heads$roofmaterial,
                    floormaterial=heads$floormaterial,
                    wallsmaterial=heads$wallsmaterial,
                    toilet=heads$toilet,
                    cookingfuel=heads$cookingfuel,
                    dryseasonwatersource=heads$dryseasonwatersource,
                    
                    stringsAsFactors=FALSE);
  print(
    paste("Total number of households with head_highest_education=NA : ",
          dim(heads[is.na(heads$highest_educ),])[1]
    )
  );
  #heads$highest_educ[is.na(heads$highest_educ)]<-1
  print("Setting members with years_community=99 as their age");
  heads$is_resident<-as.integer(as.integer(heads$years_community)==99)
  heads$years_community<-heads$years_community+heads$is_resident*(heads$age-99);
  
  return(heads)
}


get_hsize<-function (ohs)
{
  
  print ("Calculating hsize")
  hhid_personid_consu<-data.frame(hhid=ohs$hhid,personid=ohs$personid,age=ohs$age,stringsAsFactors=FALSE);
  szNonIgnoredAgeWithNAs<- dim(hhid_personid_consu)[1]
  szIgnoredAgeWithNAs<-dim(hhid_personid_consu[is.na(hhid_personid_consu$age),])[1]
  hhid_personid_consu<-hhid_personid_consu[!is.na(hhid_personid_consu$age),]
  print (paste("Ignored ", szIgnoredAgeWithNAs ," (out of ",szNonIgnoredAgeWithNAs ,") ohs entries with no age (cursz=",
               dim(hhid_personid_consu)[1],")"))
  
  #calculating the consumption_factor
  
  hhid_personid_consu$consumption_factor<-as.integer(hhid_personid_consu$age<=5)*.2+as.integer(hhid_personid_consu$age>5 & hhid_personid_consu$age<=10)*.3+as.integer(hhid_personid_consu$age>10 & hhid_personid_consu$age<=15)*.4+as.integer(hhid_personid_consu$age>15 & hhid_personid_consu$age<=45)+as.integer(hhid_personid_consu$age>45 & hhid_personid_consu$age<=65)*.7+as.integer(hhid_personid_consu$age>65)*.6
  
  hhid_personid<- ddply(hhid_personid_consu,.(hhid),summarize,hsize=length(personid), consu=sum(consumption_factor));
  print(paste("Number of households with hsize data = ",length(unique(hhid_personid$hhid))))
  
  return(hhid_personid)
}

get_regressed_market_prices <-function (lgc,marketpricesdata,ohsdata,diarydata) {
  
  
  
  region_district_consumed_items <- unique(merge(unique(ohsdata[,c("region","district","hhid")]),
                                                 unique(diarydata[,c("hhid","shortname","item")],by=c("hhid")))[,c("region","district","shortname","item")])
  
  print("Obtaining district level prices")
  market_prices_district <- ddply(unique(marketpricesdata[,c("region","district","code","shortname","lwp","price","factor")]),
                                  .(region,district,shortname),summarise,nprices=length(price[!is.na(price)]),
                                  median_price=median(price[!is.na(price)]), 
                                  reg_price_district=lgc()@get_regressed_market_price(lwp=lwp,price=price) )[,c("region","district","shortname","median_price","nprices","reg_price_district")]
  print("Marking the missing prices from districts") 
  matched_district_prices <- merge(market_prices_district,region_district_consumed_items,by=c("region","district","shortname"),
                                   all.y=TRUE)
  print("Obtaining regional,national prices")
  market_prices_regional <- ddply(marketpricesdata,.(region,shortname),summarise,reg_price_region=lgc()@get_regressed_market_price(lwp=lwp,price=price))[,c("region","shortname","reg_price_region")]
  market_prices_national <- ddply(marketpricesdata,.(shortname),summarise,reg_price_nat=lgc()@get_regressed_market_price(lwp=lwp,price=price))[,c("shortname","reg_price_nat")]
  print("Merging regional, rational prices")
  price_reg_dstt  <-merge(market_prices_regional,matched_district_prices,by=c("region","shortname"),all.y=TRUE)
  price_nat_reg_dstt <-merge(market_prices_national,price_reg_dstt,by=c("shortname"),all.y=TRUE)
  
  price_nat_reg_dstt <- subset(price_nat_reg_dstt,item>10000);
  
  natprices <- subset(price_nat_reg_dstt,!is.na(reg_price_nat))
  print(paste("Ignored price entries:",(dim(price_nat_reg_dstt)[1]-dim(natprices)[1]),"/",dim(price_nat_reg_dstt)[1],"(", 
              100*(dim(price_nat_reg_dstt)[1]-dim(natprices)[1])/dim(price_nat_reg_dstt)[1],"%)"))
  prices = ddply(natprices,.(shortname,region,district),summarise,price=lgc()@select_market_price(nat=reg_price_nat,
                                                                                                  reg=reg_price_region,
                                                                                                  dstt=reg_price_district))
  #k<-merge(c2010,np2010,by=c("region","district","shortname"),all.x=TRUE)
  return(prices)
}

get_total_expenditures <- function (hh,ohs) {
  print("Calculating total expenditures")
  totexp<-ddply(hh,.(hhid),summarize,total_expenditure=sum(cost))
  print(paste("Number of households with total expenditure data minus housing = ",length(unique(totexp$hhid))))
  
  # obtain map (hhid->housing) with ddply
  tothouserent<-ddply(ohs,.(hhid),summarize,tothouserent=sum(houserent))
  # obtain map (hhid->educexpen) with ddply 
  print(paste("Number of households with houserent data = ",length(unique(tothouserent$hhid))))
  
  print ("Appending educexpense and houserent to total expenditure");
  #* setting houses with education exenses= NA as zero
  ohs$educexpense[is.na(ohs$educexpense)]<-0
  toteducexpense<-ddply(ohs,.(hhid),summarize,toteducexpense=sum(educexpense))
  print(paste("Number of households with educexpense data = ",length(unique(toteducexpense$hhid))))
  
  totexp<-merge(totexp,toteducexpense)
  print(paste("Number of households after merging total_expenditure and total_educexpense = ",length(unique(totexp$hhid))))
  
  totexp<-merge(totexp,tothouserent)
  print(paste("Number of households after merging total_expenditure with houserent = ",length(unique(totexp$hhid))))
  
  totexp$total_expenditure=totexp$total_expenditure+totexp$toteducexpense+totexp$tothouserent
  
  #* calculating educational expense and total houserent
  print(paste("Number of households with total expenditure data = ",length(unique(totexp$hhid))))
  #* finding personids of the house-heads and their education-level, age, years in community, 
  #* language, occupation and 
  #* other household characteristics
  return(totexp)
}






run_test <- function(){
  categories <- c("densefoods","nonfresh","fruitsveg","protein","alcohol","compliments")
  g2010 <- ll@group_expenditure(year = 2010, dirprefix = "../",
                            fu = fu , ln = lsms_normalizer, lgc=lgc,
                            basis = "quality", categoryNames = categories,returnBeforeGrouping = FALSE,
                            minConsumerNumber = 5,use_market_prices=TRUE)
  return(g2010)
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
