setwd('c:/local_files/research/consumption/datamapper/')
source('translation/frameutils.R');source('lsms/lsms_normalizer.r');source('lsms/lsms_loader.r');ll=lsms_loader(fu=fu,ln=lsms_normalizer,lgc=lgc)
source('lsms/lsms_group_collect.r'); source('lsms/lsms_datastorage.R')
#assign("last.warning", NULL, envir = baseenv())

write_mills_input <- function (allgroupsdat,millsi,yr){
  # gcols obtained using: toString(paste("'",colnames(x),"'",sep=""))
  gcols <- c('hhid', 'total_expenditure', 'toteducexpense', 'tothouserent',  'hsize', 'consu', 'highest_educ', 'age'
             , 'expensiveregion', 'popdensity','region','district','litlang',
             'isrural', 'isurbanp', 'occupation', 'occupation_rank', 'years_community', 
             'housingstatus', 'roomsnum',  'floormaterial', 'cookingfuel', 'is_resident', 'ln_tot_exp', 'year')
  ag <- unique(subset(allgroupsdat,year==yr)[,gcols])
  ydat <- merge(ag,subset(millsi,year==yr),all.x=TRUE,by=c("hhid","year","region", "district"))
  
  for (sn in as.character(unique(ydat$shortname))) { if (!is.na(sn)) { dat <- subset(ydat,shortname==sn); write_dta(dat,paste('c:/temp/dat',yr,'_',sn,'.dta',sep="")) } } 
  
}
read_stata_aids_results <- function(r,read_coeff){
  #load results 
  colnames(r) <- c("name","coeff","se","z","p","l","r" )
  namesrow <- c(as.integer(rownames(subset(r,is.na(coeff)))),dim(r)[1]+1)
  out <- NULL
  for ( i in seq(length(namesrow)-1) ) { 
    start = namesrow[i]
    end = namesrow[i+1]-1
    x = data.frame(r[(start+1):end,])
    x$q <- r[start,]$name
    out <- rbind(out,x)
  }
  if ( read_coeff == FALSE) { 
    k  <- out[c("q","name","z")]
    kk <- k %>% spread(name,z)
  } else {
    k  <- out[c("q","name","coeff")]
    kk <- k %>% spread(name,coeff)
  }
  
  return(kk)
  
}

split_sz <- function(N,sz) {
  if (sz > N ){
    return(data.frame(start=1,end=N))
  }
  nsplit <- as.integer(N/sz); 
  start_list  <- seq(1,N,nsplit) ; 
  end_list    <- sapply(nsplit + seq(1,N,nsplit) -1 , function(x) { min(x,N) })
  x <- NULL
  for ( i in seq(length(start_list))) {
    x <- rbind(x,data.frame(start=start_list[i],end=end_list[i]))
  }
  return(x)
}

num_stars <- function(pgtz) {
  return ((as.numeric(pgtz)<.001)*3 + (as.numeric(pgtz)>=.001 & as.numeric(pgtz) < .01)*2 + (as.numeric(pgtz)>=.01 & as.numeric(pgtz) < .05)*1)
}

read_stata_aids_results_files <- function(f,skip_first,precision,split_size){
  
  
  #a <- read.csv('c:/local_files/research/consumption/aidsresults9.csv')
  a <- read.csv(f,header=TRUE)
  b <- a[,c("comm","var","coef","Pgtz")]
  bcoef <-t( b[,c("comm","var","coef")] %>% spread(var,coef))
  #write.csv(bcoef,'c:/temp/bcoef.csv')
  bPgtz <-t( b[,c("comm","var","Pgtz")] %>% spread(var,Pgtz))
  
  fm <- function(df) { for ( i in seq(dim(df)[2])) { df[,i] = format(round(as.numeric(df[,i]),4),nsmall=4) } ; return(df) } 
  
  if (skip_first){
    colnames(bcoef) <- bcoef[1,]
    bcoef <- fm(bcoef[2:dim(bcoef)[1],])
    colnames(bPgtz) <- bPgtz[1,]
    bPgtz <- fm(bPgtz[2:dim(bPgtz)[1],])
  }
  for ( i in seq(dim(bPgtz)[2])){
    #numstars_suffix  <- sapply(num_stars(bPgtz[,i]), function(x) { if (x>0) paste("^{",rep("*",x),"}",sep="") else ""} )
    numstars_suffix = array()
    nstars_array <- num_stars(bPgtz[,i])
    for ( j in seq(length(nstars_array))) {
      if (nstars_array[j]>0){
        numstars_suffix[j] <- paste("$^{",strrep("*",nstars_array[j]),"}$",sep="")
      } else {
        numstars_suffix[j] <- ""
      }
    }
    bPgtz[,i]        <- paste("( ",bPgtz[,i],numstars_suffix," )",sep="")
  }

  idnames <- rownames(as.data.frame(bcoef))
  for ( i in seq(1,dim(bPgtz)[1])) {
    y <- as.data.frame(rbind(bcoef[i,],bPgtz[i,]))
    y$idname <- c(idnames[i],"")
    if (i == 1){
      x <- y
    } else {
      x <- rbind( x, y)     
    }
  }
  
  nonidcols <- setdiff(colnames(x),c("idname"))
  
  if (missing(split_size)){
    x <- x [ ,c("idname",nonidcols)]
    return(x)
  } else {
    indices  <- split_sz(length(nonidcols),split_size)
    retlist  <- list()
    for ( i in seq(dim(indices)[1])){
      retlist [[i]] <- x[,c("idname",nonidcols[indices[i,]$start:indices[i,]$end])]
    }
    return(retlist)
  }
  
} 


fill_in_2010_house_prices <- function(){
  ap <-subset(get_asset_average_prices(2012,use_ward=TRUE),shortname=="house")
  a2010 <- subset(ll@read_assets_file(year = 2010, dirprefix = "../", fu = fu, ln = lsms_normalizer),shortname=="house" & number >=1)
  o2010 <- ll@load_ohs_file(year = 2010, dirprefix = "../", fu = fu, ln = lsms_normalizer)
  a2010 <- merge(a2010,unique(o2010[,c("hhid","region","district","ward")]),by=c("hhid"))
  mm <- merge(a2010,ap,by=c("shortname","region","district","ward"),all.x=TRUE)
  print(paste("Ignored",dim(subset(mm,is.na(assetprice)))[1], "entries"))
  mm2 <- subset(mm,!is.na(assetprice))
  
  return(mm2)
}


fill_in_2010_prices <- function(){
  ap <- get_asset_average_prices(2012,FALSE)
  a2010wohouse <- subset(ll@read_assets_file(year = 2010, dirprefix = "../", fu = fu, ln = lsms_normalizer), number >=1 & shortname!="house")
  datwohouse <- merge(a2010wohouse,ap,by=c("shortname")) %>% mutate(asset_mtm = assetprice *number )
  
  mm <- fill_in_2010_house_prices()[,c("hhid","shortname","number","assetprice","itemcode","longname","category")]
  datwhouse <- mm %>% mutate(asset_mtm = assetprice *number )
  dat <- rbind(datwohouse,datwhouse)
  return(dat)
}

get_asset_average_prices <- function(yr,use_ward){
  assetnames_transport   <- c('bike', 'motorbike', 'car')
  #ignored house and land (because their prices may vary too much for us to estimate 2010 values)
  assetnames_household   <- c(  'sewingmachine', 'bed',  'watch',  'chair', 'table', 'cupboard', 'sofa','sports_hobby')#,'land', 'house')
  assetnames_electric     <- c('mobile', 'waterheater','camera', 'phone', 'musicplayer', 'videoplayer', 'musicsystem', 'ac_fan', 'waterpump', 'tv', 'dishtv', 'computer',  'refrigerator' )
  
  assetnames <- c(assetnames_electric,c(assetnames_household,assetnames_transport))
  adat <- ll@read_assets_file(year = yr, dirprefix = "../", fu = fu, ln = lsms_normalizer)
  odat <- ll@load_ohs_file(year = yr, dirprefix = "../", fu = fu, ln = lsms_normalizer)
  if (use_ward==TRUE){
    adat <- merge(adat,unique(odat[,c("hhid","region","district","ward")]),by=c("hhid"))
  }
  assets <- unique(as.character(adat$shortname))
  if (use_ward == TRUE){
    assetpricesdf <- ddply(subset(adat[,c("mtm","hhid","number","shortname","region","district","ward")],number>=1 & !is.na(mtm)),
                           .(shortname,region,district,ward), summarise, assetprice=mean(mtm))
  } else  {
    assetprices <- sapply(assets, function(x) { quantile(with(subset(adat, number>=1 & shortname==x & !is.na(mtm)),mtm),0.85) } )
    assetpricesdf <- data.frame(shortname=assets, assetprice = assetprices)
    rownames(assetpricesdf) <- assetpricesdf$shortname
    assetpricesdf[is.na(assetpricesdf$assetprice),]$assetprice <- 0
  }
  
  
  #dat <- merge(adat,assetpricesdf,by=c("shortname")) %>% mutate(asset_mtm = assetprice *number )
  #k <- ddply(subset(dat,is.element(shortname,assetnames))[,c("hhid","asset_mtm")],.(hhid),summarise,cost = log(sum(asset_mtm)+1e-7))
  
  return (assetpricesdf)
}

merge_asset_mtms_with_prepared_quality_data <- function(allg,m){
  aggdat                                                           <- merge(allg,m,all.x=TRUE,by=c("hhid","year"))
  aggdat[is.na(aggdat$electric_assets_mtm),]$electric_assets_mtm   <- 0
  aggdat[is.na(aggdat$transport_assets_mtm),]$transport_assets_mtm <- 0
  aggdat[is.na(aggdat$household_assets_mtm),]$household_assets_mtm <- 0
  aggdat[is.na(aggdat$all_assets_mtm),]$all_assets_mtm             <- 0
  aggdat$energy_rank   <- as.integer(is.element(aggdat$lightingfuel,c(1,2,8)))*3 + as.integer(is.element(aggdat$lightingfuel,c(3,4,7)))*2 + as.integer(is.element(aggdat$lightingfuel,c(5,6,9,10)))*1
  aggdat$has_electric  <- as.integer(is.element(aggdat$lightingfuel,c(1,8)))
  return(aggdat)
}

all_asset_mtms <- function(sum_costs) {
  assetnames_transport   <- c('bike', 'motorbike', 'car')
  #ignored house and land (because their prices may vary too much for us to estimate 2010 values)
  assetnames_household   <- c(  'sewingmachine', 'bed',  'watch',  'chair', 'table', 'cupboard', 'sofa','sports_hobby','land', 'house')
  assetnames_electric     <- c('mobile', 'waterheater','camera', 'phone', 'musicplayer', 'videoplayer', 'musicsystem', 'ac_fan', 'waterpump', 'tv', 'dishtv', 'computer',  'refrigerator' )
  
  commoncols <- c("itemcode" ,"hhid","number","mtm","longname","shortname","category")
  convert = function (x) plyr::rename(x[,commoncols],c ("mtm"="assetprice")) %>% mutate(asset_mtm = assetprice * number)
  
  a2010       <- fill_in_2010_prices()
  a2010$year  <- 2010
  a2012       <- ll@read_assets_file(year = 2012, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  a2012       <- convert(a2012)
  a2012$year  <- 2012
  x           <- rbind(a2010,a2012)
  a2014       <- ll@read_assets_file(year = 2014, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  a2014       <- convert(a2014)
  a2014$year  <- 2014
  x     <- rbind(x,a2014)
  if (sum_costs) {
    transport_assetcosts <- ddply(subset(x,is.element(shortname,assetnames_transport) & !is.na(asset_mtm))[,c("hhid","year","asset_mtm")], .(hhid,year),summarise,transport_assets_mtm = log(sum(asset_mtm)+1))
    household_assetcosts <- ddply(subset(x,is.element(shortname,assetnames_household) & !is.na(asset_mtm))[,c("hhid","year","asset_mtm")], .(hhid,year),summarise,household_assets_mtm = log(sum(asset_mtm)+1))
    electric_assetcosts  <- ddply(subset(x,is.element(shortname,assetnames_electric) & !is.na(asset_mtm))[,c("hhid","year","asset_mtm")], .(hhid,year),summarise,electric_assets_mtm = log(sum(asset_mtm)+1))
    assetcosts           <- ddply(subset(x,!is.na(asset_mtm))[,c("hhid","year","asset_mtm")], .(hhid,year),summarise,all_assets_mtm = log(sum(asset_mtm)+1))
    
    y <- merge(assetcosts,merge(electric_assetcosts,merge(transport_assetcosts,household_assetcosts,by=c("hhid","year"),all=TRUE),by=c("hhid","year"),all=TRUE),by=c("hhid","year"),all=TRUE)
    y[is.na(y)]<-0
    return(y)
  }
  else {
    return(x)
  }
}

all_asset_scores <- function(years,dirprefix,fu,ln,ll){
  assetnames_transport   <- c('bike', 'motorbike', 'car')
  #ignored house and land (because their prices may vary too much for us to estimate 2010 values)
  assetnames_household   <- c(  'sewingmachine', 'bed',  'watch',  'chair', 'table', 'cupboard', 'sofa','sports_hobby')#,'land', 'house')
  assetnames_electric     <- c('mobile', 'waterheater','camera', 'phone', 'musicplayer', 'videoplayer', 'musicsystem', 'ac_fan', 'waterpump', 'tv', 'dishtv', 'computer',  'refrigerator' )
  out <- NULL
  for (yr in years){
    cdat <- ll@load_diary_file(dirprefix = dirprefix, year = yr, fu = fu, ln = ln )
    adat <- ll@read_assets_file(year = yr, dirprefix = dirprefix, fu = fu, ln = ln)
    asset_scores_transport <- plyr::rename(ll@get_asset_score(year = yr, diaryData = cdat, assetsData = adat, assetsList = assetnames_transport, ln = ln ),
                                           c("asset_score"="trans_asset_score"))
    asset_scores_household <- plyr::rename(ll@get_asset_score(year = yr, diaryData = cdat, assetsData = adat, assetsList = assetnames_household, ln = ln ),
                                           c("asset_score"="hh_asset_score"))
    asset_scores_electric    <- plyr::rename(ll@get_asset_score(year = yr, diaryData = cdat, assetsData = adat, assetsList = assetnames_electric, ln = ln ),
                                             c("asset_score"="elec_asset_score"))
    asset_scores <- merge(asset_scores_household,asset_scores_transport,by=c("hhid"))
    asset_scores <- merge(asset_scores, asset_scores_electric, by = c("hhid"))
    asset_scores$year <- yr
    out  <- rbind(out,asset_scores)
  }
  out$norm_trans_asset_score <- (out$trans_asset_score - min(out$trans_asset_score)) / ( max(out$trans_asset_score) - min(out$trans_asset_score) )
  out$norm_hh_asset_score <- (out$hh_asset_score - min(out$hh_asset_score)) / ( max(out$hh_asset_score) - min(out$hh_asset_score) )
  out$norm_elec_asset_score <- (out$elec_asset_score - min(out$elec_asset_score)) / ( max(out$elec_asset_score) - min(out$elec_asset_score) )
  return(out)
}

run_test <- function() {
  # gcols obtained using: toString(paste("'",colnames(x),"'",sep=""))
  #gcols <- c('hhid', 'total_expenditure', 'toteducexpense', 'tothouserent',  'hsize', 'consu', 'highest_educ', 'age'
  #           , 'expensiveregion', 'popdensity','region','district','litlang',
  #           'isrural', 'isurbanp', 'occupation', 'occupation_rank', 'years_community', 
  #           'housingstatus', 'roomsnum',  'floormaterial', 'cookingfuel', 'is_resident', 'ln_tot_exp', 'year')
  #ag <- unique(allgroupsdat[,gcols])
  #agi <- merge(ag,i,all.x=TRUE,by=c("hhid","year","region", "district"))
  #ydat <- subset(agi,year==yr)
  #for (sn in as.character(unique(ydat$shortname))) { if (!is.na(sn)) { dat <- subset(ydat,shortname==sn); write_dta(dat,paste('c:/temp/dat',yr,'_',sn,'.dta',sep="")) } } 
  #return(agi)
  
  #return(tot)
  c2014 <- ll@load_diary_file(dirprefix = "../",year = 2014, fu = fu, ln =lsms_normalizer, load_cost = FALSE)
  return(c2014)
}

combine_mills_files <- function(years,dirprefix){
  shortnames=c('banana_green', 'banana_ripe', 'beef', 'beer', 'bread', 'brews', 'bunscakes', 'canned_milk', 'cassava_flour', 'cassava_fresh', 'charcoal', 'chicken', 'citrus', 'coconut', 'cooking_oil', 'dried_canned_fish', 'dried_canned_veg', 'eggs', 'electricity', 'fish_seafood', 'fresh_milk', 'gas', 'goat', 'greens', 'kerosene', 'maize_flour', 'maize_grain', 'maize_green', 'mangoes', 'millet_flour', 'millet_grain', 'onion', 'othervegstarch', 'pasta', 'peanuts', 'petrol', 'pork', 'potatoes', 'pulses', 'rice_husked', 'salt', 'sugar', 'sugarcane', 'sweet_potato', 'tea', 'wheat', 'winespirits', 'yam')
  o = NULL
  for (yr in years){
    files <- paste(dirprefix,'/dat',yr,"_",shortnames,"_mills.dta",sep="")
    for ( f in files ){
      print(paste("Reading file:",f))
      o <- rbind(o,read_dta(f))
    }
  }
  o$invmills <- (o$hasex)*o$mills + (1-o$hasex)*o$millsn
  return(o)
}

stored_food_cpi <- function(){
  x <- data.frame(year=c(2010), food_cpi=c(100))
  x <- rbind(x,data.frame(year=c(2012), food_cpi=c(142.070554851631)))
  x <- rbind(x,data.frame(year=c(2014), food_cpi=c(135.859046554779)))
  return(x)
}
calculate_food_cpi <- function(c2010,g2010,o2010){
  
  cc <- merge(c2010,g2010[,c("hhid","total_expenditure")],by=c("hhid"))
  cc <- subset(cc,!is.na(cost))
  cc <- cc %>% mutate(w = cost/total_expenditure)
  s <- ddply(cc[,c("hhid","w","shortname")],.(shortname),summarise,wq = quantile(w,0.8))
  
  foodshortnames <- unique(as.character(subset(lsms_normalizer()@lsms_groups_qualitybased_2010_2012(), is.element(category,c("densefoods","nonfresh","fruitsveg","protein","complements")))$shortname))
  s <- subset(s,is.element(shortname,foodshortnames))
  s <- s[rev(order(s$wq)),]
  #
  mktprices2010 <- ll@load_market_prices(year = 2010, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  fooddiarydata2010      <- subset(c2010,as.integer(as.character(item))>10000)
  hhp <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2010,ohsdata=o2010,ddata=fooddiarydata2010)
  
  #1. get mean quantities of items in base year (2010)
  #B      = sum(p_2010*q_2010)
  #P_2012 = sum (p_2012*q_2010)/ B
  #P_2014 = sum (p_2014*q_2010)/ B
  #lnp <- lnp + data[[shareNames[i]]] * log(data[[priceNames[i]]]/basePrices[i])
  
  groups      <- subset( lsms_normalizer()@lsms_groups_qualitybased_2010_2012(), is.element(shortname,foodshortnames)  )
  hhp <- merge(hhp,groups,by=c("shortname"))
  
  minprices <- ddply(hhp[,c("shortname","price","category","region","district")],.(category,region,district),summarise,min_price=min(price))
  hhp <- merge(minprices,hhp)
  hhp$price_ratio <- with (hhp,price/min_price) 
  hhp$factor<-as.integer(hhp$lwp_unit==1)+as.integer(hhp$lwp_unit==2)/1000.0+as.integer(hhp$lwp_unit==3)+as.integer(hhp$lwp_unit==4)/1000.0+as.integer(hhp$lwp_unit==5)
  hhp$quantity <-with (hhp,lwp*factor)
  basket <- ddply(hhp[,c("shortname","region","district","quantity","price")],.(region,district,shortname), summarise, w = mean(quantity) , price2010 = mean(price))
  #totq <- ddply(unique(hhp[,c("hhid","shortname","category","quantity","price_ratio","min_price","cost")]),.(category,hhid),summarise,totq=sum(quantity), 
  #              qsum = sum (price_ratio*quantity), min_price=unique(min_price),tot_categ_exp = sum(cost))
  #totq$quality <- with(totq,qsum/totq)
  #totq <- merge(o2010[,c("region","district","hhid")], totq,by = c("hhid"))
  #res  <- ddply(totq,.(category),summarise,qall = mean(qsum[!is.na(qsum)]), costall = mean(tot_categ_exp[!is.na(tot_categ_exp)]))
  
  c2012         <- ll@load_diary_file(dirprefix = "../",year = 2012, fu = fu, ln = lsms_normalizer)
  o2012         <- ll@load_ohs_file(year = 2012, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  mktprices2012 <- ll@load_market_prices(year = 2012, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  fooddiarydata2012     <- subset(c2012,as.integer(as.character(item))>10000)
  hhp2012 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2012,ohsdata=o2012,ddata=fooddiarydata2012)
  hhp2012 <- merge(hhp2012,groups,by=c("shortname"))
  prices2012 <- plyr::rename(hhp2012[,c("region","district","shortname","price")],c("price"="price2012"))
  basket <- merge(basket,prices2012, by = c("region","district","shortname"))
  
  c2014         <- ll@load_diary_file(dirprefix = "../",year = 2014, fu = fu, ln = lsms_normalizer)
  o2014         <- ll@load_ohs_file(year = 2014, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  mktprices2014 <- ll@load_market_prices(year = 2014, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  fooddiarydata2014     <- subset(c2014,as.integer(as.character(item))>10000)
  hhp2014 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2014,ohsdata=o2014,ddata=fooddiarydata2014)
  hhp2014 <- merge(hhp2014,groups,by=c("shortname"))
  prices2014 <- plyr::rename(hhp2014[,c("region","district","shortname","price")],c("price"="price2014"))
  basket <- merge(basket,prices2014, by = c("region","district","shortname"))
  
  basket <- unique(basket) 
  basket <- ddply(basket, .(shortname),summarise, w = mean(w) , price2010 = mean(price2010[!is.na(price2010)]), price2012 = mean(price2012[!is.na(price2012)]),price2014 = mean(price2014[!is.na(price2014)]))
  basket <- basket%>% mutate( cost2012 = price2012*w , cost2014 = price2014*w , cost2010= price2010)
  cpi2012 <- mean(with(basket,(price2012*w)/(price2010*w)))
  cpi2014 <- mean(with(basket,(price2014*w)/(price2010*w)))
  df      <- data.frame( year = c(2010,2012,2014) , cpi = c(100,cpi2012*100, cpi2014*100))
  return(df)
  
}

prepare_quality_aids <-function (prepdat){
  #consu age is_resident expensiveregion, occupation_rank
  nonna_highest_educ <-subset(prepdat,!is.na(highest_educ))$highest_educ
  prepdat$educ_rank <- (prepdat$highest_educ- min(nonna_highest_educ))/(max(nonna_highest_educ)-min(nonna_highest_educ))
  prepdat$is_rented <- as.integer(as.integer(as.character(prepdat$housingstatus))==4)
  #consu + educ_rank + age + expensiveregion + occupation_rank + is_resident + is_rented
  prepdat$highest_educ <- NULL
  prepdat$housingstatus <- NULL
  sz = grep("_min_price$",colnames(prepdat))
  if (length(sz)>0){
    for (col in colnames(prepdat)[sz]){
      
      lpcolname <- paste("lp",gsub("_min_price$","",col),sep="")
      print(paste("Writing lp for column:",col, " as ", lpcolname))
      prepdat[,lpcolname] <- log(prepdat[,col]+1e-7)
    }
  }
  
  sz = grep("_quality$",colnames(prepdat))
  
  if (length(sz)>0){
    for (col in colnames(prepdat)[sz]){
      qcolname <- paste("lnV_",gsub("_quality$","",col),sep="")
      print(paste("Writing unit-value for column:",col, " as ",qcolname))
      pricecol <- gsub("_quality$","_min_price",col)
      prepdat[,qcolname] <- log(prepdat[,pricecol] * prepdat[,col]+1e-7)
      
    }
  }
  prepdat[is.na(prepdat$densefoods_quality),]$densefoods_quality <- 0
  prepdat[is.na(prepdat$nonfresh_quality),]$nonfresh_quality <- 0
  prepdat[is.na(prepdat$fruitsveg_quality),]$fruitsveg_quality <- 0
  prepdat[is.na(prepdat$protein_quality),]$protein_quality <- 0
  prepdat[is.na(prepdat$complements_quality),]$complements_quality <- 0

  prepdat[is.na(prepdat$w_densefoods),]$w_densefoods <- 0
  prepdat[is.na(prepdat$w_nonfresh),]$w_nonfresh <- 0
  prepdat[is.na(prepdat$w_fruitsveg),]$w_fruitsveg <- 0
  prepdat[is.na(prepdat$w_protein),]$w_protein <- 0
  prepdat[is.na(prepdat$w_complements),]$w_complements <- 0
  
  #densefoods_quality nonfresh_quality fruitsveg_quality protein_quality complements_quality
  #w_densefoods w_nonfresh w_fruitsveg w_protein w_complements
  prepdat <- prepdat %>% mutate(food_quality=densefoods_quality+nonfresh_quality+fruitsveg_quality+protein_quality+complements_quality)
  prepdat <- prepdat %>% mutate(w_food= w_densefoods+w_nonfresh+w_fruitsveg+w_protein+w_complements)
  prepdat$lnV_food <- with(prepdat,log(food_quality+1))
  
  foodcpilog <- ( stored_food_cpi() %>% mutate(lpfood = log(food_cpi)) ) [,c("year","lpfood")]
  prepdat <- merge(foodcpilog,prepdat,by=c("year"))
  
  return(prepdat)
}

prepare_mills_aids <-function (allgroupsdat, itemsw, pricesi){
  #consu age is_resident expensiveregion, occupation_rank
  prepdat <- allgroupsdat[,c("hhid","year", "highest_educ","housingstatus")]
  nonna_highest_educ <-subset(allgroupsdat,!is.na(highest_educ))$highest_educ
  prepdat$educ_rank <- (allgroupsdat$highest_educ- min(nonna_highest_educ))/(max(nonna_highest_educ)-min(nonna_highest_educ))
  prepdat$is_rented <- as.integer(as.integer(as.character(allgroupsdat$housingstatus))==4)
  #consu + educ_rank + age + expensiveregion + occupation_rank + is_resident + is_rented
  prepdat$highest_educ <- NULL
  prepdat$housingstatus <- NULL
  mdat <- merge(prepdat,itemsw,by=c("hhid","year"))
  mdatp <- merge(mdat,pricesi,by=c("hhid","year"))
  sz = grep("^price_",colnames(mdatp))
  if (length(sz)>0){
    for (col in colnames(mdatp)[sz]){
      print(paste("Writing lp for column:",col))
      lpcolname <- paste("lp",gsub("price_","",col),sep="")
      mdatp[,lpcolname] <- log(mdatp[,col]+1e-7)
    }
  }
  return(mdatp)
}

#Use the following with tot to get mean,median budget shares
#an <- ddply(tot,.(year,shortname),summarise,mw = mean(w),medw=quantile(w[!is.na(w)],0.8))
sum_items <- function(millsdata){
  print("Checking for duplicates in household expenditure on items")
  itemcosts <- unique(millsdata[,c("hhid","shortname","cost","year")])
  itemcosts[is.na(itemcosts$cost),]$cost <- 0
  
  print("Summing up items expenditure")
  totsn    <- ddply(itemcosts,.(shortname,hhid,year),summarise, item_exp = sum(cost))
  print("Summing up diary expenditure")
  totdiary <- ddply(itemcosts,.(hhid,year),summarise, totdiary = sum(cost))
  print("Merging total and item expenditures")
  tot      <- merge(totdiary,totsn,by=c("hhid","year"))
  
  tot$w       <- with(tot,item_exp/totdiary)
  tottospread <- tot %>% mutate(id=paste(tot$hhid,tot$year,sep="-"))
  mapping     <- unique(tottospread[,c("hhid","year","id")])
  
  stot  <- tottospread[,c("id","shortname","w")] %>% spread(shortname,w)
  stot[is.na(stot)] <- 0
  colnames (stot) <- as.character(sapply(colnames(stot), function(x) { if(is.element(x,c("id"))) {x} else {paste("w_",x,sep="")} }))
  totm      <- merge(mapping,stot,by="id")
  totm$id   <- NULL
  return(totm)
}

get_categories <- function(){
  return (c("densefoods","nonfresh","fruitsveg","protein","alcohol","complements","energy","household","transport"))
}

load_group <- function(dat,year,categories){
  if (missing(categories)){
  categories <- get_categories()
  }
  if (missing(dat)){
    dat <- ll@group_expenditure(year = year, dirprefix = "../",
                                fu = fu , ln = lsms_normalizer, lgc=lgc,
                                basis = "quality", categoryNames = categories,returnBeforeGrouping = FALSE,
                                ld = ldat, minConsumerNumber = 5,use_market_prices=TRUE, use_diary_costs=FALSE, 
                                ignore_non_price_for_quality=FALSE)
  }
  for (categ in categories){
    dat[,paste("w_",categ,sep="")] <- dat[,paste(categ,"_tot_categ_exp",sep="")]/dat$total_expenditure
  }
  
  #c2010<- ll@load_diary_file(dirprefix = "../",year = 2010, fu = fu, ln = lsms_normalizer)
  #o2010 <- ll@load_ohs_file(year = 2010, dirprefix = "../",fu = fu, ln=lsms_normalizer )
  #g <- ll@group_collect(year = 2010, dirprefix = "../",categoryName = "densefoods",fu = fu, ln =lsms_normalizer, lgc = lgc, ohs = o2010, hh = c2010, basis = "quality", use_market_prices = TRUE)
  
  return(dat)
}

#w2010$lpnonfresh<-with(w2010,log(nonfresh_min_price))
#w2010$lpdensefoods<-with(w2010,log(densefoods_min_price))
#w2010$lpcomplements<-with(w2010,log(complements_min_price))
#w2010$lpfruitsveg<-with(w2010,log(fruitsveg_min_price))
#w2010$lpprotein<-with(w2010,log(protein_min_price))
#w2010$lpalcohol<-with(w2010,log(alcohol_min_price))

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


plot_weights <- function(dat,categories,set_device_off,config_pair){
  if (set_device_off){
    dev.off()
  }
  par(mfrow=config_pair)
  for (categ in categories)
  {
    colname <- paste("w_",categ,sep="")
    hist(dat[,c(colname)],breaks=100,main=categ,xlab=colname)
  }
}
# EXAMPLE: 


get_inverse_mills_data <- function(allgroupsdat,dirprefix,years){
  if (missing(allgroupsdat)){
    g2010 <- load_group(year=2010)
    
    g2012 <- load_group(year=2012) 
    
    g2014 <- load_group(year=2014)
    g2010$year <- 2010
    g2012$year <- 2012
    g2014$year <- 2014
    commoncols <- intersect(intersect(colnames(g2010),colnames(g2012)),colnames(g2014))
    allgroupsdat <- rbind(g2010[,commoncols],g2012[,commoncols])
    allgroupsdat <- rbind(allgroupsdat,g2014[,commoncols])
    
  }
  
  outdat<- NULL
  
  for (yr in years){
    cdat <- ll@load_diary_file(dirprefix = dirprefix, year = yr, fu = fu, ln = lsms_normalizer)
    odat <- ll@load_ohs_file(year = yr, dirprefix = dirprefix, fu = fu, ln= lsms_normalizer)
    
    imdat <- NULL
    aggprices <- NULL
    if (yr == 2010){
      icf = lsms_normalizer()@items_codes_2010
    } else if (yr == 2012){
      icf = lsms_normalizer()@items_codes_2012
    } else if (yr == 2014){
      icf = lsms_normalizer()@items_codes_2014
    } else {
      print("Unknown year")
    }
    
    for (catg in setdiff(get_categories(), c("household","transport"))){
      #for (catg in c("fruitsveg","densefoods")){
      
      im <- inverse_mills(item_codes_func = icf, diarydata = cdat, ohsdata = odat, year = yr, 
                          groups = lsms_normalizer()@lsms_groups_qualitybased_2010_2012(),categ = catg)
      
      im$year <- yr
      if (!is.element("category",colnames(im))){
        stop("inverse mills are assumed to be grouped by category")
      }
      print (paste("Spreading data for category - ",catg))
      pricedat <- unique(im[,c("shortname","price","region","district")])  %>% mutate(id=paste(as.character(region),as.character(district),sep="-"))
      #prices <- unique(im[,c("shortname","price","hhid")]) %>% spread(shortname,price)
      
      prices   <- pricedat[,c("shortname","id","price")] %>% spread(shortname,price)
      #colnames (prices) <- as.character(sapply(colnames(prices), function(x) { if(is.element(x,c("hhid"))) {x} else {paste("price_",x,sep="")} }))
      colnames (prices) <- as.character(sapply(colnames(prices), function(x) { if(is.element(x,c("id"))) {x} else {paste("price_",x,sep="")} }))
      prices <- merge(subset(unique(pricedat[,c("region","district","id")]),!is.na(region)),prices,by="id")
      prices$id <- NULL
      if (is.null(aggprices) ) {
        aggprices <- prices
      } else {
        aggprices <- merge(prices,aggprices,by=c("region","district"),all=TRUE)
      }
      imdat <- rbind(imdat,im)
      
    }
    
    k<- merge(imdat,aggprices,by=c("region","district"),all.x=TRUE)
    if (!is.null(outdat)) {
      commoncols <- intersect(colnames(k),colnames(outdat))
      
      missing_in_k <- setdiff(colnames(outdat),colnames(k))
      if (length(missing_in_k)>0) {
        print(paste("Following columns were missing in year",yr,"-",toString(missing_in_k)))
      }
      
      missing_in_outdat <- setdiff(colnames(k),colnames(outdat))
      if (length(missing_in_outdat)>0) {
        print(paste("Following columns were ignored for year",yr," due to them not being there in the past years - ",toString(missing_in_outdat)))
      }
      
      outdat <- rbind(outdat[,commoncols],k[,commoncols])
      
    } else {
      outdat <- k
    }
    
  }
  
  return(outdat)
  
}
inverse_mills <- function(item_codes_func,diarydata,ohsdata,year,groups,categ){
  
  filtereddiary <- subset(diarydata,is.element ( shortname , subset(groups,category==categ)$shortname) )
  x<-merge(expand.grid(hhid=unique(filtereddiary$hhid),shortname=unique(filtereddiary$shortname)),filtereddiary,all.x=TRUE)[,c("shortname","hhid","cost")]
  x <- merge(plyr::rename(item_codes_func()[,c("shortname","code")],c("code"="item") ),x,all.y=TRUE )
  x[is.na(x$cost),]$cost <- 0
  x$hasex                <- as.integer(with(x,cost>0))
  x$cost                 <- NULL
  extendeddata <- merge(x,filtereddiary[,c("hhid","shortname","cost","item","lwp_unit","lwp","own_unit","own","gift_unit","gift")], 
                        by = c("shortname","hhid","item"),
                        all.x=TRUE)
  
  
  im <- ll@group_collect(year = year, dirprefix = "../",categoryName = categ,
                         fu = fu, ln=lsms_normalizer,lgc = lgc, ld = ldat, ohs = ohsdata, hh = extendeddata, basis = "quality",
                         use_market_prices = TRUE,
                         return_before_agg = TRUE)
  
  
  return(im)
  
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
