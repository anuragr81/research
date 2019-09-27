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

run_test <- function() {

  # gcols obtained using: toString(paste("'",colnames(x),"'",sep=""))
  gcols <- c('hhid', 'total_expenditure', 'toteducexpense', 'tothouserent',  'hsize', 'consu', 'highest_educ', 'age'
             , 'expensiveregion', 'popdensity','region','district','litlang',
             'isrural', 'isurbanp', 'occupation', 'occupation_rank', 'years_community', 
             'housingstatus', 'roomsnum',  'floormaterial', 'cookingfuel', 'is_resident', 'ln_tot_exp', 'year')
  #ag <- unique(allgroupsdat[,gcols])
  #agi <- merge(ag,i,all.x=TRUE,by=c("hhid","year","region", "district"))
  #ydat <- subset(agi,year==yr)
  #for (sn in as.character(unique(ydat$shortname))) { if (!is.na(sn)) { dat <- subset(ydat,shortname==sn); write_dta(dat,paste('c:/temp/dat',yr,'_',sn,'.dta',sep="")) } } 
  #return(agi)
  
  #return(tot)
  
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

load_group <- function(dat,year){
  
  categories <- get_categories()
  if (missing(dat)){
    dat <- ll@group_expenditure(year = year, dirprefix = "../",
                                fu = fu , ln = lsms_normalizer, lgc=lgc,
                                basis = "quality", categoryNames = categories,returnBeforeGrouping = FALSE,
                                ld = ldat, minConsumerNumber = 5,use_market_prices=TRUE)
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
