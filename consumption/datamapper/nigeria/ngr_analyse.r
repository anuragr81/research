
setwd('c:/local_files/research/consumption/datamapper/')
source('translation/frameutils.R');
source('nigeria/ngr_normaliser.r'); 
source('nigeria/ngr_loader.r'); 
nl <- ngr_loader(fu,ngr_normaliser,lgc)
source('lsms/lsms_normalizer.r');
source('lsms/lsms_loader.r');llc=lsms_loader(fu=fu,ln=lsms_normalizer,lgc=lgc)

test <- function(){
  #dat <- nl@load_diary_file("../",2010,fu, ngr_normaliser, load_cost=TRUE)
  ohs <- nl@load_ohs_file(year = 2010, dirprefix = "../",fu = fu,ngrn = ngr_normaliser )
  return(ohs)
}

get_ngr_categories <- function(){
  return (c("densefoods","nonfresh","fruitsveg","protein","alcohol","complements","energy","household","transport"))
}

minimum_household_needs_wo_usage <- function(ll, c2010, c2012, c2014, o2010, o2012, o2014, mktprices2010,mktprices2012,mktprices2014, housing_fee){
  # 
  DAYS_IN_YEAR<- 365
  MONTHS_IN_YEAR <- 12
  
  ############# FOOD ##############
  
  fooddiarydata2010      <- subset(c2010,as.integer(as.character(item))>10000)
  fooddiarydata2012      <- subset(c2012,as.integer(as.character(item))>10000)
  fooddiarydata2014      <- subset(c2014,as.integer(as.character(item))>10000)

  
  mktprices2010 <- ll@load_market_prices(year = 2010, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  
  
  mktprices2012 <- ll@load_market_prices(year = 2012, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  
  mktprices2014 <- ll@load_market_prices(year = 2014, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  
  hhp2010 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2010,ohsdata=o2010,ddata=fooddiarydata2010)
  hhp2010 <- merge(lsms_normalizer()@categories_needs_based(),hhp2010)
  regionfoodprice2010 <- hhp2010 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2010 <- ddply( unique(regionfoodprice2010[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2010 <- ddply(basket_constituent_costs2010, .(region,district), summarise, basket_cost = sum(rec_cost))
  
  hsize2010              <- unique(merge(o2010[,c("hhid","region","district")], ll@get_hsize(o2010), by = c("hhid")))
  
  # multiple prices with the number of members in the family to arrive at average cost (prices index could be calculated as a consu weighted average)
  foodbasket_costs2010 <- merge(hsize2010,basket_costs2010, by = c("region","district")) %>% mutate(familybasket_cost = consu*basket_cost)
  
  
  hhp2012 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2012,ohsdata=o2012,ddata=fooddiarydata2012)
  hhp2012 <- merge(lsms_normalizer()@categories_needs_based(),hhp2012)
  regionfoodprice2012 <- hhp2012 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2012 <- ddply( unique(regionfoodprice2012[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2012 <- ddply(basket_constituent_costs2012, .(region,district), summarise, basket_cost = sum(rec_cost)) 
  
  hsize2012              <- unique(merge(o2012[,c("hhid","region","district")], ll@get_hsize(o2012), by = c("hhid")))
  foodbasket_costs2012 <- merge(hsize2012,basket_costs2012, by = c("region","district")) %>% mutate(familybasket_cost = consu*basket_cost) 
  
  hhp2014 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2014,ohsdata=o2014,ddata=fooddiarydata2014)
  hhp2014 <- merge(lsms_normalizer()@categories_needs_based(),hhp2014)
  
  regionfoodprice2014 <- hhp2014 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2014 <- ddply( unique(regionfoodprice2014[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2014 <- ddply(basket_constituent_costs2014, .(region,district), summarise, basket_cost = sum(rec_cost))
  
  hsize2014              <- unique(merge(o2014[,c("hhid","region","district")], ll@get_hsize(o2014), by = c("hhid")))
  foodbasket_costs2014 <- merge(hsize2014,basket_costs2014, by = c("region","district")) %>% mutate(familybasket_cost = consu*basket_cost) 
  
  ############# ENERGY ##############
  #energy - load cheapest energy prices (electricity prices are not available locally - so just take minimum of kerosene and charcoal)
  # we can't use asset-level recqs any more.
  energy_groups_elems <- subset(lsms_normalizer()@categories_needs_based(),is.element(assetlevel,c("kerosene_lighting","kerosene_cooking")) & is.element(shortname,c("kerosene")) )
  energy_groups <- ddply(energy_groups_elems,.(shortname,category,group),summarise,recqs=sum(recq))
  if (length(energy_groups$recqs)>1){
    stop("Non-kerosene min-cost fuel not supported")
  }
  
  miscdiarydata2010  <- subset(c2010,is.element(shortname,energy_groups$shortname))
  hhpm2010       <- ll@add_market_price_to_misc_diary (curyear = 2010, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = energy_groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2010,ohsdata=o2010,ddata=miscdiarydata2010)
  if (setequal(unique(paste(subset(hhpm2010, shortname=="kerosene")$region,subset(hhpm2010, shortname=="kerosene")$district)), unique(paste(hhpm2010$region,hhpm2010$district)))==FALSE){
    stop("Kerosene not available in all regions")
  }
  
  energy_prices2010 <- merge(energy_groups,hhpm2010)
  
  ###
  miscdiarydata2012  <- subset(c2012,is.element(shortname,subset(energy_groups , category =="energy")$shortname))
  
  hhpm2012       <- ll@add_market_price_to_misc_diary (curyear = 2012, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = energy_groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2012,ohsdata=o2012,ddata=miscdiarydata2012)
  if (setequal(unique(paste(subset(hhpm2012, shortname=="kerosene")$region,subset(hhpm2012, shortname=="kerosene")$district)), unique(paste(hhpm2012$region,hhpm2012$district)))==FALSE){
    stop("Kerosene not available in all regions")
  }
  
  energy_prices2012 <- merge(energy_groups,hhpm2012)
  
  ####
  
  miscdiarydata2014  <- subset(c2014,is.element(shortname,subset(energy_groups , category =="energy")$shortname))
  hhpm2014       <- ll@add_market_price_to_misc_diary (curyear = 2014, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = energy_groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2014,ohsdata=o2014,ddata=miscdiarydata2014)
  if (setequal(unique(paste(subset(hhpm2014, shortname=="kerosene")$region,subset(hhpm2014, shortname=="kerosene")$district)), unique(paste(hhpm2014$region,hhpm2014$district)))==FALSE){
    stop("Kerosene not available in all regions")
    #median(subset(mktprices2014, shortname=="kerosene")$median_price)
  }
  
  energy_prices2014 <- merge(energy_groups,hhpm2014)
  
  
  ### BASIC NEEDS 
  needs2010 <- (merge( (energy_prices2010 [,c("region","district","hhid","price","recqs")]) %>% mutate( energy_cost = recqs*price) ,foodbasket_costs2010, by = c("hhid","region","district") ) %>% mutate(basic_needs_cost=energy_cost+familybasket_cost))
  needs2012 <- (merge( (energy_prices2012 [,c("region","district","hhid","price","recqs")]) %>% mutate( energy_cost = recqs*price) ,foodbasket_costs2012, by = c("hhid","region","district") ) %>% mutate(basic_needs_cost=energy_cost+familybasket_cost))
  needs2014 <- (merge( (energy_prices2014 [,c("region","district","hhid","price","recqs")]) %>% mutate( energy_cost = recqs*price) ,foodbasket_costs2014, by = c("hhid","region","district") ) %>% mutate(basic_needs_cost=energy_cost+familybasket_cost))
  
  # BASIC NEEDS PRICES
  prices2010 <- merge(ddply(foodbasket_costs2010 , .(region,district), summarise, food_price = sum(familybasket_cost)/sum(consu)), ddply(energy_prices2010 , .(region,district), summarise, energy_price = sum(recqs*price)/sum(recqs)) , by = c("region","district")) %>% mutate(needs_price = food_price + energy_price)
  prices2012 <- merge(ddply(foodbasket_costs2012 , .(region,district), summarise, food_price = sum(familybasket_cost)/sum(consu)), ddply(energy_prices2012 , .(region,district), summarise, energy_price = sum(recqs*price)/sum(recqs)) , by = c("region","district")) %>% mutate(needs_price = food_price + energy_price)
  prices2014 <- merge(ddply(foodbasket_costs2014 , .(region,district), summarise, food_price = sum(familybasket_cost)/sum(consu)), ddply(energy_prices2014 , .(region,district), summarise, energy_price = sum(recqs*price)/sum(recqs)) , by = c("region","district")) %>% mutate(needs_price = food_price + energy_price)
  
  needs2010 <- merge(needs2010,prices2010,by=c("region","district"))
  needs2012 <- merge(needs2012,prices2010,by=c("region","district"))
  needs2014 <- merge(needs2014,prices2010,by=c("region","district"))
  
  select_cols      <- c("hhid","basic_needs_cost","needs_price")
  r                <- data.frame()
  r                <- rbind(r, needs2010[,select_cols] %>% mutate(year=2010))
  r                <- rbind(r, needs2012[,select_cols] %>% mutate(year=2012)) 
  r                <- rbind(r, needs2014[,select_cols] %>% mutate(year=2014))
  
  res              <- list()
  res[["df"]]      <- r
  res[["df2010"]]  <- needs2010
  res[["df2012"]]  <- needs2012
  res[["df2014"]]  <- needs2014
  return(res)
}



load_group_ngr <- function(dat,year,categories){
  if (missing(categories)){
    categories <- get_ngr_categories()
  }
  if (missing(dat)){
    mdat <- nl@load_market_prices (dirprefix = "../",year = year,fu = fu ,ngrn= ngr_normaliser)
    hh <- nl@load_diary_file(dirprefix = "../",year = year, fu = fu, ngrn = ngr_normaliser,load_cost = TRUE)
    ohs <- nl@load_ohs_file(year = year, dirprefix = "../",fu = fu, ngrn = ngr_normaliser)
    dat <- llc@group_expenditure(hh = hh, ohs = ohs, year = year, dirprefix = "../",
                                fu = fu , ln = ngr_normaliser, lgc=lgc,
                                basis = "quality", categoryNames = categories,returnBeforeGrouping = FALSE,
                                ld = ldat, minConsumerNumber = 5,use_market_prices=TRUE, use_diary_costs=FALSE, 
                                ignore_non_price_for_quality=TRUE)
  }
  for (categ in categories){
    dat[,paste("w_",categ,sep="")] <- dat[,paste(categ,"_tot_categ_exp",sep="")]/dat$total_expenditure
  }
  
  #c2010<- ll@load_diary_file(dirprefix = "../",year = 2010, fu = fu, ln = lsms_normalizer)
  #o2010 <- ll@load_ohs_file(year = 2010, dirprefix = "../",fu = fu, ln=lsms_normalizer )
  #g <- ll@group_collect(year = 2010, dirprefix = "../",categoryName = "densefoods",fu = fu, ln =lsms_normalizer, lgc = lgc, ohs = o2010, hh = c2010, basis = "quality", use_market_prices = TRUE)
  
  return(dat)
}