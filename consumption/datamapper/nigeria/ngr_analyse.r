
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

  
  mktprices2010 <- nl@load_market_prices(year = 2010, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  
  
  mktprices2012 <- nl@load_market_prices(year = 2012, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  
  mktprices2014 <- nl@load_market_prices(year = 2014, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  
  hhp2010 <- nl@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2010,ohsdata=o2010,ddata=fooddiarydata2010)
  hhp2010 <- merge(lsms_normalizer()@categories_needs_based(),hhp2010)
  regionfoodprice2010 <- hhp2010 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2010 <- ddply( unique(regionfoodprice2010[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2010 <- ddply(basket_constituent_costs2010, .(region,district), summarise, basket_cost = sum(rec_cost))
  
  hsize2010              <- unique(merge(o2010[,c("hhid","region","district")], nl@get_hsize(o2010), by = c("hhid")))
  
  # multiple prices with the number of members in the family to arrive at average cost (prices index could be calculated as a consu weighted average)
  foodbasket_costs2010 <- merge(hsize2010,basket_costs2010, by = c("region","district")) %>% mutate(familybasket_cost = consu*basket_cost)
  
  
  hhp2012 <- nl@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2012,ohsdata=o2012,ddata=fooddiarydata2012)
  hhp2012 <- merge(lsms_normalizer()@categories_needs_based(),hhp2012)
  regionfoodprice2012 <- hhp2012 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2012 <- ddply( unique(regionfoodprice2012[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2012 <- ddply(basket_constituent_costs2012, .(region,district), summarise, basket_cost = sum(rec_cost)) 
  
  hsize2012              <- unique(merge(o2012[,c("hhid","region","district")], nl@get_hsize(o2012), by = c("hhid")))
  foodbasket_costs2012 <- merge(hsize2012,basket_costs2012, by = c("region","district")) %>% mutate(familybasket_cost = consu*basket_cost) 
  
  hhp2014 <- nl@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2014,ohsdata=o2014,ddata=fooddiarydata2014)
  hhp2014 <- merge(lsms_normalizer()@categories_needs_based(),hhp2014)
  
  regionfoodprice2014 <- hhp2014 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2014 <- ddply( unique(regionfoodprice2014[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2014 <- ddply(basket_constituent_costs2014, .(region,district), summarise, basket_cost = sum(rec_cost))
  
  hsize2014              <- unique(merge(o2014[,c("hhid","region","district")], nl@get_hsize(o2014), by = c("hhid")))
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
  hhpm2010       <- nl@add_market_price_to_misc_diary (curyear = 2010, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = energy_groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2010,ohsdata=o2010,ddata=miscdiarydata2010)
  if (setequal(unique(paste(subset(hhpm2010, shortname=="kerosene")$region,subset(hhpm2010, shortname=="kerosene")$district)), unique(paste(hhpm2010$region,hhpm2010$district)))==FALSE){
    stop("Kerosene not available in all regions")
  }
  
  energy_prices2010 <- merge(energy_groups,hhpm2010)
  
  ###
  miscdiarydata2012  <- subset(c2012,is.element(shortname,subset(energy_groups , category =="energy")$shortname))
  
  hhpm2012       <- nl@add_market_price_to_misc_diary (curyear = 2012, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = energy_groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2012,ohsdata=o2012,ddata=miscdiarydata2012)
  if (setequal(unique(paste(subset(hhpm2012, shortname=="kerosene")$region,subset(hhpm2012, shortname=="kerosene")$district)), unique(paste(hhpm2012$region,hhpm2012$district)))==FALSE){
    stop("Kerosene not available in all regions")
  }
  
  energy_prices2012 <- merge(energy_groups,hhpm2012)
  
  ####
  
  miscdiarydata2014  <- subset(c2014,is.element(shortname,subset(energy_groups , category =="energy")$shortname))
  hhpm2014       <- nl@add_market_price_to_misc_diary (curyear = 2014, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = energy_groups, lgc=lgc,
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
  
  #c2010<- nl@load_diary_file(dirprefix = "../",year = 2010, fu = fu, ln = lsms_normalizer)
  #o2010 <- nl@load_ohs_file(year = 2010, dirprefix = "../",fu = fu, ln=lsms_normalizer )
  #g <- nl@group_collect(year = 2010, dirprefix = "../",categoryName = "densefoods",fu = fu, ln =lsms_normalizer, lgc = lgc, ohs = o2010, hh = c2010, basis = "quality", use_market_prices = TRUE)
  
  return(dat)
}

mapping_hhids_2010_2012 <- function(o2012){
  return ( plyr::rename(subset(unique(o2012[,c("hhid","hhid2010")]), !is.na(hhid2010)) , c("hhid"="hhid2012"))) 
}
mapping_hhids_2012_2015 <- function(o2015){
  return ( plyr::rename(subset(unique(o2015[,c("hhid","hhid2012")]), !is.na(hhid2012)) , c("hhid"="hhid2015")))
}

init_data <- function(){
  
  o2010 <- nl@load_ohs_file(year = 2010, dirprefix = "../",fu=fu, ngrn=ngr_normaliser) ; 
  o2012 <- nl@load_ohs_file(year = 2012, dirprefix = "../",fu=fu, ngrn=ngr_normaliser) ; 
  o2015 <- nl@load_ohs_file(year = 2015, dirprefix = "../",fu=fu, ngrn=ngr_normaliser) ;
  a2010 <- nl@read_assets_file(year = 2010, dirprefix = "../",fu = fu, ngrn = ngr_normaliser) ; 
  a2012 <- nl@read_assets_file(year = 2012, dirprefix = "../",fu = fu, ngrn = ngr_normaliser) ; 
  a2015 <- nl@read_assets_file(year = 2015, dirprefix = "../",fu = fu, ngrn = ngr_normaliser) ; 
  c2010 <- nl@load_diary_file(dirprefix = "../",year = 2010, fu = fu, ngrn =ngr_normaliser, load_cost = TRUE)
  c2012 <- nl@load_diary_file(dirprefix = "../",year = 2012, fu = fu, ngrn =ngr_normaliser, load_cost = TRUE)
  c2015 <- nl@load_diary_file(dirprefix = "../",year = 2015, fu = fu, ngrn =ngr_normaliser, load_cost = TRUE)
  
}

get_nonparametric_df <- function(ll,food_analysis,o2012,a2012){
  # Don't include education or housing expenses - because they're part of needs anyways
  inc_houserent = F
  inc_educexpense = F
  #food_analysis = F
  
  educ_pivot <- 3
  occup_pivot <- 2
  
  
  hhids2010_2012 <- mapping_hhids_2010_2012(o2012)
  
  asset_mtms_2012 = asset_mtms(a2012,"bed","2012")
  asset_mtms_2010 <- plyr::rename(merge(hhids2010_2012,asset_mtms_2012),c("mtm.2012"="mtm.2010","cost.2012"="cost.2010","number.2012"="number.2010"))[,c("hhid2010","hhid2012","shortname","number.2010","mtm.2010","cost.2010")]
  asset_mtms_2014 = asset_mtms(a2014,"bed","2014")
  
  assetslog2010 <- ddply(asset_mtms_2010,.(hhid2010),summarise,lnA0=log(sum(number.2010*mtm.2010)+1e-7),A0=(sum(number.2010*mtm.2010)))
  assetslog2012 <- ddply(asset_mtms_2012,.(hhid2012),summarise,lnA0=log(sum(number.2012*mtm.2012)+1e-7),A0=sum(number.2012*mtm.2012))
  assetslog2014 <- ddply(asset_mtms_2014,.(hhid2014),summarise,lnA0=log(sum(number.2014*mtm.2014)+1e-7),A0=sum(number.2014*mtm.2014))
  
  
  if (food_analysis==T){
    all_costs_considered <- lsms_normalizer()@categories_non_basic_wassets(include_food=T)
    food_costs_group <- subset(all_costs_considered,is.element(group,c("needs")))$shortname
    excess_costs_group <- subset(all_costs_considered,is.element(group,c("excess")))$shortname
    
    x2010 <- plyr::rename(ll@get_total_expenditures(hh = c2010, ohs = o2010, include_education=inc_educexpense, include_houserent = inc_houserent), c("total_expenditure"="x"))
    x2012 <- plyr::rename(ll@get_total_expenditures(hh = c2012, ohs = o2012, include_education=inc_educexpense, include_houserent = inc_houserent),c("total_expenditure"="x"))
    x2014 <- plyr::rename(ll@get_total_expenditures(hh = c2014, ohs = o2014, include_education=inc_educexpense, include_houserent = inc_houserent),c("total_expenditure"="x"))
    
    
    hsizex2010 <- merge(ll@get_hsize(o2010),x2010,by=c("hhid"))
    hsizex2012 <- merge(ll@get_hsize(o2012),x2012,by=c("hhid"))
    hsizex2014 <- merge(ll@get_hsize(o2014),x2014,by=c("hhid"))
    
    k2010_tot <- get_split_costs(categs_a = food_costs_group,categs_b = excess_costs_group,dat = c2010, group_field = "shortname")
    k2012_tot <- get_split_costs(categs_a = food_costs_group,categs_b = excess_costs_group,dat = c2012, group_field = "shortname")
    k2014_tot <- get_split_costs(categs_a = food_costs_group,categs_b = excess_costs_group,dat = c2014, group_field = "shortname")
    
    k2010 <- (merge(k2010_tot,hsizex2010,by=c("hhid")) %>% mutate(cost_a=cost_a/hsize) %>% mutate(cost_b=cost_b/hsize))
    k2012 <- (merge(k2012_tot,hsizex2012,by=c("hhid")) %>% mutate(cost_a=cost_a/hsize) %>% mutate(cost_b=cost_b/hsize))
    k2014 <- (merge(k2014_tot,hsizex2014,by=c("hhid")) %>% mutate(cost_a=cost_a/hsize) %>% mutate(cost_b=cost_b/hsize))
    
    k2010 <- k2010 %>% mutate(w_a = cost_a/(cost_a+cost_b)) %>% mutate(w_b = cost_b/(cost_a+cost_b))
    k2012 <- k2012 %>% mutate(w_a = cost_a/(cost_a+cost_b)) %>% mutate(w_b = cost_b/(cost_a+cost_b)) 
    k2014 <- k2014 %>% mutate(w_a = cost_a/(cost_a+cost_b)) %>% mutate(w_b = cost_b/(cost_a+cost_b)) 
    
    ka2010 <- (merge(assetslog2010,plyr::rename(k2010,c("hhid"="hhid2010")),by=c("hhid2010"))) %>% mutate (year=2010) %>% mutate( logx=log(x)) %>% mutate( logxc=log(x/consu))
    ka2010 <- ka2010[,setdiff(colnames(ka2010),c("consu","hsize"))]
    ka2012 <- (merge(assetslog2012,plyr::rename(k2012,c("hhid"="hhid2012")),by=c("hhid2012"))) %>% mutate (year=2012) %>% mutate( logx=log(x)) %>% mutate( logxc=log(x/consu))
    ka2012 <- ka2012[,setdiff(colnames(ka2012),c("consu","hsize"))]
    ka2014 <- (merge(assetslog2014,plyr::rename(k2014,c("hhid"="hhid2014")),by=c("hhid2014"))) %>% mutate (year=2014) %>% mutate( logx=log(x)) %>% mutate( logxc=log(x/consu))
    ka2014 <- ka2014[,setdiff(colnames(ka2014),c("consu","hsize"))]
    
  } else {
    all_costs <- lsms_normalizer()@categories_non_basic_wassets(include_food=T)
    #asset purchases and asset-bearing costs are not considered
    needs_and_excess_costs <- subset(all_costs, is.element(group,c("excess","needs")))
    ne2010 <- plyr::rename(ddply(subset(c2010,is.element(shortname,needs_and_excess_costs$shortname)),.(hhid),summarise,cost_ne=sum(cost)),c("hhid"="hhid2010"))
    ne2012 <- plyr::rename(ddply(subset(c2012,is.element(shortname,needs_and_excess_costs$shortname)),.(hhid),summarise,cost_ne=sum(cost)),c("hhid"="hhid2012"))
    ne2014 <- plyr::rename(ddply(subset(c2014,is.element(shortname,needs_and_excess_costs$shortname)),.(hhid),summarise,cost_ne=sum(cost)),c("hhid"="hhid2014"))
    
  }
  #
  perception_columns <- c("life_perception"="hh_life_perception" , "finance_perception"="hh_finance_perception", "richness_perception"="hh_richness_perception","housing_perception"="hh_housing_perception","health_perception"="hh_health_perception")
  hhead_columns <- c("hhid"="hhid","years_community"="hh_years_community","age"="hh_age","highest_educ"="hh_highest_educ","occupation_rank"="hh_occupation_rank","litlang"="hh_litlang")
  #total consumption
  relevant_fields <-c("hhid","region","district","ward","isrural","expensiveregion","S","E","population")
  # 2010
  ohs2010 <- subset(o2010,!is.na(region))
  hs2010 <- unique(merge(unique(ohs2010[,relevant_fields]), ll@get_hsize(ohs2010), by = c("hhid")))
  chosenchars2010 <- ddply(ohs2010[,c("hhid","education_rank","occupation_rank","litlang")],.(hhid),summarise,max_education_rank = choose_max_education_rank(education_rank) , max_occupation_rank = max(occupation_rank) , litlang = choose_max_litlang(litlang))
  #  perception_columns
  hhead2010 <- plyr::rename(subset(o2010,household_status==1)[,names(hhead_columns)],hhead_columns )
  chosencharshead2010 <- merge(chosenchars2010,hhead2010, all.x=T)
  hswithchars2010 <- merge(hs2010,chosencharshead2010,all.x = T)
  # -6.727135 39.14395
  
  
  # 2012
  ohs2012 <- subset(o2012,!is.na(region))
  hs2012 <- unique(merge(unique(ohs2012[,relevant_fields]), ll@get_hsize(ohs2012), by = c("hhid")))
  chosenchars2012 <- ddply(ohs2012[,c("hhid","education_rank","occupation_rank","age","litlang")],.(hhid),summarise,max_education_rank = choose_max_education_rank(education_rank) , max_occupation_rank = max(occupation_rank) , litlang = choose_max_litlang(litlang))
  
  if (length(setdiff(names(perception_columns),colnames(o2012)))==0){
    hhead_columns_w_percept <- c(hhead_columns,perception_columns)
  }
  
  hhead2012 <- plyr::rename(subset(o2012,household_status==1)[,names(hhead_columns_w_percept)],hhead_columns_w_percept )
  
  chosencharshead2012 <- merge(chosenchars2012,hhead2012, all.x=T)
  hswithchars2012 <- merge(hs2012,chosencharshead2012,all.x = T)
  
  #2014
  ohs2014 <- subset(o2014,!is.na(region))
  hs2014 <- unique(merge(unique(ohs2014[,relevant_fields]), ll@get_hsize(ohs2014), by = c("hhid")))
  chosenchars2014 <- ddply(ohs2014[,c("hhid","education_rank","occupation_rank","age","litlang")],.(hhid),summarise,max_education_rank = choose_max_education_rank(education_rank) , max_occupation_rank = max(occupation_rank) , litlang = choose_max_litlang(litlang))
  
  hhead2014 <- plyr::rename(subset(o2014,household_status==1)[,names(hhead_columns)],hhead_columns )
  
  chosencharshead2014 <- merge(chosenchars2014,hhead2014, all.x=T)
  
  hswithchars2014 <- merge(hs2014,chosencharshead2014,all.x = T)
  
  
  #a<-merge(plyr::rename(i2010,c("hhid"="hhid2010")),assetslog2010 ,by=c("hhid2010"))
  res=list()
  if(food_analysis==T){
    dat2010 <- merge(plyr::rename(hswithchars2010,c("hhid"="hhid2010")),ka2010, by = c("hhid2010")) %>% mutate (cpA_a = cost_a/A0) %>% mutate (cpA_b = cost_b/A0)
    dat2012 <- merge(plyr::rename(hswithchars2012,c("hhid"="hhid2012")),ka2012, by = c("hhid2012")) %>% mutate (cpA_a = cost_a/A0) %>% mutate (cpA_b = cost_b/A0)
    dat2014 <- merge(plyr::rename(hswithchars2014,c("hhid"="hhid2014")),ka2014, by = c("hhid2014")) %>% mutate (cpA_a = cost_a/A0) %>% mutate (cpA_b = cost_b/A0)
    dat2010 <- subset(dat2010,!is.na(A0) & !is.infinite(cpA_a))
    dat2012 <- subset(dat2012,!is.na(A0) & !is.infinite(cpA_a))
    dat2014 <- subset(dat2014,!is.na(A0) & !is.infinite(cpA_a))
    res[["df2010"]] <- dat2010
    res[["df2012"]] <- dat2012
    res[["df2014"]] <- dat2014
    
  } else{
    indivdat2010_woassets <- merge(plyr::rename(hswithchars2010,c("hhid"="hhid2010")),ne2010, by = c("hhid2010")) 
    indivdat2012_woassets <- merge(plyr::rename(hswithchars2012,c("hhid"="hhid2012")),ne2012, by = c("hhid2012")) 
    indivdat2014_woassets <- merge(plyr::rename(hswithchars2014,c("hhid"="hhid2014")),ne2014, by = c("hhid2014")) 
    
    indivdat2010 <- merge(assetslog2010, indivdat2010_woassets, by = "hhid2010")
    indivdat2012 <- merge(assetslog2012, indivdat2012_woassets, by = "hhid2012")
    indivdat2014 <- merge(assetslog2014, indivdat2014_woassets, by = "hhid2014")
    
    indivdat2010$P1 <- paste(indivdat2010$region,indivdat2010$district,sep="-")
    indivdat2012$P1 <- paste(indivdat2012$region,indivdat2012$district,sep="-")
    indivdat2014$P1 <- paste(indivdat2014$region,indivdat2014$district,sep="-")
    
    #dat2010 <- subset(dat2010,!is.na(A0) )
    #dat2012 <- subset(dat2012,!is.na(A0) )
    #dat2014 <- subset(dat2014,!is.na(A0) )
    
    
    #length(grep("9-31",k[143,]$n))
    
    # in the desired data-frame we would have hhdis with their region-id in P2 (which also included P1). So that pi(r) is the same for all consumers in the P2. 
    # the output would be the pi(r) for all hhid 
    dflist <- list()
    dflist[["indivdat2010"]] <- indivdat2010
    dflist[["indivdat2012"]] <- indivdat2012
    dflist[["indivdat2014"]] <- indivdat2014
    
    for (year in c(2010,2012,2014)){
      dfdat <- dflist[[paste0("indivdat",year)]]
      dfdat <- dfdat %>% mutate( high_educ = as.integer(max_education_rank>educ_pivot) , high_occup = as.integer(max_occupation_rank>occup_pivot))
      
      bubble_distances <- get_bubble_distances(dat=dfdat, distance_threshold = .06)
      dat_over_bubbles <- get_bubble_aggregated_df(input_dat = dfdat,bubble_distances = bubble_distances)
      
      bubble_fields <- ddply(dat_over_bubbles,.(B),summarise,mean_cost_ne_x=mean(cost_ne/hsize),mean_A0=mean(A0)) 
      bubble_occup <- ddply(dat_over_bubbles,.(B,high_occup),summarise,mean_occup_cost_ne_x=mean(cost_ne/hsize),mean_occup_A0=mean(A0))
      bubble_educ <- ddply(dat_over_bubbles,.(B,high_educ),summarise,mean_educ_cost_ne_x=mean(cost_ne/hsize),mean_educ_A0=mean(A0))
      
      bubble_fields_w_P1 <- merge(bubble_distances,bubble_fields,by=c('B'))
      
      rd_bubble <- merge(bubble_fields_w_P1, dfdat, by="P1")
      rd_bubble_weduc <- merge(rd_bubble,bubble_educ, by = c("B","high_educ"))
      rd_bubble_weducoccup <- merge(rd_bubble_weduc,bubble_occup, by = c("B","high_occup"))
      rd <- rd_bubble_weducoccup %>% mutate(x = cost_ne/hsize) %>% mutate(logx=log(x+1e-7))
      rd <- rd %>% mutate (r = log(mean_A0)) %>% mutate ( nu = x/mean_cost_ne_x) %>% mutate (Ar=lnA0-r)
      rd <- rd %>% mutate (r_occup = log(mean_occup_A0)) %>% mutate ( nu_occup = x/mean_occup_cost_ne_x) %>% mutate (Ar_occup=lnA0-r_occup)
      rd <- rd %>% mutate (r_educ = log(mean_educ_A0)) %>% mutate ( nu_educ = x/mean_educ_cost_ne_x) %>% mutate (Ar_educ=lnA0-r_educ)
      rd <-subset(rd,!is.na(r))
      res[[paste0("df",year)]] <- rd
    }
    
    #test
    #print(summary(lm(data=dat2010, nu~ r + max_occupation_rank + max_education_rank)))
    
  }
  
  return(res)
}
