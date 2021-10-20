

setwd('c:/local_files/research/consumption/datamapper/')
source('translation/frameutils.R');
source('nigeria/ngr_normaliser.r'); 
source('nigeria/ngr_loader.r'); 
nl <- ngr_loader(fu,ngr_normaliser,lgc)
source('lsms/lsms_normalizer.r');
source('lsms/lsms_loader.r');llc=lsms_loader(fu=fu,ln=lsms_normalizer,lgc=lgc)

library(maps)
library(ggplot2)
library(ggrepel)


test <- function(){
  #dat <- nl@load_diary_file("../",2010,fu, ngr_normaliser, load_cost=TRUE)
  ohs <- nl@load_ohs_file(year = 2010, dirprefix = "../",fu = fu,ngrn = ngr_normaliser )
  return(ohs)
}

get_ngr_categories <- function(){
  return (c("densefoods","nonfresh","fruitsveg","protein","alcohol","complements","energy","household","transport"))
}


plot_region_map <- function(plot_type,a2012,o2010,o2012,ignored_assets_top){
  
  world_map <- map_data("world")
  ngr_map = subset(world_map ,region=="Nigeria")
  hhid_mtms_2012_all <- ddply(subset(a2012,!is.na(number)  & !is.na(mtm) & number>0), .(hhid), summarise, hhid_mtm=sum(mtm*number))
  highest_quantile_considered <- quantile(hhid_mtms_2012_all$hhid_mtm,1-ignored_assets_top)
  hhid_mtms_2012 <- subset(hhid_mtms_2012_all,hhid_mtm<highest_quantile_considered)
  
  
  
  
  if (plot_type=="occupation"){
    occupation_mapping <- infer_occupation_ranks(o2010 = o2010,ignore_top = .05)
    ohs2012_wrank<- merge(plyr::rename(subset(o2012,!is.na(region)),c("occupation_primary"="occupation")),occupation_mapping[,c("occupation","occupation_rank")],by=c("occupation"))
    chosenchars2012 <- ddply(ohs2012_wrank[,c("hhid","education_rank","occupation_rank","age")],.(hhid),summarise,max_education_rank = choose_max_education_rank(education_rank) , max_occupation_rank = max(occupation_rank),max_age=max(age))
    
    plot_data_wo_assets <- subset(merge(chosenchars2012,o2012,by=c("hhid")), !is.na(max_occupation_rank))
    plot_data <- merge(plot_data_wo_assets,hhid_mtms_2012 ,by="hhid")
     
    plot_data$region_name <-""
    map_data <- subset(ddply(plot_data,.(region,district,S,E,region_name),summarise, mean_a = median(hhid_mtm), occupation_rank=mean(max_occupation_rank)), !is.na(region))
    ggplot()+geom_polygon(data=ngr_map, aes(x=long, y=lat, group=group), 
                          colour="light yellow", fill="light yellow") + geom_point(data=map_data,aes(x=E, y=S, size = mean_a, color = occupation_rank))+ scale_size(range = c(.1, 10), name="assets_value") + geom_label_repel(data=map_data, aes(x=E,y=S, label=ifelse(district==1,as.character(region_name),'')),box.padding = .3, point.padding = .5, segment.color ='grey50') + ggtitle("Asset Values and Occupation Rank distribution in Nigeria (2012)")
  } else if (plot_type == "education") {
    hhid_mtms_o2012 <- merge(o2012,hhid_mtms_2012 ,by="hhid")
    hhid_mtms_o2012_weduc <- subset(hhid_mtms_o2012, !is.na(education_rank))
    hhid_mtms_o2012_weduc$region_name <-""
    map_data <- subset(ddply(hhid_mtms_o2012_weduc,.(region,district,S,E,region_name),summarise, mean_a = median(hhid_mtm), education_rank=mean(education_rank)), !is.na(region))
    ggplot()+geom_polygon(data=ngr_map, aes(x=long, y=lat, group=group), 
                          colour="light yellow", fill="light yellow") + geom_point(data=map_data,aes(x=E, y=S, size = mean_a, color = education_rank))+ scale_size(range = c(.1, 10), name="assets_value") + geom_label_repel(data=map_data, aes(x=E,y=S, label=ifelse(district==1,as.character(region_name),'')),box.padding = .3, point.padding = .5, segment.color ='grey50') + ggtitle("Asset Values and Education Rank distribution in Nigeria (2012)")
    
  }
  
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

compute_yearly_pay<- function(pay,pay_units, hours_per_week,weeks_worked) { 
  if (pay_units == 1 ) { 
    return(hours_per_week * pay * weeks_worked)
  }
  
  if (pay_units == 2 ){
    #daily pay
    # number of days worked per week - (hours_per_week/40) 
    return(weeks_worked*(hours_per_week/40)*pay)
  } 
  if (pay_units == 3){
    #weekly pay
    return(weeks_worked*pay)
  } 
  if (pay_units == 4){
    #fortnightly pay
    return (pay/2*weeks_worked)
  }
  if (pay_units == 5){
    #monthly pay
    return (pay/4 * weeks_worked)
  }
  if (pay_units == 6){
    #quarterly pay
    #13 weeks in a quarter
    return (pay/13 * weeks_worked)
  }
  if (pay_units == 7){
    #half-yearly pay
    #26 weeks in a half-year
    return (pay/26 * weeks_worked)
  }
  if (pay_units == 8){
    #yearly
    return (pay)
  }
  stop("Unknown payment unit")
}

infer_education_ranks <- function(o2010){
  educ <- unique(o2010[c("hhid","personid","highest_educ")])
  educ[is.na(educ$highest_educ),]$highest_educ <- 0
  educ$education_rank <- as.integer(educ$highest_educ<=0)*0 + as.integer(educ$highest_educ>0 & educ$highest_educ<=11)*1 + as.integer(educ$highest_educ>11 & educ$highest_educ<=23)*2 +as.integer(educ$highest_educ>23)*3
  return(educ)
}

infer_occupation_ranks <- function(o2010 , ignore_top) {
  warning("Using only 2010 data - ignoring the worked-for-outside-hh field")
  get_weeks=function(m,w){ if (is.na(w)) { return (m*4.3)} else {return(w)} }
  ow2010 <- o2010
  ow2010$weeks_worked_primary <- mapply(get_weeks,ow2010$yearmonths_worked_primary, ow2010$yearweeks_worked_primary)
  ow2010$weeks_worked_secondary <- mapply(get_weeks,ow2010$yearmonths_worked_secondary, ow2010$yearweeks_worked_secondary)
  
  i2010_primary <- plyr::rename(subset(ow2010,!is.na(occupation_primary) & !is.na(last_payment_primary)  )[,c("hhid","personid","occupation_primary","last_payment_primary","last_payment_primary_units","weeks_worked_primary","hours_worked_week_primary")],c("occupation_primary"="occupation","last_payment_primary"="pay","last_payment_primary_units"="pay_units","weeks_worked_primary"="weeks_worked",'hours_worked_week_primary'='hours_per_week'))
  i2010_secondary <- plyr::rename(subset(ow2010,!is.na(occupation_secondary) & !is.na(last_payment_secondary) )[,c("hhid","personid","occupation_secondary","last_payment_secondary","last_payment_secondary_units","weeks_worked_secondary",'hoursperweek_secondary_work')],c("occupation_secondary"="occupation","last_payment_secondary"="pay","last_payment_secondary_units"="pay_units","weeks_worked_secondary"="weeks_worked",'hoursperweek_secondary_work'='hours_per_week'))
  i2010 <- rbind(i2010_primary,i2010_secondary)
  
  i2010 <- (subset(i2010,!is.na(hours_per_week) & !is.na(pay_units)))
  i2010$yearly_pay <- mapply(compute_yearly_pay,i2010$pay,i2010$pay_units,i2010$hours_per_week,i2010$weeks_worked)
  ignored <- subset(i2010, yearly_pay>=quantile(i2010$yearly_pay,1-ignore_top))
  print(paste("Ignoring ", length(unique(ignored$hhid)) ,"/",length(unique(i2010$hhid)), "households"))
  i2010 <- subset(i2010, yearly_pay<quantile(i2010$yearly_pay,1-ignore_top))
  occup <- ddply(i2010,.(occupation),summarise,l_mean_ypay = log(mean(yearly_pay)),l_median_ypay = log(median(yearly_pay)))
  occup <- merge(plyr::rename(read.csv('../lsms/nigeria/occupation_codes.csv')[,c("occupation_code","occupation_name")],c("occupation_code"="occupation")),occup,by="occupation")
  occup <- occup[order(occup$l_median_ypay),]
  
  occup$occupation_rank <- as.integer(occup$l_median_ypay<=11.9) +  2*as.integer(occup$l_median_ypay>11.9 & occup$l_median_ypay<12.61154)  + 3*as.integer(occup$l_median_ypay>=12.61154)
  # adjustments
  occup<- rbind(occup,data.frame(occupation="7133",occupation_name="insulators",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="2211",occupation_name="biologists",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=2))
  occup<- rbind(occup,data.frame(occupation="7241",occupation_name="electrical mechanics",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="7311",occupation_name="precision instrument makers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="5122",occupation_name="waiters",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="2453",occupation_name="musicians",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="8269",occupation_name="textile machine operators",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="7312",occupation_name="musicians (acoustic)",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="8223",occupation_name="metal finishers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="7324",occupation_name="ceramic painters",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="4144",occupation_name="scribes",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="3320",occupation_name="education specialists(1)",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="7341",occupation_name="type setters",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="3418",occupation_name="auctioneers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="3122",occupation_name="computer equipment operators",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="7216",occupation_name="under-water workers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="3223",occupation_name="dieticians and nutritionists",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="7345",occupation_name="textile printers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="3224",occupation_name="optometrists",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="8312",occupation_name="railway workers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="7436",occupation_name="embroiderers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="8122",occupation_name="metal melters",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="3141",occupation_name="ship engineers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="7344",occupation_name="bookbinders",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="3226",occupation_name="physiotherapists",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="7111",occupation_name="miners",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="8132",occupation_name="ceramic plant operators",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="8334",occupation_name="lift-truck operators",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="9120",occupation_name="shoe-cleaners",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="8159",occupation_name="chemical-plant operators",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="3228",occupation_name="pharma assistants",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="8282",occupation_name="elec and machinery assemblers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="3151",occupation_name="building and fire inspectors",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="812",occupation_name="cement and materials processing machine operators",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="9161",occupation_name="garbage collectors",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  occup<- rbind(occup,data.frame(occupation="8143",occupation_name="paper plant operators",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=1))
  
  
  occup<- rbind(occup,data.frame(occupation="3470",occupation_name="religion associate",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=2))
  occup<- rbind(occup,data.frame(occupation="2351",occupation_name="education specialists(2)",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=2))
  occup<- rbind(occup,data.frame(occupation="2441",occupation_name="economists",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=2))
  occup<- rbind(occup,data.frame(occupation="3132",occupation_name="broadcasting equipment controllers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=2))
  occup<- rbind(occup,data.frame(occupation="3412",occupation_name="insurance representatives",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=2))
  occup<- rbind(occup,data.frame(occupation="1317",occupation_name="business managers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=2))
  occup<- rbind(occup,data.frame(occupation="3145",occupation_name="air-traffic safety technicians",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=2))
  occup<- rbind(occup,data.frame(occupation="2422",occupation_name="judges",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=2))
  occup<- rbind(occup,data.frame(occupation="2147",occupation_name="mining engineers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=2))
  
  
  occup<- rbind(occup,data.frame(occupation="1110",occupation_name="legislators",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=3))
  occup<- rbind(occup,data.frame(occupation="1142",occupation_name="senior business officers",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=3))
  occup<- rbind(occup,data.frame(occupation="3432",occupation_name="legal professionals",l_mean_ypay = NA, l_median_ypay=NA, occupation_rank=3))
  
  
  
  return(occup)
}

load_data <- function()
{
  
  ngrdf2010 <- read_dta('../lsms/data/ngr_df2010.dta')
  ngrdf2012 <- read_dta('../lsms/data/ngr_df2012.dta')
  ngrdf2015 <- read_dta('../lsms/data/ngr_df2015.dta')
  res = list()
  res[['df2010']] <- ngrdf2010
  res[['df2012']] <- ngrdf2012
  res[['df2015']] <- ngrdf2015
  return(res)
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


choose_max_education_rank <- function (x) { arr = x[!is.na(x)] ; if (length(arr)>1) {return (max(arr))} else {return(0)}}


get_bubble_distances <- function(dat,distance_threshold,popdistance_threshold){
  # the average of consumption of consumers within a given population-distance becomes pi(r), the total asset value becomes r, the total expenditure is cost_ne
  # remember we have distances only of consumers 
  
  loc_cols <- c("region","district","S","E")
  
  all_points <- unique(dat[,loc_cols])
  
  all_points$point <- paste(all_points$region,all_points$district,sep="-")
  all_distances <- expand.grid(all_points$point,all_points$point)
  colnames(all_distances) <- c("P1","P2")
  all_distances <- plyr::rename(merge(plyr::rename(all_points,c("point"="P1")),all_distances,by=c("P1")) ,c("S"="S1","E"="E1","region"="region1","district"="district1") )
  all_distances <- plyr::rename(merge(plyr::rename(all_points,c("point"="P2")),all_distances,by=c("P2")) ,c("S"="S2","E"="E2","region"="region2","district"="district2") )
  
  # The distances between two points that are populous would be lower than two points that are less populous
  # The distances are still symmetric - because even if one is significantly more populous than the other - they're closer than they would be when they're not populous.
  all_distances$distance <- mapply(function(s1,e1,s2,e2) { sqrt((s1-s2)**2 + (e1-e2)**2) } , all_distances$S1,all_distances$E1,all_distances$S2,all_distances$E2)
  
  if (missing(distance_threshold)){
    stop("Must provide either distance_threshold or popdistance_threshold")  
  } else{
    filtered_distances <- subset(all_distances,distance<distance_threshold)
  }
  
  bubble_distances <- ddply(unique(filtered_distances[,c("P1","P2")]),.(P1),summarise,B=toJSON(P2))
  return(bubble_distances)
}

get_bubble_aggregated_df <- function(input_dat,bubble_distances){
  
  pb <- txtProgressBar(min = 0, max = dim(bubble_distances)[1], style = 3)
  resdf <- NULL
  for (i in seq(dim(bubble_distances)[1])){
    tempdat <- subset(input_dat %>% mutate(found=sapply(input_dat$P1,function(x){ is.element(x,fromJSON(bubble_distances[i,]$B))}) ), found==T)
    tempdat <- tempdat %>% mutate(B=bubble_distances[i,]$B , found=NULL)
    resdf <- rbind(resdf,tempdat)
    
    #mean can be calculated over 
    #tempdat %>% mutate(high_occup = as.integer(max_occupation_rank>1))
    #tempdat %>% mutate(high_educ = as.integer(max_education_rank>1))
    setTxtProgressBar(pb, i)
  }
  
  return(resdf)
}



#ng <- ngr_get_nonparametric_df(nl = nl,food_analysis = F,o2010 = o2010,o2012 = o2012,o2015 = o2015,a2010 = a2010,a2012 = a2012,a2015 = a2015,c2010 = c2010,c2012 = c2012,c2015 = c2015)
ngr_get_nonparametric_df <- function(nl,food_analysis,o2010, o2012,o2015,a2010, a2012, a2015,c2010,c2012,c2015){
  
  educ_pivot <- 3
  occup_pivot <- 2
  
  #occupation_mapping
  occupation_mapping <- infer_occupation_ranks(o2010 = o2010,ignore_top = .05)

  asset_mtms_2010 = asset_mtms(a2010,"furntiure_medium","2010")
  asset_mtms_2012 = asset_mtms(a2012,"furntiure_medium","2012")
  asset_mtms_2015 = asset_mtms(a2015,"furntiure_medium","2015")
  print("Summing up asset values")
  assetslog2010 <- ddply(asset_mtms_2010,.(hhid),summarise,lnA0=log(sum(number.2010*mtm.2010)+1e-7),A0=(sum(number.2010*mtm.2010)))
  assetslog2012 <- ddply(asset_mtms_2012,.(hhid),summarise,lnA0=log(sum(number.2012*mtm.2012)+1e-7),A0=sum(number.2012*mtm.2012))
  assetslog2015 <- ddply(asset_mtms_2015,.(hhid),summarise,lnA0=log(sum(number.2015*mtm.2015)+1e-7),A0=sum(number.2015*mtm.2015))
  
  if (food_analysis==T){
    all_costs_considered <- ngr_normaliser()@expenditure_categories()
    food_costs_group <- subset(all_costs_considered,is.element(group,c("food")))$shortname
    excess_costs_group <- subset(all_costs_considered,is.element(group,c("excess")))$shortname
    
    x2010 <- plyr::rename(ngr_normaliser()@get_total_expenditures(hh = c2010, ohs = o2010), c("total_expenditure"="x"))
    x2012 <- plyr::rename(ngr_normaliser()@get_total_expenditures(hh = c2012, ohs = o2012),c("total_expenditure"="x"))
    x2015 <- plyr::rename(ngr_normaliser()@get_total_expenditures(hh = c2015, ohs = o2015),c("total_expenditure"="x"))
    
    hsizex2010 <- merge(ngr_normaliser()@get_hsize(o2010),x2010,by=c("hhid"))
    hsizex2012 <- merge(ngr_normaliser()@get_hsize(o2012),x2012,by=c("hhid"))
    hsizex2014 <- merge(ngr_normaliser()@get_hsize(o2014),x2014,by=c("hhid"))
    
    k2010_tot <- get_split_costs(categs_a = food_costs_group,categs_b = excess_costs_group,dat = c2010, group_field = "shortname")
    k2012_tot <- get_split_costs(categs_a = food_costs_group,categs_b = excess_costs_group,dat = c2012, group_field = "shortname")
    k2015_tot <- get_split_costs(categs_a = food_costs_group,categs_b = excess_costs_group,dat = c2015, group_field = "shortname")
    
    k2010 <- (merge(k2010_tot,hsizex2010,by=c("hhid")) %>% mutate(cost_a=cost_a/hsize) %>% mutate(cost_b=cost_b/hsize))
    k2012 <- (merge(k2012_tot,hsizex2012,by=c("hhid")) %>% mutate(cost_a=cost_a/hsize) %>% mutate(cost_b=cost_b/hsize))
    k2015 <- (merge(k2015_tot,hsizex2014,by=c("hhid")) %>% mutate(cost_a=cost_a/hsize) %>% mutate(cost_b=cost_b/hsize))
    
    k2010 <- k2010 %>% mutate(w_a = cost_a/(cost_a+cost_b)) %>% mutate(w_b = cost_b/(cost_a+cost_b))
    k2012 <- k2012 %>% mutate(w_a = cost_a/(cost_a+cost_b)) %>% mutate(w_b = cost_b/(cost_a+cost_b)) 
    k2015 <- k2015 %>% mutate(w_a = cost_a/(cost_a+cost_b)) %>% mutate(w_b = cost_b/(cost_a+cost_b)) 
    
    ka2010 <- (merge(assetslog2010,plyr::rename(k2010,c("hhid"="hhid2010")),by=c("hhid2010"))) %>% mutate (year=2010) %>% mutate( logx=log(x)) %>% mutate( logxc=log(x/consu))
    ka2010 <- ka2010[,setdiff(colnames(ka2010),c("consu","hsize"))]
    ka2012 <- (merge(assetslog2012,plyr::rename(k2012,c("hhid"="hhid2012")),by=c("hhid2012"))) %>% mutate (year=2012) %>% mutate( logx=log(x)) %>% mutate( logxc=log(x/consu))
    ka2012 <- ka2012[,setdiff(colnames(ka2012),c("consu","hsize"))]
    ka2014 <- (merge(assetslog2015,plyr::rename(k2015,c("hhid"="hhid2015")),by=c("hhid2015"))) %>% mutate (year=2014) %>% mutate( logx=log(x)) %>% mutate( logxc=log(x/consu))
    ka2014 <- ka2014[,setdiff(colnames(ka2014),c("consu","hsize"))]
    
  } else {
    all_costs <- ngr_normaliser()@expenditure_categories()
    #asset purchases and asset-bearing costs are not considered
    food_costs <- subset(all_costs, is.element(group,c("food")))
    excess_costs <- subset(all_costs, is.element(group,c("excess")))
    
    food2010 <- ddply(subset(c2010,is.element(shortname,food_costs$shortname)),.(hhid),summarise,cost_ne_food=sum(cost))
    food2012 <- ddply(subset(c2012,is.element(shortname,food_costs$shortname)),.(hhid),summarise,cost_ne_food=sum(cost))
    food2015 <- ddply(subset(c2015,is.element(shortname,food_costs$shortname)),.(hhid),summarise,cost_ne_food=sum(cost))
    
    excess2010 <- ddply(subset(c2010,is.element(shortname,excess_costs$shortname)),.(hhid),summarise,cost_ne_nonfood=sum(cost))
    excess2012 <- ddply(subset(c2012,is.element(shortname,excess_costs$shortname)),.(hhid),summarise,cost_ne_nonfood=sum(cost))
    excess2015 <- ddply(subset(c2015,is.element(shortname,excess_costs$shortname)),.(hhid),summarise,cost_ne_nonfood=sum(cost))
    
    ne2010 <- merge(food2010,excess2010,by=c("hhid"),all=T)
    ne2012 <- merge(food2012,excess2012,by=c("hhid"),all=T)
    ne2015 <- merge(food2015,excess2015,by=c("hhid"),all=T)
    
  }
  
  #total consumption
  relevant_fields <-c("hhid","region","district","ea","is_urban","S","E")
  # 2010
  ohs2010 <- subset(o2010,!is.na(region))
  hsizes2010 <- ddply(ohs2010[,c("hhid","personid")],.(hhid),summarise,hsize=length(personid))
  hs2010 <- unique(merge(unique(ohs2010[,relevant_fields]), hsizes2010, by = c("hhid")))
  
  ohs2010_wrank<- merge(plyr::rename(ohs2010,c("occupation_primary"="occupation")),occupation_mapping[,c("occupation","occupation_rank")],by=c("occupation"))
  
  chosenchars2010 <- ddply(ohs2010_wrank[,c("hhid","education_rank","occupation_rank","age")],.(hhid),summarise,max_education_rank = choose_max_education_rank(education_rank) , max_occupation_rank = max(occupation_rank),age=max(age) )
  #  perception_columns
  
  hswithchars2010 <- merge(hs2010,chosenchars2010,all.x = T)
  # -6.727135 39.14395
  
  
  # 2012
  ohs2012 <- subset(o2012,!is.na(region))
  hsizes2012 <- ddply(ohs2012[,c("hhid","personid")],.(hhid),summarise,hsize=length(personid))
  hs2012 <- unique(merge(unique(ohs2012[,relevant_fields]), hsizes2012, by = c("hhid")))
  ohs2012_wranks<- merge(plyr::rename(ohs2012,c("occupation_primary"="occupation")),occupation_mapping[,c("occupation","occupation_rank")],by=c("occupation"))
  
  chosenchars2012 <- ddply(ohs2012_wranks[,c("hhid","education_rank","occupation_rank","age")],.(hhid),summarise,max_education_rank = choose_max_education_rank(education_rank) , max_occupation_rank = max(occupation_rank),age=max(age))
  
  hswithchars2012 <- merge(hs2012,chosenchars2012,all.x = T)
  
  #2015
  ohs2015 <- subset(o2015,!is.na(region))
  hsizes2015 <- ddply(ohs2015[,c("hhid","personid")],.(hhid),summarise,hsize=length(personid))
  hs2015 <- unique(merge(unique(ohs2015[,relevant_fields]), hsizes2015, by = c("hhid")))
  
  chosenchars2015_woranks <- ddply(ohs2015[,c("hhid","age")],.(hhid),summarise,age=max(age))
  rank_from_past_years <- ddply(rbind(chosenchars2010,chosenchars2012),.(hhid),summarise, max_education_rank=max(max_education_rank), max_occupation_rank=max(max_occupation_rank))
  chosenchars2015 <- merge(rank_from_past_years,chosenchars2015_woranks,by=c("hhid"))
  hswithchars2015 <- merge(hs2015,chosenchars2015,all.x = T)

  
  #a<-merge(plyr::rename(i2010,c("hhid"="hhid2010")),assetslog2010 ,by=c("hhid2010"))
  res=list()
  if(food_analysis==T){
    dat2010 <- merge(hswithchars2010,ka2010, by = c("hhid")) %>% mutate (cpA_a = cost_a/A0) %>% mutate (cpA_b = cost_b/A0)
    dat2012 <- merge(hswithchars2012,ka2012, by = c("hhid")) %>% mutate (cpA_a = cost_a/A0) %>% mutate (cpA_b = cost_b/A0)
    dat2015 <- merge(hswithchars2015,ka2015, by = c("hhid")) %>% mutate (cpA_a = cost_a/A0) %>% mutate (cpA_b = cost_b/A0)
    dat2010 <- subset(dat2010,!is.na(A0) & !is.infinite(cpA_a))
    dat2012 <- subset(dat2012,!is.na(A0) & !is.infinite(cpA_a))
    dat2015 <- subset(dat2015,!is.na(A0) & !is.infinite(cpA_a))
    res[["df2010"]] <- dat2010
    res[["df2012"]] <- dat2012
    res[["df2015"]] <- dat2015
    
  } else{
    indivdat2010_woassets <- merge(hswithchars2010,ne2010, by = c("hhid")) 
    indivdat2012_woassets <- merge(hswithchars2012,ne2012, by = c("hhid")) 
    indivdat2015_woassets <- merge(hswithchars2015,ne2015, by = c("hhid")) 
    
    indivdat2010 <- merge(assetslog2010, indivdat2010_woassets, by = "hhid")
    indivdat2012 <- merge(assetslog2012, indivdat2012_woassets, by = "hhid")
    indivdat2015 <- merge(assetslog2015, indivdat2015_woassets, by = "hhid")
    
    indivdat2010$P1 <- paste(indivdat2010$region,indivdat2010$district,sep="-")
    indivdat2012$P1 <- paste(indivdat2012$region,indivdat2012$district,sep="-")
    indivdat2015$P1 <- paste(indivdat2015$region,indivdat2015$district,sep="-")
    
    
    # in the desired data-frame we would have hhdis with their region-id in P2 (which also included P1). So that pi(r) is the same for all consumers in the P2. 
    # the output would be the pi(r) for all hhid 
    dflist <- list()
    dflist[["indivdat2010"]] <- indivdat2010
    dflist[["indivdat2012"]] <- indivdat2012
    dflist[["indivdat2015"]] <- indivdat2015
    
    for (year in c(2010,2012,2015)){
      
      dfdat <- dflist[[paste0("indivdat",year)]]
      dfdat <- dfdat %>% mutate( high_educ = as.integer(max_education_rank>educ_pivot) , high_occup = as.integer(max_occupation_rank>occup_pivot))
      
      bubble_distances <- get_bubble_distances(dat=dfdat, distance_threshold = .06)
      dat_over_bubbles <- get_bubble_aggregated_df(input_dat = dfdat,bubble_distances = bubble_distances)
      
      bubble_fields <- ddply(dat_over_bubbles,.(B),summarise,mean_cost_ne_food_x=mean(cost_ne_food/hsize),mean_cost_ne_nonfood_x=mean(cost_ne_nonfood/hsize), mean_A0=mean(A0)) 
      bubble_occup <- ddply(dat_over_bubbles,.(B,high_occup),summarise,mean_occup_cost_ne_food_x=mean(cost_ne_food/hsize), mean_occup_cost_ne_nonfood_x = mean(cost_ne_nonfood/hsize), mean_occup_A0=mean(A0))
      bubble_educ <- ddply(dat_over_bubbles,.(B,high_educ),summarise,mean_educ_cost_ne_food_x=mean(cost_ne_food/hsize),mean_educ_cost_ne_nonfood_x=mean(cost_ne_nonfood/hsize),mean_educ_A0=mean(A0))
      
      bubble_fields_w_P1 <- merge(bubble_distances,bubble_fields,by=c('B'))
      
      rd_bubble <- merge(bubble_fields_w_P1, dfdat, by="P1")
      rd_bubble_weduc <- merge(rd_bubble,bubble_educ, by = c("B","high_educ"))
      rd_bubble_weducoccup <- merge(rd_bubble_weduc,bubble_occup, by = c("B","high_occup"))
      rd <- rd_bubble_weducoccup %>% mutate(x_food = cost_ne_food/hsize) %>% mutate(x_nonfood = cost_ne_nonfood/hsize) %>% mutate(logx_food=log(x_food+1e-7),logx_nonfood=log(x_nonfood+1e-7))
      rd <- rd %>% mutate (r = log(mean_A0)) %>% mutate (Ar=lnA0-r)
      rd <- rd %>% mutate (r_occup = log(mean_occup_A0)) %>%  mutate (Ar_occup=lnA0-r_occup)
      rd <- rd %>% mutate (r_educ = log(mean_educ_A0)) %>% mutate (Ar_educ=lnA0-r_educ)
      rd <-subset(rd,!is.na(r))
      res[[paste0("df",year)]] <- rd
    }
    
    #test
    #print(summary(lm(data=dat2010, nu~ r + max_occupation_rank + max_education_rank)))
    
  }
  
  return(res)
}

run_non_parametric_regression_for_food_nonfood_ne <- function(ll,dfslist,year,sp,theta,phi)
{
  #Also plot - plot(data=nf[["df2012"]] %>% mutate(log_cost=log(mean_cost_ne)), log_cost ~ r, xlab=latex2exp::TeX("$r_t$"),ylab = latex2exp::TeX("$log(x_t)$"))")
  
  if(missing(dfslist)){
    dfslist <- ngr_get_nonparametric_df(ll,food_analysis = F)
  }
  select_df=paste0("df",year)
  
  S <- with(dfslist[[select_df]], seq(min(S), max(S), len=25))
  E <- with(dfslist[[select_df]], seq(min(E), max(E), len=25))
  newdata <- expand.grid(S=S, E=E)
  mod.lo_food_x <- loess(mean_cost_ne_food_x ~ S + E , span=sp, degree=1, data=dfslist[[select_df]] )
  mod.lo_nonfood_x <- loess(mean_cost_ne_nonfood_x ~ S + E , span=sp, degree=1, data=dfslist[[select_df]] )
  
  fit.lo_food_x <- matrix(predict(mod.lo_food_x, newdata), 25, 25)
  fit.lo_nonfood_x <- matrix(predict(mod.lo_nonfood_x, newdata), 25, 25)
  
  par(mfrow=c(1,1))
  if (missing(theta)){
    theta <- 10
  }
  if (missing(phi)){
    phi <- 20
  }
  par(mfrow=c(1,2))
  persp(S, E, fit.lo_food_x, theta=theta, phi=phi, ticktype="detailed", expand=2/3,shade=0.5,main = "food exp", zlab = "")
  persp(S, E, fit.lo_nonfood_x, theta=theta, phi=phi, ticktype="detailed", expand=2/3,shade=0.5,main = "non-food exp", zlab = "")
  
}


asset_mtms <- function(assets_dat,pivot_asset,year){
  
  assets <- subset(assets_dat,!is.na(mtm) & mtm >0 & number >0 & !is.na(number) )
  assets$shortname <- as.character(assets$shortname)
  
  assets_src    <- (dplyr::filter( merge(assets,ddply(assets,.(shortname),summarise,v=fu()@fv(mtm)),all.x=TRUE) , mtm < v))
  
  print("Filtered out excessively high asset values")
  c0 <- ddply(subset(assets_src, number>0 & !is.na(mtm) & mtm>0), .(shortname), summarise , median_mtm = median(mtm), mean_mtm = mean(mtm), n = length(hhid))
  c0 <- c0[order(c0$mean_mtm),]
  
  # anything ge expensive than pivot_asset is an asset
  
  all_assets              <- setdiff(subset(c0,median_mtm>= c0[c0$shortname==pivot_asset,]$median_mtm)$shortname,c()) #c("land","house")
  

  select_cols <- c("hhid","number","shortname","mtm")
  a <- plyr::rename(subset(assets_src[,select_cols],number>0 & is.element(shortname,all_assets)),c("number"=paste0("number.",year),"mtm"=paste0("mtm.",year)))
  return(a)
  
}


test <- function(){
  ngr_get_nonparametric_df(nl=nl,food_analysis = F,o2010 = o2010, o2012 = o2012, o2015 = o2015, a2010 = a2010, a2012 = a2012, a2015 = a2015,c2010 = c2010, c2012 = c2012, c2015 = c2015)
}