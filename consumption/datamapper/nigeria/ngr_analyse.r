
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
load_group_ngr <- function(dat,year,categories){
  if (missing(categories)){
    categories <- get_ngr_categories()
  }
  if (missing(dat)){
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