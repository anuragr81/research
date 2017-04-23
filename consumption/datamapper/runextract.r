
setwd('c:/local_files/research/consumption/datamapper/')

debugSource('callertree.r')
debugSource('./sa/sa.r')

debugSource('us_cex/us_cex_loader.r')
debugSource('translation/frameutils.R')

#debugSource('us_cex/us_cex_loader.r');ds<-uscex(fcdu=fu)@combined_data_set(2004,"C:/local_files/research/consumption/cex/cex_data/",201,FALSE)

debugSource('lsms/lsms_normalizer.r');debugSource('lsms/lsms_loader.r');
#ln@food_categories_lsms_2010()



print("DONE")

runtest<-function(){
  ll=lsms_loader(fu=fu,ln=lsms_normalizer)  
  ds<-ll@combined_data_set(year=2010,selected_category=lsms_normalizer()@food_categories_lsms_2010(), dirprefix='c:/local_files/research/consumption/')
  return(ds)
}

get_region <- function(regions,districts) {
  region<-(ds[is.element(ds$district,c(1,2,3)) & is.element(ds$region,c(7)),]$region)
  
}
compute_appeal<-function(ln,hh,item,availability){
  # availability is calculated based on whether the item is anywhere in the hh but since its sense involves
  # whether the item can be available somewhere else it's passed as a boolean input (electricity is not available in many areas for example)
  
  #affordability is calculated on whether the expenditure on the item 
  
  
  #
  #bandwagon<-
  
}