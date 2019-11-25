
setwd('c:/local_files/research/consumption/datamapper/')
source('translation/frameutils.R');
source('nigeria/ngr_normaliser.r'); 
source('nigeria/ngr_loader.r'); 
nl <- ngr_loader(fu,ngr_normaliser,lgc)

test <- function(){
  #dat <- nl@load_diary_file("../",2010,fu, ngr_normaliser, load_cost=TRUE)
  ohs <- nl@load_ohs_file(year = 2010, dirprefix = "../",fu = fu,ngrn = ngr_normaliser )
  return(ohs)
}