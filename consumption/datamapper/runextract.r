
setwd('c:/local_files/research/consumption/datamapper/')

source('callertree.r')
source('./sa/sa.r')

source('us_cex/us_cex_loader.r')
source('translation/frameutils.R')

#source('us_cex/us_cex_loader.r');ds<-uscex(fu=fu)@combined_data_set(2004,"C:/local_files/research/consumption/cex/cex_data/",201,FALSE)

#source('lsms/lsms_normalizer.r');source('lsms/lsms_loader.r');lsms_loader(fu=fu,ln=lsms_normalizer)@combined_data_set(year=2010,dirprefix='c:/local_files/research/consumption/')
print("DONE")