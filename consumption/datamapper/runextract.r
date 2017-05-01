
setwd('c:/local_files/research/consumption/datamapper/')

debugSource('callertree.r')
debugSource('./sa/sa.r')

debugSource('us_cex/us_cex_loader.r')
debugSource('translation/frameutils.R')

#debugSource('us_cex/us_cex_loader.r');ds<-uscex(fcdu=fu)@combined_data_set(2004,"C:/local_files/research/consumption/cex/cex_data/",201,FALSE)

debugSource('lsms/lsms_normalizer.r');debugSource('lsms/lsms_loader.r');
#ln@food_categories_lsms_2010()

sample_vec<-function(vec,n){
  if (length(n)>1){
    stop("n must be a scalar")
  }
  if (dim(vec)<=1 || dim(vec)[1]<n ){
    stop("condition: dim(vec)[1]  > n > 1 must be met")
  }
  return(vec[floor((dim(vec)[1])*runif(n)+1),])
}

#run as : estimate_dq_drho(ds=ds,fu=fu())

significant_lmvars<-function(ds,depvar,vars_init){
  while(TRUE){
    res = lm(data=ds,paste(depvar,"~",write_variables_as_sum(vars_init),sep=""))
    resdf<-as.data.frame(summary(res)$coefficients)
    resdf<-resdf[rownames(resdf)!="(Intercept)",] # suppress ignoring of (Intercept)
    min_tval <- min(abs(resdf[,"t value"]))
    if (min_tval > 1.9){
      print(paste("significant variables - ",toString (vars_init)));
      retlst = list()
      retlst[["vars"]]=vars_init
      retlst[["result"]]=res
      return(retlst)
    } else {
      min_tval_var<-rownames(resdf[abs(resdf[,"t value"])==min_tval,])
      vars_init<-setdiff(vars_init,min_tval_var)
    }
  }
}

estimate_dq_drho<-function(ds,fu,N,nTimes){
  
  r<-data.frame(dvis=NULL,dx=NULL,dconsu=NULL,deduc=NULL,dage=NULL,docc=NULL,dhous=NULL,drh=NULL)
  
  for (i in seq(nTimes)){
    s1 <- sample_vec(ds,N)
    s2 <- sample_vec(ds,N)
    
    dvis= mean(s1$visible_consumption)-mean(s2$visible_consumption) 
    dx = mean(s1$total_expenditure)-mean(s2$total_expenditure)
    deduc = mean(s1$highest_educ[!is.na(s1$highest_educ)]) -mean(s2$highest_educ[!is.na(s2$highest_educ)])
    dage = mean(s1$age) - mean(s2$age)
    docc = mean(s1$occupation_rank)-mean(s2$occupation_rank)
    dhous = mean(s1$housingstatus) - mean(s2$housingstatus)
    dconsu = mean(s1$consu) - mean(s2$consu)
    drho = fu@find_nonzero_percentile(s1,"visible_consumption",.001)-fu@find_nonzero_percentile(s2,"visible_consumption",.001)
    r<-rbind(r,data.frame(dvis=dvis,dx=dx,dconsu=dconsu,deduc=deduc,dage=dage,docc=docc,dhous=dhous,drh=drho))
  }
  print(summary(lm(data=r,dvis~dx+drh)))
  return(r)
}

print("DONE")

items_map<-function(categories){
  r=data.frame(item=NULL,code=NULL)
  r=rbind(r,data.frame(item="rice",code="10101"))
  r=rbind(r,data.frame(item="rice",code="10102"))
  r=rbind(r,data.frame(code="218",item="barsoap"))# drh not significant
  r=rbind(r,data.frame(code="224",item="personalitemsrepairs")) # dx not significant
  r=rbind(r,data.frame(code="211", item="toothbrush"))
  r=rbind(r,data.frame(code="213", item="skincream"))
  r=rbind(r,data.frame(code="10801",item="meat"))
  r=rbind(r,data.frame(code="10802",item="meat"))
  r=rbind(r,data.frame(code="10803",item="meat"))
  r=rbind(r,data.frame(code="10804",item="meat"))
  r=rbind(r,data.frame(code="10805",item="meat"))
  r=rbind(r,data.frame(code="10806",item="meat"))
  r=rbind(r,data.frame(code="10807",item="meat"))
  r=rbind(r,data.frame(code="10808",item="meat"))
  r=rbind(r,data.frame(code="10809",item="meat"))
  r=rbind(r,data.frame(code="10810",item="meat"))
  r=rbind(r,data.frame(code="10701",item="fruits"))
  r=rbind(r,data.frame(code="10702",item="fruits"))
  r=rbind(r,data.frame(code="10703",item="fruits"))
  r=rbind(r,data.frame(code="10704",item="fruits"))
  r=rbind(r,data.frame(code="101",item="tobacco"))
  r=rbind(r,data.frame(code="11106",item="alcohol"))# dx not significant for alcohol
  r=rbind(r,data.frame(code="11107",item="alcohol"))
  r=rbind(r,data.frame(code="11108",item="alcohol"))
  r=rbind(r,data.frame(code="218",item="donations"))
  r=rbind(r,data.frame(code="202",item="electricity"))
  r=rbind(r,data.frame(code="313",item="marriage"))
  r=rbind(r,data.frame(code="314",item="marriage"))
  r=rbind(r,data.frame(code="219",item="vehicleservice"))
  r=rbind(r,data.frame(code="214",item="cosmeticspersonalproducts"))
  r=rbind(r,data.frame(code="301",item="carpetsrugs"))
  r=rbind(r,data.frame(code="315",item="funeral"))
  if (missing(categories)){
  return(r)
  }else{
    return(r[is.element(r$item,categories),])
  }
}



runtest<-function(m){
  #m=items_map()
  results <- data.frame(category=NULL,dx=NULL, drh=NULL)
  
  for (strcategory in as.character(unique(m$item)))
  {
    category<-as.character(m[m$item==strcategory,]$code)
    
    ll=lsms_loader(fu=fu,ln=lsms_normalizer)
    #ds<-ll@combined_data_set(year=2010,selected_category=lsms_normalizer()@food_categories_lsms_2010(), dirprefix='c:/local_files/research/consumption/')
    ds<-ll@combined_data_set(year=2010,selected_category=category, dirprefix='c:/local_files/research/consumption/')

    for (i in seq(10)){
      print (paste("Running iteration ",i))
      r=estimate_dq_drho(ds=ds[ds$region==7,],fu=fu(),N=400,nTimes=400);
      # TODO: use significant selection here
      res <- (lm(data=r,dvis~dx+drh))
      
      results <- rbind(results,data.frame(category=strcategory,dx=coef(res)["dx"], drh=coef(res)["drh"]))
    }
    print(paste("results for", strcategory))
    print (results[results$category==strcategory,])
  }
  #write.csv(file='c:/temp/results.csv',results)
  return(results)
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