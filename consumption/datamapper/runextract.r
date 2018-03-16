library(haven)  # write_dta
setwd('c:/local_files/research/consumption/datamapper/')

debugSource('callertree.r')
debugSource('./sa/sa.r')

debugSource('us_cex/us_cex_loader.r')


#debugSource('us_cex/us_cex_loader.r');ds<-uscex(fcdu=fu)@combined_data_set(2un004,"C:/local_files/research/consumption/cex/cex_data/",201,FALSE)

debugSource('lsms/lsms_normalizer.r');debugSource('lsms/lsms_loader.r');debugSource('translation/frameutils.R')
#ln@food_categories_lsms_2010()

get_allgrp_food_data_frame<-function(year,dirprefix,fu,ln,shortNamesFile){
  r   <-data.frame();
  ii  <-merge(ln()@items_codes_2010(),read.csv(shortNamesFile)[c("calories","shortname","group")],by=c("shortname"))
  for ( group in unique(as.character(ii$group))) {
    r<- rbind(r,run_food_group_regress(year=year,dirprefix=dirprefix,fu=fu,ln=ln, groupName = group, shortNamesFile = shortNamesFile ))
  }
  return(r)
}
run_food_group_regress<-function(year,groupName,dirprefix,fu,ln,shortNamesFile,foodExpenditureData,printResults)
{
  ll=lsms_loader(fu=fu,ln=ln)
  if (missing(foodExpenditureData)){
    ds<-ll@food_expenditure_data(dirprefix = dirprefix, year = year,fu = fu, ln = ln, shortNamesFile=shortNamesFile)
  } else {
    ds<-foodExpenditureData 
  }
  
  if (!is.element(groupName,as.character(unique(ds$group))) ){
    stop(paste("Group",groupName,"not found in data"))
  }
  
  ds<-subset(ds,group==groupName)
  
  print("Aggregating group quantities")
  
  ds                    <-ddply(ds,.(hhid,group),summarise,cost=sum(cost),merge_quantity=sum(merge_quantity),merge_unit=unique(merge_unit),x=unique(x),
                                hsize=unique(hsize),highest_educ=unique(highest_educ),age=unique(age),region=unique(region),
                                litlang=unique(litlang),occupation_rank=unique(occupation_rank),roomsnum=unique(roomsnum))
  
  if (dim(unique(ds))[1]!=dim(ds)[1]){
    stop("run_food_group_regress - Non-unique data for hhid")
  }
  
  
  
  
  if (length(unique(ds$hhid))<50 )
  {
    stop("Number of households (=",length(unique(ds$hhid)),") is too small.")
  }
  
  ds$lnx               <- with( ds,log(x))
  ds$lnunitvalue       <- with( ds, log(cost/merge_quantity))
  ds$lnmerge_quantity  <- with( ds, log(merge_quantity)) 
  
  clusterMeans         <-ddply(ds,.(region),summarise,cost_c=mean(cost),lnmerge_quantity_c=mean(lnmerge_quantity),hsize_c=mean(hsize),
                               age_c=mean(age),roomsnum_c=mean(roomsnum),
                               occupation_rank_c = mean(occupation_rank),lnx_c=mean(lnx),
                               lnunitvalue_c =mean(lnunitvalue))
  if (any(is.na( clusterMeans [ , setdiff(colnames(clusterMeans),"region") ] )) )
  {
    stop("Cluster means cannot have NAs")
  }
  
  ds <- merge(ds,clusterMeans,by=c("region"))
  print("Merged with cluster means")
  
  
  ds$lnunitvalue_a       <- with (ds, lnunitvalue-lnunitvalue_c)
  
  ds$lnx_a               <- with (ds, lnx-lnx_c)
  ds$lnmerge_quantity_a  <- with (ds, lnmerge_quantity-lnmerge_quantity_c )
  ds$occupation_rank_a   <- with (ds, occupation_rank - occupation_rank_c )
  ds$hsize_a             <- with (ds, hsize - hsize_c )
  ds$age_a               <- with (ds, age - age_c )
  ds$roomsnum_a          <- with (ds, roomsnum - roomsnum_c)
  print ("Completed adjsuting quantities and personal characteristics")
  if (!missing(printResults)){
    print (summary( lm (data=ds,lnmerge_quantity_a~lnx_a+occupation_rank_a+hsize_a+age_a+roomsnum_a  )))
    print (summary( lm (data=ds,lnunitvalue_a~lnx_a+occupation_rank_a+hsize_a+age_a+roomsnum_a  ))   )
  }
  return(ds)
  
}

run_price_regress<-function(p){
  priceColumns <- NULL
  
  for (column in colnames(p)){
    if (regexpr("^localmedianprice_",column)[[1]]==1)
    { 
      
      lenNAPrices<-dim(p[!is.na(p[,column]),])[1]
      print(paste(column,":",lenNAPrices))
      if (lenNAPrices <= 0){
        stop("Prices not available")
      }
      priceColumns<-c(priceColumns,column);
    }
  }
}



pq_regress_x_intercept<-function(pq,x){ return((lm(pq~x))$coeff[["(Intercept)"]]) }
pq_regress_x_coeff<-function(pq,x){ return((lm(pq~x))$coeff[["x"]]) }

pq_regress_logx_coeff<-function(w,logx){ return((lm(w~logx))$coeff[["logx"]]) }
pq_regress_logx_intercept<-function(w,logx){ return((lm(w~logx))$coeff[["(Intercept)"]]) }

#ddply(cp,.(item),summarise,x_coeff=pq_regress_x_coeff(pq=pq,x=x),intercept=pq_regress_x_intercept(pq=pq,x=x))
#ddply(cp,.(item),summarise,x_coeff=pq_regress_logx(w=pq/x,logx=log(x)),intercept=pq_regress_logx_intercept(w=pq/x,logx=log(x)))

merge2010_2012<-function(category){
  ll=lsms_loader(fu=fu,ln=lsms_normalizer);
  ds2010<-ll@combined_data_set(year=2010,selected_category=category, dirprefix='c:/local_files/research/consumption/')
  ds2012<-ll@combined_data_set(year=2012,selected_category=category, dirprefix='c:/local_files/research/consumption/')
  ds2010$y2_hhid<- ds2010$hhid
  ds2010$hhid <-NULL
  ds2010$year <- rep(2010,dim(ds2010)[1])
  ds2012$hhid <-NULL
  ds2012$year <- rep(2012,dim(ds2012)[1])
  
  return(rbind(ds2010,ds2012))
}

sample_vec<-function(vec,n){
  if (length(n)>1){
    stop("n must be a scalar")
  }
  if (dim(vec)<=1 || dim(vec)[1]<n ){
    stop("condition: dim(vec)[1]  > n > 1 must be met")
  }
  return(vec[floor((dim(vec)[1])*runif(n)+1),])
}


write_variables_as_sum<-function(vars){
  prefix=""
  strout=""
  for ( i in vars){
    strout= paste(strout,prefix,i,sep='') 
    prefix="+"
  }
  return(strout)
}

significant_lmvars<-function(ds,depvar,vars_init){
  while(TRUE){
    if (length(vars_init)==0){
      return(NULL)
    }
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
  
  r<-data.frame(dgamma=NULL,dconsu=NULL,deduc=NULL,dage=NULL,docc=NULL,dhous=NULL,drh=NULL)
  validResult= FALSE
  while (validResult==FALSE){
    for (i in seq(nTimes)){
      s1 <- sample_vec(ds,N)
      s2 <- sample_vec(ds,N)
      
      dgamma= mean(s1$visible_consumption)/mean(s1$total_expenditure)-mean(s2$visible_consumption)/mean(s2$total_expenditure) 
      deduc = mean(s1$highest_educ[!is.na(s1$highest_educ)]) -mean(s2$highest_educ[!is.na(s2$highest_educ)])
      dage = mean(s1$age) - mean(s2$age)
      docc = mean(s1$occupation_rank)-mean(s2$occupation_rank)
      dhous = mean(s1$housingstatus) - mean(s2$housingstatus)
      dconsu = mean(s1$consu) - mean(s2$consu)
      drho = fu@find_nonzero_percentile(s1,"visible_consumption",.001)-fu@find_nonzero_percentile(s2,"visible_consumption",.001)
      r<-rbind(r,data.frame(dgamma=dgamma,dconsu=dconsu,deduc=deduc,dage=dage,docc=docc,dhous=dhous,drh=drho))
    } # end for
    
    resReg=significant_lmvars(ds=r,depvar="dgamma",vars_init=all_vars())
    if (!is.null(resReg)){
      validResult<-TRUE
    } else {
      print ("Invalid result (no var significant) found. Skipping the sample.");
    }
  } # end while
  return(resReg)
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


all_vars<-function(){
  return(c("drh","dage","deduc","docc","dhous","dconsu","drh"))
}

create_results_frame<-function(allColNames,fillWith){
  if (missing(fillWith)){
    initVal=NA
  } else {
    initVal=fillWith
  }
  argsList=list()
  argsList[["category"]]=initVal
  
  for (var in allColNames) {
    argsList[[var]]=initVal;
    print
  }
  df<-do.call(data.frame,argsList)
  
  if (missing(fillWith)){
    df<-df[0,] # sets empty data-frame
  }
  return (df)
}

update_results_frame<-function(category,results,coefficients,variables){
  newData <- create_results_frame(allColNames = all_vars(),fillWith = NA)
  
  for (variable in variables){
    newData[,variable]<-coefficients[[variable]]  
  }
  newData$category<-category
  
  return(rbind(results,newData));
}

runtest<-function(m,nTimes){
  #m=items_map()
  if (missing(nTimes)){
    nTimes=400
  }
  results<-create_results_frame(allColNames = c("category",all_vars()));
  for (strcategory in as.character(unique(m$item)))
  {
    category<-as.character(m[m$item==strcategory,]$code)
    
    ll=lsms_loader(fu=fu,ln=lsms_normalizer)
    #ds<-ll@combined_data_set(year=2010,selected_category=lsms_normalizer()@food_categories_lsms_2010(), dirprefix='c:/local_files/research/consumption/')
    ds<-ll@combined_data_set(year=2010,selected_category=category, dirprefix='c:/local_files/research/consumption/')
    
    for (i in seq(10)){
      print (paste("Running iteration ",i))
      res=estimate_dq_drho(ds=ds[ds$region==7,],fu=fu(),N=400,nTimes=nTimes);
      # TODO: use significant selection here
      
      results<-update_results_frame(category=strcategory,results=results,coefficients=coef(res[["result"]]),variables=res[["vars"]])
      
      #      results <- rbind(results,data.frame(category=strcategory,dx=coef(res)["dx"], drh=coef(res)["drh"]))
    }
    print(paste("results for", strcategory))
    print (results[results$category==strcategory,])
  }
  #write.csv(file='c:/temp/results.csv',results)
  return(results)
}

calculate_affordability_popularity<-function(m,fu){
  results<-data.frame(category=NULL,lnslope=NULL,usermean=NULL,popularity=NULL)
  for (strcategory in as.character(unique(m$item)))
  {
    category<-as.character(m[m$item==strcategory,]$code)
    
    ll=lsms_loader(fu=fu,ln=lsms_normalizer)
    ds<-ll@combined_data_set(year=2010,selected_category=category, dirprefix='c:/local_files/research/consumption/')
    lmres<-lm(log(ds$visible_consumption+1e-17)~log(ds$total_expenditure+1e-17))
    userDat <-ds[ds$visible_consumption>0,]
    popularity<-fu@find_nonzero_percentile(ds,"visible_consumption",.001)
    results=rbind(results,data.frame(category=strcategory,
                                     lnslope=coef(lmres)[2],
                                     usermean=mean(userDat$visible_consumption),
                                     popularity=popularity))
    print(results)
  }
  write.csv(file='c:/temp/results.csv',results)
  return(results)
}

temporary_calculations<-function(m,fu){
  aff<-calculate_affordability_popularity(m,fu)
  
  a<-merge(merge(aff,popularity),bw)
  a$drh_scaled<-(a$drh-min(a$drh))/diff(range(a$drh))
}

get_region <- function(regions,districts) {
  region<-(ds[is.element(ds$district,c(1,2,3)) & is.element(ds$region,c(7)),]$region)
  
}
compute_appeal<-function(ln,hh,item,availability){
  # availability is calculated based on whether the item is anywhere in the hh but since its sense involves
  # whether the item can be available somewhere else it's passed as a boolean input (electricity is not available in many areas for example)
  
  #affordability is calculated on whether the expenditure on the item 
  
  
}