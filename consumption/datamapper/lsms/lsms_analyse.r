require(latex2exp)
library(dplyr)

library(maps)
library(ggplot2)
library(ggrepel)
library(acid) # for polarisation

#library(moments) # for kurtosis
setwd('c:/local_files/research/consumption/datamapper/')


source('translation/frameutils.R');source('lsms/lsms_datastorage.R');source('lsms/lsms_normalizer.r');source('lsms/lsms_loader.r');source('lsms/lsms_group_collect.r');ll=lsms_loader(fu=fu,ln=lsms_normalizer,lgc=lgc)

#assign("last.warning", NULL, envir = baseenv())
#showing plots of 2010
#dat <- unique(subset(m2010,abs(price)<10*median_price)[,c("shortname","price")])
#datt <- subset(dat,is.element(shortname,subset(ddply(dat,.(shortname),summarise,n=length(price)),!is.na(shortname) & n>20)$shortname)) %>% mutate(shortname = as.character(shortname))
#par(mar=c(5,7,1,1)); boxplot(price~shortname,data=datt,horizontal=TRUE,las=2)

h <- taskCallbackManager()
h$add(function(expr, value, ok, visible) { options("prompt"=format(Sys.time(), "%H:%M:%S> ")); return(TRUE) }, name = "simpleHandler")

parents_educ_rank <- function(x){
  #1 NO SCHOOL
  #2 SOME PRIMARY
  #3 COMPLETED PRIMARY
  #4 SOME SECONDARY
  #5 COMPLETED SECONDARY
  #6 MORE THAN SECONDARY
  #7 DON'T KNOW
  #  converted to 
  #  le_primary - 1, le_secondary - 2, gt_secondary -3, rest - NA
  if (is.na(x)){
    return(NA)
  }
  if (x >=1 && x <=3) {
    return (1)
  } else {
    if (x>=4 && x<=5 ){
      return (2)
    } else if(x==6){
      return(3)
    }
  }
  return(NA)
  
}
choose_max_non_na <- function (x) { arr = x[!is.na(x)] ; if (length(arr)>1) {return (max(arr))} else {return(0)}}

mean_of_nonzeros <- function (x) { arr = x[x!=0] ; if (length(arr)>0) {return (mean(arr))} else {return(0)}}
choose_max_litlang <- function (x) { 
  arr = x[!is.na(x)];
  ##
  #KISWAHILI...1
  #KISWAHILI & ENGLISH.....2
  #ENGLISH...3
  #ANY OTHER LANGUAGE..4
  #NO .. 5
  if (length(arr)>1) {
    if (is.element(2,arr)){
      return (2)
    } 
    if (is.element(3,arr)){
      return (3)
    } 
    if (is.element(1,arr)){
      return (1)
    }
    if (is.element(4,arr)){
      return (4)
    }
    if (is.element(5,arr)){
      return (5)
    }
  } else {
    return(NA)
  }
}

retrieve_missing_rural_info_2014 <- function(o2014){
  #ddply(unique(o2014[,c("hhid","region","district","ward")]),.(region,district,ward),summarise,n=toJSON(hhid2012))
  
}
add_yob_match_score <-function(k,yobtol,ncdifftol){
  arr <- array();
  for ( i in seq(length(k$YOB_array.x))) { arr[i] <- YOB_similarity(a = fromJSON(k$YOB_array.x[i]), b=fromJSON(k$YOB_array.y[i]),tol=yobtol)}
  k$score <- arr;
  k <- subset(k,score < 15 & score >=0)
  k$ncdiff <- abs(k$num_children.x-k$num_children.y)
  k <- subset(k,ncdiff<=ncdifftol)
  return(k)
}

min_array <- function(k){
  arr = array()
  for (i in seq(length(k))) { 
    arr[i] = min(fromJSON(k[i])) 
  }
  return(arr)
}

count_greater_than <- function(json_array, b){
  arr = array()
  for ( i in seq(length(b))){
    a <- fromJSON(json_array[i])
    if (length(a)>0){
      arr[i] <- length(a[a>b[i]])
    } else {
      arr[i] <-0
    }
  }
  return(arr)
}

prepare_pseudo_panels_2010_2012_2014 <- function (o2010,o2012,o2014, i2010, i2012,i2014, ll,dirprefix,fu,ln,ncdifftol, yobtol, calibrate_needs){
  # Using hh as the unit (not hh-head)
  if (missing(o2010)){
    o2010 <- ll@load_ohs_file(year = 2010, dirprefix = dirprefix,fu=fu, ln=ln)
    o2010 <- plyr::rename(o2010, c("hhid"="hhid2010"))
    write.csv(o2010,'c:/temp/o2010.csv',row.names=FALSE)
  }
  if (missing(o2012)){
    o2012 <- ll@load_ohs_file(year = 2012, dirprefix = dirprefix,fu=fu, ln=ln)
    o2012 <- plyr::rename(o2012, c("hhid"="hhid2012"))
    if (!is.element("YOB",colnames(o2012))){
      o2012$YOB <- 2012 - o2012$age - 1
    }
    write.csv(o2012,'c:/temp/o2012.csv',row.names=FALSE)
  }
  if (missing(o2014)){
    o2014 <- ll@load_ohs_file(year = 2014, dirprefix = dirprefix,fu=fu, ln=ln)
    o2014 <- plyr::rename(o2014, c("hhid"="hhid2014"))
    if (!is.element("YOB",colnames(o2014))){
      o2014$YOB <- 2014 - o2014$age - 1
    }
    write.csv(o2014,'c:/temp/o2014.csv',row.names=FALSE)
  }
  
  if (missing(i2010)){
    #i2010 <- income_data(ll = ll, yr = 2010, dirprefix = dirprefix, fu=fu, ln=ln)
    i2010            <- ll@load_income_file(year = 2010, dirprefix = dirprefix, fu = fu, ln = ln)
    write.csv(i2010, 'c:/temp/i2010.csv',row.names=FALSE)
  }
  if (missing(i2012)){
    #i2012 <- income_data(ll = ll, yr = 2012, dirprefix = dirprefix, fu=fu, ln=ln)
    i2012            <- ll@load_income_file(year = 2012, dirprefix = dirprefix, fu = fu, ln = ln)
    write.csv(i2012, 'c:/temp/i2012.csv',row.names=FALSE)
  }
  if (missing(i2014)){
    #i2014 <- income_data(ll = ll, yr = 2014, dirprefix = dirprefix, fu=fu, ln=ln)
    i2014            <- ll@load_income_file(year = 2014, dirprefix = dirprefix, fu = fu, ln = ln)
    write.csv(i2014, 'c:/temp/i2014.csv',row.names=FALSE)
  }
  
  if (!is.element("hhid2010",colnames(i2010))){
    o2010 <- plyr::rename(o2010,c("hhid"="hhid2010"))
  }
  if (!is.element("hhid2012",colnames(i2012))){
    o2012 <- plyr::rename(o2012,c("hhid"="hhid2012"))
  }
  if (!is.element("hhid2014",colnames(i2014))){
    o2014 <- plyr::rename(o2014,c("hhid"="hhid2014"))
    if (!is.element("YOB",colnames(o2014))){
      o2014$YOB <- 2014 - o2014$age - 1
    }
  }
  
  
  o2010<- merge(o2010,plyr::rename(i2010,c("hhid"="hhid2010")),by=c("hhid2010","personid"),all.x=TRUE)
  
  o2012<- merge(o2012,plyr::rename(i2012,c("hhid"="hhid2012")),by=c("hhid2012","personid"),all.x=TRUE)
  
  o2014<- merge(o2014,plyr::rename(i2014,c("hhid"="hhid2014")),by=c("hhid2014","personid"),all.x=TRUE)
  
  #Adding arrays to ohs frames
  o2010$hhid2010 <- as.character(o2010$hhid2010)
  o2010 <- merge(o2010, ddply(unique(o2010[,c("hhid2010","personid","YOB")]), .(hhid2010), summarise, num_children = length(YOB[(YOB<=2010) & (YOB >= (2010-18))])), by =c("hhid2010"))
  o2010 <- subset(o2010, YOB < 1990)
  
  
  o2012$hhid2012 <- as.character(o2012$hhid2012)
  o2012 <- merge(o2012, ddply(unique(o2012[,c("hhid2012","personid","YOB")]), .(hhid2012), summarise, num_children = length(YOB[(YOB<=2012) & (YOB >= (2012-18))])), by =c("hhid2012"))
  o2012 <- subset(o2012, YOB < 1990)
  
  o2014$hhid2014 <- as.character(o2014$hhid2014)
  o2014 <- merge(o2014, ddply(unique(o2014[,c("hhid2014","personid","YOB")]), .(hhid2014), summarise, num_children = length(YOB[(YOB<=2014) & (YOB >= (2014-18))])), by =c("hhid2014"))
  o2014 <- subset(o2014, YOB < 1990)
  
  common_cols <- c("region","district","housingstatus")
  combine_cols <- c("YOB","education_rank","occupation_rank","num_children","yearly_pay",common_cols)
  ho2010YOB<- ddply(unique((o2010[,c("hhid2010",combine_cols)])),.(hhid2010),summarise, YOB_array=toJSON(sort(YOB)), max_education_rank = choose_max_non_na(education_rank) , max_occupation_rank = choose_max_non_na(occupation_rank) , region= unique(region), district = unique(district), housingstatus=unique(housingstatus), num_children=unique(num_children), sum_yearly_pay = sum(yearly_pay[!is.na(yearly_pay)]))
  ho2012YOB<- ddply(unique((o2012[,c("hhid2012",combine_cols)])),.(hhid2012),summarise, YOB_array=toJSON(sort(YOB)), max_education_rank = choose_max_non_na(education_rank), max_occupation_rank = choose_max_non_na(occupation_rank) , region= unique(region), district = unique(district), housingstatus=unique(housingstatus), num_children=unique(num_children), sum_yearly_pay = sum(yearly_pay[!is.na(yearly_pay)]))
  ho2014YOB<- ddply(unique((o2014[,c("hhid2014",combine_cols)])),.(hhid2014),summarise, YOB_array=toJSON(sort(YOB)), max_education_rank = choose_max_non_na(education_rank), max_occupation_rank = choose_max_non_na(occupation_rank) , region= unique(region), district = unique(district), housingstatus=unique(housingstatus), num_children=unique(num_children), sum_yearly_pay = sum(yearly_pay[!is.na(yearly_pay)]))
  
  if (calibrate_needs){
    #Adding hhead's YOB
    ho2010YOB<- merge(ho2010YOB,plyr::rename(unique(subset(o2010,personid==1)[,c("hhid2010","YOB")]), c("YOB"="headYOB")),by=c("hhid2010"))
    ho2012YOB<- merge(ho2012YOB,plyr::rename(unique(subset(o2012,personid==1)[,c("hhid2012","YOB")]), c("YOB"="headYOB")),by=c("hhid2012"))
    ho2014YOB<- merge(ho2014YOB,plyr::rename(unique(subset(o2014,personid==1)[,c("hhid2014","YOB")]), c("YOB"="headYOB")),by=c("hhid2014"))
    
    personids_with_max_occupation_rank2010 = ddply(unique(merge(o2010[,c("hhid2010","occupation_rank","personid")],ddply(subset(o2010,!is.na(occupation_rank))[,c("hhid2010","occupation_rank")], .(hhid2010), occupation_rank = max(occupation_rank)))[,c("hhid2010","personid")]), .(hhid2010), summarise, personid = min(personid))
    personids_with_max_occupation_rank2012 = ddply(unique(merge(o2012[,c("hhid2012","occupation_rank","personid")],ddply(subset(o2012,!is.na(occupation_rank))[,c("hhid2012","occupation_rank")], .(hhid2012), occupation_rank = max(occupation_rank)))[,c("hhid2012","personid")]), .(hhid2012), summarise, personid = min(personid))
    personids_with_max_occupation_rank2014 = ddply(unique(merge(o2014[,c("hhid2014","occupation_rank","personid")],ddply(subset(o2014,!is.na(occupation_rank))[,c("hhid2014","occupation_rank")], .(hhid2014), occupation_rank = max(occupation_rank)))[,c("hhid2014","personid")]), .(hhid2014), summarise, personid = min(personid))
    
    ho2010YOB <- merge(ho2010YOB,plyr::rename(merge(personids_with_max_occupation_rank2010,unique(o2010[,c("hhid2010","personid","YOB")])), c("YOB"="occYOB")), by=c("hhid2010"))
    ho2012YOB <- merge(ho2012YOB,plyr::rename(merge(personids_with_max_occupation_rank2012,unique(o2012[,c("hhid2012","personid","YOB")])), c("YOB"="occYOB")), by=c("hhid2012"))
    ho2014YOB <- merge(ho2014YOB,plyr::rename(merge(personids_with_max_occupation_rank2014,unique(o2014[,c("hhid2014","personid","YOB")])), c("YOB"="occYOB")), by=c("hhid2014"))
    
    ho2010YOB$minYOB <- min_array(ho2010YOB$YOB_array)
    ho2012YOB$minYOB <- min_array(ho2012YOB$YOB_array)
    ho2014YOB$minYOB <- min_array(ho2014YOB$YOB_array)
    
    ho2010YOB$num_elderly <- count_greater_than(ho2010YOB$YOB_array,ho2010YOB$occYOB)
    ho2012YOB$num_elderly <- count_greater_than(ho2012YOB$YOB_array,ho2012YOB$occYOB)
    ho2014YOB$num_elderly <- count_greater_than(ho2014YOB$YOB_array,ho2014YOB$occYOB)
    
    ho2010YOB$num_mem <- count_greater_than(ho2010YOB$YOB_array,rep(0,dim(ho2010YOB)[1]))
    ho2012YOB$num_mem <- count_greater_than(ho2012YOB$YOB_array,rep(0,dim(ho2012YOB)[1]))
    ho2014YOB$num_mem <- count_greater_than(ho2014YOB$YOB_array,rep(0,dim(ho2014YOB)[1]))
    
    ho2010YOB$num_dep <- with(ho2010YOB,num_children+ num_elderly)
    ho2012YOB$num_dep <- with(ho2012YOB,num_children+ num_elderly)
    ho2014YOB$num_dep <- with(ho2014YOB,num_children+ num_elderly)
    
    ync <- ddply(ho2012YOB,.(occYOB),summarise,qnc = quantile(num_mem,.8)) %>% mutate ( age = 2012 - occYOB)
    ync$lnqnc = log(ync$qnc)
    ync$lnage = log(ync$age)
    ync$agesqr = (ync$age)^2
    res = lm(data=ync, lnqnc ~ lnage + agesqr)
    a <- exp(res$coefficients[[1]])
    b <- res$coefficients[["lnage"]]
    d <- -res$coefficients[["agesqr"]]
    print(paste("a=",a,"b=",b,"d=",d))
    plot(x, a*x^b * exp(-x*x*d),type='l')
    #ync$age_group <- as.integer(ync$age>18 & ync$age<=30)*1 + as.integer(ync$age>30 & ync$age<=40)*2 + as.integer(ync$age>40 & ync$age<=50)*4 + as.integer(ync$age>50 & ync$age<=70)*8 +  as.integer(ync$age>70)*16
    ync$age_group <- as.integer(ync$age>18 & ync$age<=30)*1 + as.integer(ync$age>30 & ync$age<=40)*2 + as.integer(ync$age>40 & ync$age<=50)*4 + as.integer(ync$age>50 & ync$age<=60)*8 +  as.integer(ync$age>60 & ync$age<=70)*16 +   as.integer(ync$age>70 & ync$age<=80)*32 +   as.integer(ync$age>80)*64
    yncc <- ddply(ync,.(age_group),summarise, nc = mean(qnc))
    #barplot(yncc$nc,space=2,main="Number of Children with age (2012)", xlab="age group", ylab="number of children" ,names.arg = c("18+","30","40","50","60","70","80+"), las = 2)
    
    
    return(yncc)
  }
  use_hhid <- FALSE
  
  if (use_hhid){
    #Use the mapping of ids derived from (o201x) on  ho201xYOB to combine them all into one dataframe
    
  } else {
    
    print("Merging data from 2010 and 2012")
    k <- merge( ho2010YOB, ho2012YOB, by=c("region","district","max_education_rank","max_occupation_rank","housingstatus"))
    k <- add_yob_match_score(k,yobtol=yobtol,ncdifftol=ncdifftol)
    
    #print("Merging data from 2010 and 2014")
    #k2 <- merge( ho2014YOB, ho2010YOB, by=c("region","district","max_education_rank","max_occupation_rank","housingstatus"))
    #k2 <- add_yob_match_score(k2,yobtol=yobtol,ncdifftol=ncdifftol)
    
    print("Merging data from 2012 and 2014")
    
    k3 <- merge( ho2012YOB, ho2014YOB, by=c("region","district","max_education_rank","max_occupation_rank","housingstatus"))
    k3 <- add_yob_match_score(k3,yobtol=yobtol, ncdifftol=ncdifftol)
    
    if (is.element("hhid",colnames(i2010))){
      i2010 <- plyr::rename(i2010,c("hhid"="hhid2010"))
    }
    if (is.element("hhid",colnames(i2012))){
      i2012 <- plyr::rename(i2012,c("hhid"="hhid2012"))
    }
    if (is.element("hhid",colnames(i2014))){
      i2014 <- plyr::rename(i2014,c("hhid"="hhid2014"))
    }
    #Choosing minimum score when duplicate hhids are present (due to relaxed matching thresholds)
    panel_columns <- c("region","district","max_education_rank","max_occupation_rank","housingstatus","sum_yearly_pay.x","sum_yearly_pay.y")
    
    h2010_1 <- (ddply(unique(k[,c("hhid2010","score")]), .(hhid2010), summarise, score = min(score)))
    h2012_1 <- (ddply(unique(k[,c("hhid2012","score")]), .(hhid2012), summarise, score = min(score)))
    
    
    k <- (merge((merge(k,h2012_1)),h2010_1))
    
    ki <- ddply(k[,panel_columns],.(region,district,max_education_rank,max_occupation_rank,housingstatus), summarise,region = unique(region), district=unique(district),
                max_education_rank=unique(max_education_rank) , max_occupation_rank=unique(max_occupation_rank), housingstatus=unique(housingstatus) , 
                ypay2010 = mean_of_nonzeros(sum_yearly_pay.x) , ypay2012 = mean_of_nonzeros(sum_yearly_pay.y))
    
    #h2010_2 <- (ddply(unique(k2[,c("hhid2010","score")]), .(hhid2010), summarise, score = min(score)))
    #h2014_2 <- (ddply(unique(k2[,c("hhid2014","score")]), .(hhid2014), summarise, score = min(score)))
    #k2 <- (merge((merge(k2,h2014_2)),h2010_2))
    
    
    h2012_3 <- (ddply(unique(k3[,c("hhid2012","score")]), .(hhid2012), summarise, score = min(score)))
    h2014_3 <- (ddply(unique(k3[,c("hhid2014","score")]), .(hhid2014), summarise, score = min(score)))
    k3 <- (merge((merge(k3,h2014_3)),h2012_3))
    
    ki3 <- ddply(k3[,panel_columns],.(region,district,max_education_rank,max_occupation_rank,housingstatus), summarise,region = unique(region), district=unique(district),
                 max_education_rank=unique(max_education_rank) , max_occupation_rank=unique(max_occupation_rank), housingstatus=unique(housingstatus) , 
                 ypay2012 = mean_of_nonzeros(sum_yearly_pay.x) , ypay2014 = mean_of_nonzeros(sum_yearly_pay.y))
    
    allk <- merge(ki,ki3, by=c("region","district","max_education_rank","max_occupation_rank","housingstatus"), all=TRUE)
    arr  <- array()
    for (i in seq(dim(allk)[1])) {
      x <- allk$ypay2012.x[i] 
      y <- allk$ypay2012.y[i]
      if (is.na(x)){
        arr[i] <- y # NA or 0 would be retained
      } else {
        if (is.na(y)){
          arr[i] <- x
        } else {
          arr[i] <- mean(c(x,y))
        }
      }
      
    }
    allk$ypay2012 <- arr
    
    allk [is.na(allk)] <- 0
    allk <- subset(allk,ypay2010>0 | ypay2012>0 | ypay2014>0 )
    allkk <- ddply(allk,.(region,max_education_rank,max_occupation_rank,housingstatus), summarise, ypay2010 = mean_of_nonzeros(ypay2010),ypay2012 = mean_of_nonzeros(ypay2012),ypay2014 = mean_of_nonzeros(ypay2014))
    allkk$nonzeros <- as.integer(allkk$ypay2010>0) +as.integer(allkk$ypay2012>0) + as.integer(allkk$ypay2014>0)
    #allkk <- (subset(allkk,nonzeros>2))
    #allkk$key <- paste(allkk$region,allkk$max_education_rank, allkk$max_occupation_rank, allkk$housingstatus,sep=":")
    allkk$key <- as.integer(paste("1",sprintf("%02d", allkk$region),sprintf("%01d",allkk$max_education_rank), sprintf("%01d",allkk$max_occupation_rank), sprintf("%01d",allkk$housingstatus),sep=""))
    allkk <- merge( allkk[,c("region","max_education_rank","max_occupation_rank","housingstatus","key")],allkk[,c("key","ypay2010","ypay2012","ypay2014")] %>% gather(year,totinc,-key), by=c("key"))
    #allkk$key <- NULL
    allkk$year <- sapply(allkk$year, function(x) { as.integer(gsub('ypay','',x)) } )
    allkk <- subset(allkk,totinc>0)
    allkk$expensiveregion<-as.integer(is.element(allkk$region,ll@get_expensiveregion_codes()))
    allkk$lntotinc <- log(allkk$totinc)
    return(allkk)
  }
}


infer_2012_2014_tnz_common_hh <- function(ll,dirprefix, fu, ln, pyout){
  
  
  
  o2014 <- ll@load_ohs_file(year = 2014, dirprefix = dirprefix,fu=fu, ln=ln)
  ho2014 <- subset(o2014,!is.na(region) & age >= 24)
  fname <- 'c:/temp/ho2014.csv'
  ho2014[is.na(ho2014)] <- 0
  if (missing(pyout)){
    write.csv(ho2014,fname,row.names = FALSE)
    
    return(fname)
  } else {
    
    o2012 <- ll@load_ohs_file(year = 2012, dirprefix = dirprefix,fu=fu, ln=ln)  
    ho2012 <- (subset(o2012,!is.na(region) & YOB <= 1990))
    ho2012[is.na(ho2012)] <- 0
    
    ho2012YOB<- ddply(unique((ho2012[,c("hhid","YOB","school_leaving_year")])),.(hhid),summarise, YOB_array=toJSON(sort(YOB)), school_leaving_year_array = toJSON(sort(school_leaving_year)))
    
    
    ho2014YOB <- ddply(unique((ho2014[,c("hhid","school_leaving_year")])),.(hhid),summarise, school_leaving_year_array = toJSON(sort(school_leaving_year)))
    #pyout <- read.csv(pyoutfile,stringsAsFactors = FALSE)
    commoncols <- c("hhid","region","district","ward", "roomsnum","ea")
    
    #ho2014YOBexpanded <- merge(pyout,ho2014YOB,by=c("hhid"))
    ho2014YOBsimple   <- unique((ho2014[,c("hhid","age")])) %>% mutate (YOB=2014-age)
    ho2014YOBsimple   <- merge(ho2014YOB,ddply(ho2014YOBsimple,.(hhid),summarise, YOB_array = toJSON(sort(YOB))))
    ho2014YOBsimple   <- plyr::rename(merge( unique(ho2014[,c(commoncols,"hhid2012")]) , ho2014YOBsimple, by = c("hhid")), c("hhid"="hhid2014"))
    
    ho2012YOB         <- plyr::rename(merge( unique(ho2012[,commoncols]) , ho2012YOB, by = c("hhid")), c("hhid"="hhid2012"))
    
    #k <- merge(ho2014YOBsimple,ho2012YOB,by=c("region","district","ward","ea"))
    k <- merge(ho2014YOBsimple,ho2012YOB,by=c("hhid2012"))
    #<investigate>
    #arr= array(); for (i in seq(dim(k)[1]) ) { arr[i] = YOB_similarity(fromJSON(k$YOB_array.x[i]),fromJSON(k$YOB_array.y[i])) }
    #k$score <- arr
    #kk <- subset(k,score!=(-1))
    #</investigate>
    
    
    
    return(k)
  }
}

YOB_similarity <- function(a,b,tol){
  
  if (length(b) < length(a)) {
    temp <- b
    b <- a
    a <- temp
  }
  # a is of shorter length than b
  if (sum(abs(sort(a)-a))!=0){
    stop("array a must be sorted")
  }
  if (sum(abs(sort(b)-b))!=0){
    stop("array b must be sorted")
  }
  start <- b[abs(b-a[1]) == min(abs(b-a[1])) ]
  end   <- b[abs(b-a[length(a)]) == min(abs(b-a[length(a)])) ]
  newa  <- a[a>=(start-tol) & a <= (end+tol)]
  newb  <- b[b>=(start-tol) & b <= (end+tol)]
  
  if (length(newb) == length(newa)){
    if (length(a)==length(b)){
      return( sum(abs(newa-newb)))
    } else {
      return (-1)
    }
  } else {
    return(-2)
  }
  
} 

apply_expand <- function(df,carr,arr,op,groupop){
  
  if (missing(carr)){
    carr <- c()
  }
  if (missing(df)){
    df = data.frame(stringsAsFactors = FALSE)
  }
  
  indices <- seq(length(arr))
  if (length(indices)==1) {
    newdf <- data.frame(x=groupop(c(carr,arr[1])),stringsAsFactors = FALSE)
    df <- rbind(df,newdf)
    return(df)
  } else {
    
    for ( i in indices){
      indices_c <- indices[indices!=i]
      if (missing(op)){
        df <- apply_expand(df=df,groupop=groupop, carr=c(carr,arr[i]),arr=arr[indices_c])
      }
      else {
        df <- apply_expand(df=df,groupop=groupop, carr=c(carr,op(arr[i])),arr=arr[indices_c])
      }
      
    }
    return(df)
  }
}

all_comb <- function(x){
  expand.grid(x=c(10,9),y=c(4,3),z=c(60,59))
}

income_process <-function(){
  commoncols <- c("totinc","personid","region","district","ward","ea","isrural","isurbanp","accessiblemarket",
                  "facilitycode","accessibility","distance","travelcost","expensiveregion","popdensity","occupation",
                  "gender","household_status","inhouse_consumer","inhouse_days_in_month","inhouse_resident","outhouse_days_in_year",
                  "fathers_educ","mothers_educ","married","spouse_resident","outhouse_spouses","years_community","reason_migration",
                  "birthregion","birthdistrict","occupation_rank","is_ge5y","litlang","highest_educ","schoolowner","schoolconveyance",
                  "has_missedschool","educexpense","has_adulteduc","adulteducmonths","education_rank","age",
                  "housingstatus","houserent","roomsnum_primary","roomsnum_secondary","wallsmaterial","roofmaterial","floormaterial","toilet",
                  "cookingfuel","lightingfuel","roomsnum","agegroup","lntotinc","fathers_educrank","mothers_educrank")
  
  i2010 <- plyr::rename(income_data_merged(yr = 2010), c("hhid"="hhid2010"))
  i2012 <- plyr::rename(income_data_merged(yr = 2012), c("hhid"="hhid2012"))
  i2014 <- plyr::rename(income_data_merged(yr = 2014), c("hhid"="hhid2014"))
  if (FALSE){
    mapping2010to2012 <- unique(merge(i2010[,c("hhid2010","region")],i2012[,c("hhid2012","hhid2010")],by=c("hhid2010"))[,c("hhid2012","hhid2010")])
    mapping2012to2014 <- unique(merge(i2012[,c("hhid2012","region")],i2014[,c("hhid2014","hhid2012")],by=c("hhid2012"))[,c("hhid2012","hhid2014")])
    
    panel <- dim(merge(merge(i2014,mapping2012to2014),mapping2010to2012))
  }
  
  i2010$year <- 2010
  i2012$year <- 2012
  i2014$year <- 2014
  
  #i2010[,c(commoncols)]
  write.csv(i2010,'c:/temp/i2010.csv',row.names = FALSE)
  write.csv(i2012,'c:/temp/i2012.csv',row.names = FALSE)
  write.csv(i2014,'c:/temp/i2014.csv',row.names = FALSE)
  return(0)
}

hh_income_data <- function(ll,yr,dirprefix,fu,ln){
  idat            <- ll@load_income_file(year = yr, dirprefix = "../",fu = fu, ln = ln)
  idat            <- ddply(idat, .(hhid), summarise, ypay = sum(yearly_pay))
  idat            <- subset(idat,!is.na(ypay)) %>% mutate ( lnY = log(ypay))
  return(idat)
}


income_data_merged <-function(yr){
  idat            <- ll@load_income_file(year = yr, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  idat            <- ddply(idat, .(hhid), summarise, totinc = sum(yearly_pay))
  odat            <- subset(ll@load_ohs_file(year = yr, dirprefix = "../",fu = fu, ln = lsms_normalizer),personid==1)
  if (!is.element("age",colnames(odat))){
    odat$age        <- yr - odat$YOB
  }
  idat            <- subset(idat,!is.na(totinc))
  idat            <- merge(idat,odat,by=c("hhid"),all.x=TRUE )
  print(paste("Ignoring ",dim(subset(idat,is.na(region)))[1],"entries (",100*dim(subset(idat,is.na(region)))[1]/dim(idat)[1],"%) from the income data due to missing OHS info."))
  idat            <- subset(idat,!is.na(region))
  idat$agegroup   <- as.integer(idat$age<20)*1 + as.integer(idat$age>=20 & idat$age<40)*2 + as.integer(idat$age>=40 & idat$age<60)*3+ as.integer(idat$age>=60 & idat$age<80)*4 + as.integer(idat$age>=80)*5
  idat$lntotinc  <- with(idat,log(totinc+1e-7))
  idat$fathers_educrank <- sapply(as.integer(idat$fathers_educ), parents_educ_rank)
  idat$mothers_educrank <- sapply(as.integer(idat$mothers_educ), parents_educ_rank)
  ag <- (ddply(idat, .(agegroup), summarise, median_income = median(totinc)))
  #plot(ag$agegroup,ag$median_income,type='l',xlab='age group', ylab='median income', main='Incomes by age group')
  
  #ar <- ddply(idat, .(region), summarise, p85_income = quantile(totinc,.85) , n = length(hhid))
  #ar <- subset(ar,n>=5)
  #par(mar=c(5,4,1,1)) ; barplot(ar$p85_income/1e+6,space=2,main="Median Incomes", xlab="region", ylab="85th percentile income (millions)" ,names.arg = ar$region, las = 2)
  
  
  
  return(idat)
}

boxplot_prices <- function(year,thresh){
  mdat <- ll@load_market_prices(year = year, dirprefix = "../",fu = fu, ln = lsms_normalizer,use_pieces = FALSE)
  pdat <- subset(dat,is.element(shortname,subset(ddply(unique(subset(mdat,abs(price)<10*median_price)[,c("shortname","price")]),.(shortname),summarise,n=length(price)),!is.na(shortname) & n>thresh)$shortname)) %>% mutate(shortname = as.character(shortname))
  pdat <- pdat[order(pdat$shortname),]
  par(mar=c(5,7,1,1)); boxplot(price~shortname,data=pdat,horizontal=TRUE,las=2,
                               main = paste("Prices for year - ",year))
}
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

split_sz <- function(N,sz) {
  if (sz > N ){
    return(data.frame(start=1,end=N))
  }
  nsplit <- as.integer(N/sz); 
  start_list  <- seq(1,N,nsplit) ; 
  end_list    <- sapply(nsplit + seq(1,N,nsplit) -1 , function(x) { min(x,N) })
  x <- NULL
  for ( i in seq(length(start_list))) {
    x <- rbind(x,data.frame(start=start_list[i],end=end_list[i]))
  }
  return(x)
}

num_stars <- function(pgtz) {
  return ((as.numeric(pgtz)<.001)*3 + (as.numeric(pgtz)>=.001 & as.numeric(pgtz) < .01)*2 + (as.numeric(pgtz)>=.01 & as.numeric(pgtz) < .05)*1)
}

read_stata_aids_results_files <- function(f,skip_first,precision,split_size){
  
  
  #a <- read.csv('c:/local_files/research/consumption/aidsresults9.csv')
  a <- read.csv(f,header=TRUE)
  b <- a[,c("comm","var","coef","Pgtz")]
  bcoef <-t( b[,c("comm","var","coef")] %>% spread(var,coef))
  #write.csv(bcoef,'c:/temp/bcoef.csv')
  bPgtz <-t( b[,c("comm","var","Pgtz")] %>% spread(var,Pgtz))
  
  fm <- function(df) { for ( i in seq(dim(df)[2])) { df[,i] = format(round(as.numeric(df[,i]),4),nsmall=4) } ; return(df) } 
  
  if (skip_first){
    colnames(bcoef) <- bcoef[1,]
    bcoef <- fm(bcoef[2:dim(bcoef)[1],])
    colnames(bPgtz) <- bPgtz[1,]
    bPgtz <- fm(bPgtz[2:dim(bPgtz)[1],])
  }
  for ( i in seq(dim(bPgtz)[2])){
    #numstars_suffix  <- sapply(num_stars(bPgtz[,i]), function(x) { if (x>0) paste("^{",rep("*",x),"}",sep="") else ""} )
    numstars_suffix = array()
    nstars_array <- num_stars(bPgtz[,i])
    for ( j in seq(length(nstars_array))) {
      if (nstars_array[j]>0){
        numstars_suffix[j] <- paste("$^{",strrep("*",nstars_array[j]),"}$",sep="")
      } else {
        numstars_suffix[j] <- ""
      }
    }
    bPgtz[,i]        <- paste("( ",bPgtz[,i],numstars_suffix," )",sep="")
  }
  
  idnames <- rownames(as.data.frame(bcoef))
  for ( i in seq(1,dim(bPgtz)[1])) {
    y <- as.data.frame(rbind(bcoef[i,],bPgtz[i,]))
    y$idname <- c(idnames[i],"")
    if (i == 1){
      x <- y
    } else {
      x <- rbind( x, y)     
    }
  }
  
  nonidcols <- setdiff(colnames(x),c("idname"))
  
  if (missing(split_size)){
    x <- x [ ,c("idname",nonidcols)]
    return(x)
  } else {
    indices  <- split_sz(length(nonidcols),split_size)
    retlist  <- list()
    for ( i in seq(dim(indices)[1])){
      retlist [[i]] <- x[,c("idname",nonidcols[indices[i,]$start:indices[i,]$end])]
    }
    return(retlist)
  }
  
} 


fill_in_2010_house_prices <- function(){
  ap <-subset(get_asset_average_prices(2012,use_ward=TRUE),shortname=="house")
  a2010 <- subset(ll@read_assets_file(year = 2010, dirprefix = "../", fu = fu, ln = lsms_normalizer),shortname=="house" & number >=1)
  o2010 <- ll@load_ohs_file(year = 2010, dirprefix = "../", fu = fu, ln = lsms_normalizer)
  a2010 <- merge(a2010,unique(o2010[,c("hhid","region","district","ward")]),by=c("hhid"))
  mm <- merge(a2010,ap,by=c("shortname","region","district","ward"),all.x=TRUE)
  print(paste("Ignored",dim(subset(mm,is.na(assetprice)))[1], "entries"))
  mm2 <- subset(mm,!is.na(assetprice))
  
  return(mm2)
}


fill_in_2010_prices <- function(){
  ap <- get_asset_average_prices(2012,FALSE)
  a2010wohouse <- subset(ll@read_assets_file(year = 2010, dirprefix = "../", fu = fu, ln = lsms_normalizer), number >=1 & shortname!="house")
  datwohouse <- merge(a2010wohouse,ap,by=c("shortname")) %>% mutate(asset_mtm = assetprice *number )
  
  mm <- fill_in_2010_house_prices()[,c("hhid","shortname","number","assetprice","itemcode","longname","category")]
  datwhouse <- mm %>% mutate(asset_mtm = assetprice *number )
  dat <- rbind(datwohouse,datwhouse)
  return(dat)
}

get_asset_average_prices <- function(yr,use_ward){
  assetnames_transport   <- c('bike', 'motorbike', 'car')
  #ignored house and land (because their prices may vary too much for us to estimate 2010 values)
  assetnames_household   <- c(  'sewingmachine', 'bed',  'watch',  'chair', 'table', 'cupboard', 'sofa','sports_hobby')#,'land', 'house')
  assetnames_electric     <- c('mobile', 'waterheater','camera', 'phone', 'musicplayer', 'videoplayer', 'musicsystem', 'ac_fan', 'waterpump', 'tv', 'dishtv', 'computer',  'refrigerator' )
  
  assetnames <- c(assetnames_electric,c(assetnames_household,assetnames_transport))
  adat <- ll@read_assets_file(year = yr, dirprefix = "../", fu = fu, ln = lsms_normalizer)
  odat <- ll@load_ohs_file(year = yr, dirprefix = "../", fu = fu, ln = lsms_normalizer)
  if (use_ward==TRUE){
    adat <- merge(adat,unique(odat[,c("hhid","region","district","ward")]),by=c("hhid"))
  }
  assets <- unique(as.character(adat$shortname))
  if (use_ward == TRUE){
    assetpricesdf <- ddply(subset(adat[,c("mtm","hhid","number","shortname","region","district","ward")],number>=1 & !is.na(mtm)),
                           .(shortname,region,district,ward), summarise, assetprice=mean(mtm))
  } else  {
    assetprices <- sapply(assets, function(x) { quantile(with(subset(adat, number>=1 & shortname==x & !is.na(mtm)),mtm),0.85) } )
    assetpricesdf <- data.frame(shortname=assets, assetprice = assetprices)
    rownames(assetpricesdf) <- assetpricesdf$shortname
    assetpricesdf[is.na(assetpricesdf$assetprice),]$assetprice <- 0
  }
  
  
  #dat <- merge(adat,assetpricesdf,by=c("shortname")) %>% mutate(asset_mtm = assetprice *number )
  #k <- ddply(subset(dat,is.element(shortname,assetnames))[,c("hhid","asset_mtm")],.(hhid),summarise,cost = log(sum(asset_mtm)+1e-7))
  
  return (assetpricesdf)
}

merge_asset_mtms_with_prepared_quality_data <- function(allg,m){
  aggdat                                                           <- merge(allg,m,all.x=TRUE,by=c("hhid","year"))
  aggdat[is.na(aggdat$electric_assets_mtm),]$electric_assets_mtm   <- 0
  aggdat[is.na(aggdat$transport_assets_mtm),]$transport_assets_mtm <- 0
  aggdat[is.na(aggdat$household_assets_mtm),]$household_assets_mtm <- 0
  aggdat[is.na(aggdat$all_assets_mtm),]$all_assets_mtm             <- 0
  aggdat$energy_rank   <- as.integer(is.element(aggdat$lightingfuel,c(1,2,8)))*3 + as.integer(is.element(aggdat$lightingfuel,c(3,4,7)))*2 + as.integer(is.element(aggdat$lightingfuel,c(5,6,9,10)))*1
  aggdat$has_electric  <- as.integer(is.element(aggdat$lightingfuel,c(1,8)))
  return(aggdat)
}
mapping_hhids_2010_2012 <- function(o2012){
  return ( plyr::rename(subset(unique(o2012[,c("hhid","hhid2010")]), !is.na(hhid2010)) , c("hhid"="hhid2012"))) 
}
mapping_hhids_2012_2014 <- function(o2014){
  return ( plyr::rename(subset(unique(o2014[,c("hhid","hhid2012")]), !is.na(hhid2012)) , c("hhid"="hhid2014"))) 
}


all_asset_mtms <- function(sum_costs) {
  assetnames_transport   <- c('bike', 'motorbike', 'car')
  #ignored house and land (because their prices may vary too much for us to estimate 2010 values)
  assetnames_household   <- c(  'sewingmachine', 'bed',  'watch',  'chair', 'table', 'cupboard', 'sofa','sports_hobby','land', 'house')
  assetnames_electric     <- c('mobile', 'waterheater','camera', 'phone', 'musicplayer', 'videoplayer', 'musicsystem', 'ac_fan', 'waterpump', 'tv', 'dishtv', 'computer',  'refrigerator' )
  
  commoncols <- c("itemcode" ,"hhid","number","mtm","longname","shortname","category")
  convert = function (x) plyr::rename(x[,commoncols],c ("mtm"="assetprice")) %>% mutate(asset_mtm = assetprice * number)
  
  a2010       <- fill_in_2010_prices()
  a2010$year  <- 2010
  a2012       <- ll@read_assets_file(year = 2012, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  a2012       <- convert(a2012)
  a2012$year  <- 2012
  x           <- rbind(a2010,a2012)
  a2014       <- ll@read_assets_file(year = 2014, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  a2014       <- convert(a2014)
  a2014$year  <- 2014
  x     <- rbind(x,a2014)
  if (sum_costs) {
    transport_assetcosts <- ddply(subset(x,is.element(shortname,assetnames_transport) & !is.na(asset_mtm))[,c("hhid","year","asset_mtm")], .(hhid,year),summarise,transport_assets_mtm = log(sum(asset_mtm)+1) , transport_assets_mtm_sum = sum(asset_mtm))
    household_assetcosts <- ddply(subset(x,is.element(shortname,assetnames_household) & !is.na(asset_mtm))[,c("hhid","year","asset_mtm")], .(hhid,year),summarise,household_assets_mtm = log(sum(asset_mtm)+1), household_assets_mtm_sum = sum(asset_mtm))
    electric_assetcosts  <- ddply(subset(x,is.element(shortname,assetnames_electric) & !is.na(asset_mtm))[,c("hhid","year","asset_mtm")], .(hhid,year),summarise,electric_assets_mtm = log(sum(asset_mtm)+1), electric_assets_mtm_sum = sum(asset_mtm))
    assetcosts           <- ddply(subset(x,!is.na(asset_mtm))[,c("hhid","year","asset_mtm")], .(hhid,year),summarise,all_assets_mtm = log(sum(asset_mtm)+1) , all_assets_mtm_sum = sum(asset_mtm))
    
    y <- merge(assetcosts,merge(electric_assetcosts,merge(transport_assetcosts,household_assetcosts,by=c("hhid","year"),all=TRUE),by=c("hhid","year"),all=TRUE),by=c("hhid","year"),all=TRUE)
    y[is.na(y)]<-0
    return(y)
  }
  else {
    return(x)
  }
}

all_asset_scores <- function(years,dirprefix,fu,ln,ll){
  assetnames_transport   <- c('bike', 'motorbike', 'car')
  #ignored house and land (because their prices may vary too much for us to estimate 2010 values)
  assetnames_household   <- c(  'sewingmachine', 'bed',  'watch',  'chair', 'table', 'cupboard', 'sofa','sports_hobby')#,'land', 'house')
  assetnames_electric     <- c('mobile', 'waterheater','camera', 'phone', 'musicplayer', 'videoplayer', 'musicsystem', 'ac_fan', 'waterpump', 'tv', 'dishtv', 'computer',  'refrigerator' )
  out <- NULL
  for (yr in years){
    cdat <- ll@load_diary_file(dirprefix = dirprefix, year = yr, fu = fu, ln = ln )
    adat <- ll@read_assets_file(year = yr, dirprefix = dirprefix, fu = fu, ln = ln)
    asset_scores_transport <- plyr::rename(ll@get_asset_score(year = yr, diaryData = cdat, assetsData = adat, assetsList = assetnames_transport, ln = ln ),
                                           c("asset_score"="trans_asset_score"))
    asset_scores_household <- plyr::rename(ll@get_asset_score(year = yr, diaryData = cdat, assetsData = adat, assetsList = assetnames_household, ln = ln ),
                                           c("asset_score"="hh_asset_score"))
    asset_scores_electric    <- plyr::rename(ll@get_asset_score(year = yr, diaryData = cdat, assetsData = adat, assetsList = assetnames_electric, ln = ln ),
                                             c("asset_score"="elec_asset_score"))
    asset_scores <- merge(asset_scores_household,asset_scores_transport,by=c("hhid"))
    asset_scores <- merge(asset_scores, asset_scores_electric, by = c("hhid"))
    asset_scores$year <- yr
    out  <- rbind(out,asset_scores)
  }
  out$norm_trans_asset_score <- (out$trans_asset_score - min(out$trans_asset_score)) / ( max(out$trans_asset_score) - min(out$trans_asset_score) )
  out$norm_hh_asset_score <- (out$hh_asset_score - min(out$hh_asset_score)) / ( max(out$hh_asset_score) - min(out$hh_asset_score) )
  out$norm_elec_asset_score <- (out$elec_asset_score - min(out$elec_asset_score)) / ( max(out$elec_asset_score) - min(out$elec_asset_score) )
  return(out)
}


run_test <- function() {
  
  print(unique(ln@lsms_groups_qualitybased_2010_2012()$category))
  if (F){
    g_densefood <- ll@group_collect(year = 2010, dirprefix = "../",categoryName = "densefoods",fu = fu, ln =lsms_normalizer, lgc = lgc, ohs = o2010, hh = c2010, basis = "quality", use_market_prices = T, 
                                    ignore_non_price_for_quality=T,use_diary_costs=T,return_before_agg=F,ld = ldat)
    
    g_transport <- ll@group_collect(year = 2010, dirprefix = "../",categoryName = "transport",fu = fu, ln =lsms_normalizer, lgc = lgc, ohs = o2010, hh = c2010, basis = "quality", use_market_prices = T, 
                                    ignore_non_price_for_quality=T,use_diary_costs=T,return_before_agg=F,ld = ldat)
    
    g_household <- ll@group_collect(year = 2010, dirprefix = "../",categoryName = "household",fu = fu, ln =lsms_normalizer, lgc = lgc, ohs = o2010, hh = c2010, basis = "quality", use_market_prices = T, 
                                    ignore_non_price_for_quality=T,use_diary_costs=T,return_before_agg=F,ld = ldat)
    
    g_nonfresh <- ll@group_collect(year = 2010, dirprefix = "../",categoryName = "nonfresh",fu = fu, ln =lsms_normalizer, lgc = lgc, ohs = o2010, hh = c2010, basis = "quality", use_market_prices = T, 
                                   ignore_non_price_for_quality=T,use_diary_costs=T,return_before_agg=F,ld = ldat)
    
    g_complements <- ll@group_collect(year = 2010, dirprefix = "../",categoryName = "complements",fu = fu, ln =lsms_normalizer, lgc = lgc, ohs = o2010, hh = c2010, basis = "quality", use_market_prices = T, 
                                   ignore_non_price_for_quality=T,use_diary_costs=T,return_before_agg=F,ld = ldat)
  
    g_fruitsveg <- ll@group_collect(year = 2010, dirprefix = "../",categoryName = "fruitsveg",fu = fu, ln =lsms_normalizer, lgc = lgc, ohs = o2010, hh = c2010, basis = "quality", use_market_prices = T, 
                                      ignore_non_price_for_quality=T,use_diary_costs=T,return_before_agg=F,ld = ldat)
    
    g_protein <- ll@group_collect(year = 2010, dirprefix = "../",categoryName = "protein",fu = fu, ln =lsms_normalizer, lgc = lgc, ohs = o2010, hh = c2010, basis = "quality", use_market_prices = T, 
                                    ignore_non_price_for_quality=T,use_diary_costs=T,return_before_agg=F,ld = ldat)
    
    r <- data.frame()
    r <- rbind(r,g_densefood)
    r <- rbind(r,g_transport)
    r <- rbind(r,g_household)
    r <- rbind(r,g_nonfresh)
    r <- rbind(r,g_complements)
    r <- rbind(r,g_fruitsveg)
    r <- rbind(r,g_protein)
    
  }
  
  
  example_df = data.frame(hhid=c('B','C'),category=c('protein','household'),quality=c(.2,NA),min_price=c(1,2),tot_categ_exp=c(10,20))

  
  }

combine_group_collect_into_quality_df<-function(groupcollectinput_df)
{
  categories <- unique(r$category)

  rename_quality_columns <- c("protein"="lnVprotein","household"="lnVhousehold")
  rename_minprice_columns <- c("protein"="lpprotein","household"="lphousehold")
  rename_weight_columns <- c("protein"="lpprotein","household"="lphousehold")
  if (length(setdiff(categories,names(rename_quality_columns)))>0){
    stop("Unsupported categories")
  }
  if (length(setdiff(categories,names(rename_quality_columns)))>0){
    stop("Unsupported categories")
  }
  if (length(setdiff(categories,names(rename_quality_columns)))>0){
    stop("Unsupported categories")
  }
  
  groupcollect_df <- groupcollectinput_df %>%  mutate (lp_min_price=log(min_price))
  
  min_prices_df <- groupcollect_df[,c('hhid','category','lp_min_price')] %>% pivot_wider(names_from='category',values_from='lp_min_price') %>% plyr::rename(rename_minprice_columns)  
  totexp_df <- ddply(groupcollect_df[,c('hhid','category','tot_categ_exp')],.(hhid),summarise,total_expenditure=sum(tot_categ_exp))
  
  lnV_df <- groupcollect_df[,c('hhid','category','quality')] %>% pivot_wider(names_from='category',values_from='quality') %>% plyr::rename(rename_quality_columns)
  
  weights_df <- (merge(groupcollect_df,totexp_df,all.y=T) %>% mutate(w_categ=tot_categ_exp/total_expenditure))[,c("hhid",'category',"w_categ")]
  weights_df <- weights_df %>% pivot_wider(names_from='category',values_from='w_categ') %>% plyr::rename(rename_weight_columns)
  
  results_df <- merge(totexp_df,lnV_df,all.x=T)
  results_df <- merge(results_df,weights_df,by=c('hhid'))
  return(results_df)
  
}
analyse_estimation_df <- function(res,use_nu) {
  
  # The age-wise decomposition of regression results is only to demonstrate that 
  # a time dependent coefficient is necessary. 
  # the following form of a(t) is expected :
  # a1 <- 2; a2 <- 2; par(mfrow=c(1,1)); x <- seq(-1,10,.1) ; plot(x,1-(1/((x**a1)*exp(-x/a2))),type='l')
  p.df  <- res[["df"]]
  agedf <- data.frame(start=c(20,30,40,50,60), end=c(30,40,50,60,200))
  r <- data.frame(stringsAsFactors = FALSE)
  for ( i in seq(nrow(agedf))){
    if (use_nu){
      res <- lm(data=subset(p.df, age >=agedf[i,]$start & age <agedf[i,]$end), nut1~At+lt1)
    } else {
      res <- lm(data=subset(p.df, age >=agedf[i,]$start & age <agedf[i,]$end), dAt~At+lt1)
    }
    
    r <- rbind(r, data.frame ( start=agedf[i,]$start, end= agedf[i,]$end, coef.At = res$coefficients[["At"]], 
                               coef.lt1 = res$coefficients[["lt1"]],
                               intercept = res$coefficients[["(Intercept)"]] ) )
  }
  par(mfrow=c(2,1))
  plot( (r$start+r$end)/2,  r$coef.At,type='l')
  plot( (r$start+r$end)/2,  r$coef.lt1,type='l')
  return(r)
  
}


get_local_clothing_expenditure <-function (ld,o2012,c2012,o2014,c2014)
{
  o2012whs <- merge( o2012, ddply(o2012,.(hhid),summarise, hsize=length(personid)), by = c("hhid"))
  cdat2012 <- merge((unique(o2012whs[,c("hhid","region","district","hsize")])),c2012,by=c("hhid")) %>% mutate( avcost = cost/hsize)
  clothing_exp2012 <- ddply(subset(cdat2012,is.element(shortname,c("mensclothes","womensclothes","childrensclothes","mensshoes","womensshoes","childrensshoes")))[,c("hhid","region","district","cost","hsize")],.(hhid), clothes_cost = sum(cost)) %>% mutate(avcost = cost/hsize)
  clothing_exp_agg2012 <- ddply(clothing_exp2012,.(region),summarise, avcost2012 = mean(avcost))
  
  o2014whs <- merge( o2014, ddply(o2014,.(hhid),summarise, hsize=length(personid)), by = c("hhid"))
  cdat2014 <- merge((unique(o2014whs[,c("hhid","region","district","hsize")])),c2014,by=c("hhid")) %>% mutate( avcost = cost/hsize)
  clothing_exp2014 <- ddply(subset(cdat2014,is.element(shortname,c("mensclothes","womensclothes","childrensclothes","mensshoes","womensshoes","childrensshoes")))[,c("hhid","region","district","cost","hsize")],.(hhid), clothes_cost = sum(cost)) %>% mutate(avcost = cost/hsize)
  clothing_exp_agg2014 <- ddply(clothing_exp2014,.(region),summarise, avcost2014 = mean(avcost))
  res <- merge(clothing_exp_agg2014,clothing_exp_agg2012, by=c("region"))
  #extrapolate 2010 prices with cpi
  hcpi <- ld@get_household_cpi()
  res$cpi2010<- subset(hcpi,year==2010)$price
  res$cpi2012<- subset(hcpi,year==2012)$price
  res$cpi2014<- subset(hcpi,year==2014)$price
  res <- res %>% mutate (r2012 = avcost2012/cpi2012) %>% mutate (r2014 = avcost2014/cpi2014) %>% mutate( r2010 = (r2012+r2014)/2 ) %>% mutate( avcost2010 = r2010 * cpi2010)
  res$r2010 <- NULL
  res$r2012 <- NULL
  res$r2014 <- NULL
  res$cpi2010 <- NULL
  res$cpi2012 <- NULL
  res$cpi2014 <- NULL
  return(res)
}

get_local_median_house_rent <- function(odat){
  #  the total rent varies across regions or district can be seen with the following:
  #  ddply(subset(rent2010whs, !is.na(region)), .(region,district), summarise, mean_rent = mean(houserent) , n = length(hhid) , sd_rent = sd(houserent))
  #  What we wish to do is to bring down the granularity to a level where variance is low and 
  #  then use that rent for those who don't pay the rent (quantiles can be used for the purpose). For now we circumvent the process with use of medians
  rentdat     <- unique(subset(odat, housingstatus==4)[,c("hhid","region","district","houserent")])
  medrent     <- ddply(subset(rentdat, !is.na(region)), .(region,district), summarise, med_rent = median(houserent) )
  rentdatres  <- merge(rentdat,medrent,by=c("region","district"))
  
  return(rentdatres)
}

get_housing_cost_df <- function(){
  r <- data.frame()
  # 1- owner occupied, 2- EMPLOYER PROVIDED - SUBSIDIZED, 3-EMPLOYER PROVIDED - FREE, 4- RENTED, 5- FREE, 6-NOMADS 
  r <- rbind(r,data.frame(housingstatus=5,has_house=1))
  r <- rbind(r,data.frame(housingstatus=4,has_house=0))
  r <- rbind(r,data.frame(housingstatus=3,has_house=0))
  r <- rbind(r,data.frame(housingstatus=2,has_house=0))
  r <- rbind(r,data.frame(housingstatus=1,has_house=1))
  return(r)
}

analyse_house_maintenance_2010 <- function(c2010, o2010){
  # We use these only to arrive at the percentage that would be taken as maintenance cost of the house
  
  #Following can check that very few houses that rent have repair costs
  #co2010 <- merge(c2010, unique(o2010[,c("hhid","housingstatus")]) , by=c("hhid"))
  #ddply(subset(co2010, shortname=="house_repair_monthly" & cost >0), .(housingstatus), summarise, n = length(unique(hhid)))
  
  rentdat2010 <- get_local_median_house_rent(o2010)
  
  alldat2010      <- merge(merge(o2010,rentdat2010,all.x=TRUE)[,c("region","district","housingstatus","hhid","med_rent")],get_housing_cost_df(),by=c("housingstatus"))
  
  if (dim(alldat2010[is.na(alldat2010$med_rent),])[1]>0){
    alldat2010[is.na(alldat2010$med_rent),]$med_rent <- 0  
  }
  
  hownerhids2010     <- unique(subset(o2010, housingstatus == 1)$hhid)
  repair2010  <- subset(c2010, shortname=="house_repair_monthly" & cost >0 & is.element(hhid,hownerhids2010))
  repair2010 <- merge(repair2010,subset(o2010[,c("hhid","region","district")] , !is.na(region)), by=c("hhid")) # adding region district
  medrepair2010 <- ddply(repair2010,.(region,district),summarise, med_maint = median(cost))
  alldatf2010      <- merge(alldat2010,medrepair2010)
  alldatf2010$housing_cost <- as.integer(alldatf2010$has_house==1) * alldatf2010$med_maint + as.integer(alldatf2010$has_house==0)*alldatf2010$med_rent
  
  return(alldatf2010)
}


analyse_house_maintenance_2012_2014 <- function(cdat, odat,adat){
  ohdat   <- ddply(odat, (.hhid), summarise, region = unique(region), district = unique(district), houserent = sum(houserent)) [ ,c("hhid","region","district","houserent")]
  rentdat <- get_local_median_house_rent(ohdat)
  
  alldat      <- merge(merge(ohdat,rentdat,all.x=TRUE)[,c("region","district","housingstatus","hhid","med_rent")],get_housing_cost_df(),by=c("housingstatus"))
  
  if (dim(alldat[is.na(alldat$med_rent),])[1]>0){
    alldat[is.na(alldat$med_rent),]$med_rent <- 0  
  }
  
  hownerhids   <- unique(subset(ohdat, housingstatus == 1)$hhid)
  houseprices  <- subset(adat, number>0 & shortname=="house")[,c("hhid","mtm")]
  repairdat    <- subset(cdat, shortname=="house_repair_yearly" & cost >0 & is.element(hhid,hownerhids))
  repairdat    <- merge(repairdat,subset(ohdat[,c("hhid","region","district")] , !is.na(region)), by=c("hhid")) # adding region district
  medrepair    <- ddply(repairdat,.(region,district),summarise, med_maint = median(cost)) # perform year to month conversion
  
  alldatf      <- merge(alldat,medrepair)
  
  alldatf$housing_cost <- as.integer(alldatf$has_house ==1) * alldatf$med_maint + as.integer(alldatf$has_house==0)*alldatf$med_rent
  
  return(alldatf)
}

assign_house_maintenance <- function (a2010, a2012, a2014, o2010, o2012, o2014, truncateval, fee) {
  
  if (missing(fee)){
    fee <- .025
  }
  
  homeownerhouses2010 <- merge ( subset( merge(a2012, unique(o2012[,c("hhid","hhid2010")]) ) , shortname=="house" & number >0) , plyr::rename(a2010,c("hhid"="hhid2010")) )
  
  oh2010   <- ddply(o2010, .(hhid), summarise, houserent = sum(houserent)) [ ,c("hhid","houserent")]
  rent2010     <- plyr::rename(subset(oh2010,houserent>0 & !is.na(houserent))[,c("hhid","houserent")] , c("houserent" = "running_cost", "hhid"="hhid2010"))
  norent2010wh <- subset((merge(plyr::rename(subset(oh2010,houserent==0 || is.na(houserent)), c("hhid"="hhid2010"))[,c("hhid2010","houserent")],homeownerhouses2010[,c("hhid2010","mtm")]) %>% mutate(running_cost = fee*mtm*1.2)) [ ,c("hhid2010","running_cost")] , !is.na(running_cost))
  
  runningcosts2010 <- rbind(rent2010,norent2010wh)
  runningcosts2010$running_cost <- sapply(runningcosts2010$running_cost,function(x){ min(x,truncateval)})
  
  oh2012               <- ddply(o2012, .(hhid), summarise, houserent = sum(houserent)) [ ,c("hhid","houserent")]
  homeownerhouses2012  <- subset( a2012,  shortname=="house" & number >0)
  rent2012             <- plyr::rename(subset(oh2012,houserent>0 & !is.na(houserent))[,c("hhid","houserent")] , c("houserent" = "running_cost"))
  norent2012wh         <- subset((merge(subset(oh2012,houserent==0 || is.na(houserent)) [,c("hhid","houserent")],homeownerhouses2012[,c("hhid","mtm")]) %>% mutate(running_cost = fee*mtm) ) [ ,c("hhid","running_cost")] , !is.na(running_cost))
  runningcosts2012 <- rbind(rent2012,norent2012wh)
  runningcosts2012$running_cost <- sapply(runningcosts2012$running_cost,function(x){ min(x,truncateval)})
  
  oh2014               <- ddply(o2014, .(hhid), summarise, houserent = sum(houserent)) [ ,c("hhid","houserent")]
  homeownerhouses2014  <- subset( a2014,  shortname=="house" & number >0)
  rent2014             <- plyr::rename(subset(oh2014,houserent>0 & !is.na(houserent))[,c("hhid","houserent")] , c("houserent" = "running_cost"))
  norent2014wh         <- subset( (merge(subset(oh2014,houserent==0 || is.na(houserent)) [,c("hhid","houserent")],homeownerhouses2014[,c("hhid","mtm")]) %>% mutate(running_cost = fee*mtm) ) [ ,c("hhid","running_cost")] , !is.na(running_cost))
  runningcosts2014     <- rbind(rent2014,norent2014wh)
  runningcosts2014$running_cost <- sapply(runningcosts2014$running_cost,function(x){ min(x,truncateval)})
  #the following gives a quantile comparison
  #data.frame(rent2010=quantile(rent2010$running_cost,seq(20)/20), norent2010 = quantile(norent2010wh$running_cost, seq(20)/20),rent2012=quantile(rent2012$running_cost,seq(20)/20), norent2012 = quantile(norent2012wh$running_cost, seq(20)/20) , rent2014=quantile(rent2014$running_cost,seq(20)/20), norent2014 = quantile(norent2014wh$running_cost, seq(20)/20))
  hc <- list()
  hc[["hc2010"]] <- runningcosts2010
  hc[["hc2012"]] <- runningcosts2012
  hc[["hc2014"]] <- runningcosts2014
  return(hc)
  
}

get_perception_rank <-function(r){
  if (is.na(r)){
    return(NA)
  }
  if(r == 1 || r == 2 || r == 3 || r==4) {
    return(T)
  } else if ( r==5 || r==6 || r==7) {
    return(F)
  } else {
    return (NA)
  }
}


plot_population_heat_map <- function(odat){
  df <- unique(odat[,c("S","E","population")])
  
} 


init_data <- function(use_ea){
  
  o2010 <- ll@load_ohs_file(year = 2010, dirprefix = "../",fu=fu, ln=lsms_normalizer) ; 
  o2012 <- ll@load_ohs_file(year = 2012, dirprefix = "../",fu=fu, ln=lsms_normalizer) ; 
  o2014 <- ll@load_ohs_file(year = 2014, dirprefix = "../",fu=fu, ln=lsms_normalizer) ;
  a2010 <- ll@read_assets_file(year = 2010, dirprefix = "../",fu = fu, ln = lsms_normalizer) ; 
  a2012 <- ll@read_assets_file(year = 2012, dirprefix = "../",fu = fu, ln = lsms_normalizer) ; 
  a2014 <- ll@read_assets_file(year = 2014, dirprefix = "../",fu = fu, ln = lsms_normalizer) ; 
  c2010 <- ll@load_diary_file(dirprefix = "../",year = 2010, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  c2012 <- ll@load_diary_file(dirprefix = "../",year = 2012, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  c2014 <- ll@load_diary_file(dirprefix = "../",year = 2014, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  #e <- minimum_needs_cost_per_head(ll= ll, c2010 = c2010, c2012 = c2012, c2014 = c2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)
  #res <- plain_asset_differences_2012_2014(a2012 = a2012, a2014 = a2014, o2012 = o2012, o2014 = o2014, pivot_asset="bed)
  #p <- prepare_pseudo_panels_2010_2012_2014(o2010 = o2010, o2012 = o2012, o2014 = o2014, ll =ll , dirprefix = "../", fu=fu, ln=lsms_normalizer,ncdifftol = 2, yobtol = 3, i2010 = i2010, i2012 = i2012, i2014 = i2014,calibrate_needs=FALSE) 
  #pres <- estimation_df(e = e, a2010=a2010,a2012= a2012, a2014 = a2014, o2010 = o2010, o2012 = o2012, o2014 = o2014, c2010=c2010, c2012=c2012, c2014=c2014)
  #hist(sapply(res[["x"]]$expenditure,logx),breaks=100)
  tn <- get_nonparametric_df(ll = ll,ln=lsms_normalizer,food_analysis = F, use_ea=use_ea, o2010=o2010, o2012=o2012, o2014=o2014, a2010=a2010, a2012=a2012, a2014=a2014, c2010=c2010, c2012=c2012, c2014=c2014 )
  return(tn)
  }

pick_non_na_outoffood_reason <- function(reason1,reason2,reason3){
  reasons <- c()
  if (!is.na(reason1)){
    reasons <- c(reasons,reason1)
  }
  if (!is.na(reason2)){
    reasons <- c(reasons,reason2)
  }
  if (!is.na(reason3)){
    reasons <- c(reasons,reason3)
  }
  
  return(jsonlite::toJSON(reasons))
}

outoffood_due_to_costs <- function(reasons_string){
  reasons <- jsonlite::fromJSON(reasons_string)
  if (length(intersect(c("5","6"),reasons))>0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

outoffood_due_to_agri <- function(reasons_string){
  reasons <- jsonlite::fromJSON(reasons_string)
  if (length(intersect(c("1","2","3","4"),reasons))>0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

plot_out_of_food<-function(o2010,reason){
  world_map <- map_data("world")
  tnz_map = subset(world_map ,region=="Tanzania")
  o2010_woutoffoodreasons <- o2010
  o2010_woutoffoodreasons$outoffood_reasons <- mapply(pick_non_na_outoffood_reason,o2010_woutoffoodreasons$outoffood_reason1, o2010_woutoffoodreasons$outoffood_reason2,o2010_woutoffoodreasons$outoffood_reason3)
  

  
  if (reason == "costs"){
    o2010_woutoffoodreasons$outoffood_costs <- mapply(outoffood_due_to_costs,o2010_woutoffoodreasons$outoffood_reasons)
    dat <- ddply(subset(o2010_woutoffoodreasons,household_status==1),.(region,district,region_name,S,E),summarise,n=length(hhid),n_outoffood_costs=sum(outoffood_costs)) %>% mutate(fraction_outoffood_costs=n_outoffood_costs/n)
    p <- ggplot()+geom_polygon(data=tnz_map, aes(x=long, y=lat, group=group), 
                          colour="light yellow", fill="light yellow") + geom_point(data=dat,aes(x=E, y=S, size = fraction_outoffood_costs))+ scale_size(range = c(.5, 8), name="out-of-food")  + geom_label_repel(data=dat, aes(x=E,y=S, label=ifelse(district==1,as.character(region_name),'')),box.padding = .3, point.padding = .5, segment.color ='grey50') + ggtitle("Out of Food HH - High Costs (2010)")
    print(p)
    
  }
  else if (reason == "agri"){
    o2010_woutoffoodreasons$outoffood_agri <- mapply(outoffood_due_to_agri,o2010_woutoffoodreasons$outoffood_reasons)
    dat <- ddply(subset(o2010_woutoffoodreasons,household_status==1),.(region,district,region_name,S,E),summarise,n=length(hhid),n_outoffood_agri=sum(outoffood_agri)) %>% mutate(fraction_outoffood_agri=n_outoffood_agri/n)
    
    p <- ggplot()+geom_polygon(data=tnz_map, aes(x=long, y=lat, group=group), 
                          colour="light yellow", fill="light yellow") + geom_point(data=dat,aes(x=E, y=S, size = fraction_outoffood_agri))+ scale_size(range = c(.5, 8), name="out-of-food")  + geom_label_repel(data=dat, aes(x=E,y=S, label=ifelse(district==1,as.character(region_name),'')),box.padding = .3, point.padding = .5, segment.color ='grey50') + ggtitle("Out of Food HH - Agricultural Shortage (2010)")
    print(p)
  }   else{
    stop(paste("Unknown Reason :",reason))
  }
  
}


plot_q_A_heatmap<-function(tn,year){
  world_map <- map_data("world")
  tnz_map = subset(world_map ,region=="Tanzania")
  dat <-tn[[paste0("df",year)]]
  dat$log_q_by_A <- ((mapply(function(logq,logA) { if (logq<=0) { return(0)} else {return((logq-logA))}},dat$log_q_ne,dat$lnA0)))
  simple_normalise <- function(x,minval,maxval) {return ((x-minval)/(maxval-minval))}
  
  ggplot()+geom_polygon(data=tnz_map, aes(x=long, y=lat, group=group), 
                        colour="light yellow", fill="light yellow") + geom_point(data=dat,aes(x=E, y=S, size = log_q_by_A))+ scale_size(range = c(.1, 10), name="excess per asset")  + ggtitle("excess per asset (2012)")
  #tnz_map$colour_to_fill <- mapply( function(lat,long,m) { if (lat**2+long**2 >m){return("yellow")} else {return("blue")}} , tnz_map$lat, tnz_map$long, median(tnz_map$long**2+tnz_map$lat**2))
}

plot_region_map <- function(plot_type,e){
  
  world_map <- map_data("world")
  tnz_map = subset(world_map ,region=="Tanzania")
  hhid_mtms_2012 <- ddply(subset(a2012,!is.na(number)  & !is.na(mtm) & number>0), .(hhid), summarise, hhid_mtm=sum(mtm*number))
  hhid_mtms_o2012 <- merge(o2012,hhid_mtms_2012 ,by="hhid")
  
  if (plot_type=="occupation"){
    hhid_mtms_o2012_woccup <- subset(hhid_mtms_o2012, !is.na(occupation_rank))
    map_data <- subset(ddply(hhid_mtms_o2012_woccup,.(region,district,S,E,region_name),summarise, mean_a = median(hhid_mtm), occupation_rank=mean(occupation_rank)), !is.na(region))
    ggplot()+geom_polygon(data=tnz_map, aes(x=long, y=lat, group=group), 
                          colour="light yellow", fill="light yellow") + geom_point(data=map_data,aes(x=E, y=S, size = mean_a, color = occupation_rank))+ scale_size(range = c(.1, 10), name="assets_value") + geom_label_repel(data=map_data, aes(x=E,y=S, label=ifelse(district==1,as.character(region_name),'')),box.padding = .3, point.padding = .5, segment.color ='grey50') + ggtitle("Asset Values and Occupation Rank distribution in Tanzania (2012)")
  } else if (plot_type == "education") {
    hhid_mtms_o2012_weduc <- subset(hhid_mtms_o2012, !is.na(education_rank))
    map_data <- subset(ddply(hhid_mtms_o2012_weduc,.(region,district,S,E,region_name),summarise, mean_a = median(hhid_mtm), education_rank=mean(education_rank)), !is.na(region))
    ggplot()+geom_polygon(data=tnz_map, aes(x=long, y=lat, group=group), 
                          colour="light yellow", fill="light yellow") + geom_point(data=map_data,aes(x=E, y=S, size = mean_a, color = education_rank))+ scale_size(range = c(.1, 10), name="assets_value") + geom_label_repel(data=map_data, aes(x=E,y=S, label=ifelse(district==1,as.character(region_name),'')),box.padding = .3, point.padding = .5, segment.color ='grey50') + ggtitle("Asset Values and Education Rank distribution in Tanzania (2012)")
  } else if (plot_type == "population") {
    pop <- unique(o2012[,c("region","district","population")])
    map_data_pop <- merge(map_data,pop)
    ggplot()+geom_polygon(data=tnz_map, aes(x=long, y=lat, group=group), 
                          colour="light yellow", fill="light yellow") + geom_point(data=map_data_pop,aes(x=E, y=S, size = population))+ scale_size(range = c(.1, 10), name="population") + geom_label_repel(data=map_data, aes(x=E,y=S, label=ifelse(district==1,as.character(region_name),'')),box.padding = .3, point.padding = .5, segment.color ='grey50') + ggtitle("Population distribution in Tanzania (2012)")
    
  }  else if(plot_type=="food_prices") {
    if (missing(e)){
      e <- minimum_needs_cost_per_head(ll = ll, c2010 = c2010, c2012 = c2012, c2014 = c2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)
    }
    costs_data_map <- merge(map_data,e$df2012)
    ggplot()+geom_polygon(data=tnz_map, aes(x=long, y=lat, group=group), 
                          colour="light yellow", fill="light yellow") + geom_point(data=costs_data_map,aes(x=E, y=S, size = foodbasket_cost ))+ scale_size(range = c(.1, 10), name="food-basket price") + geom_label_repel(data=map_data, aes(x=E,y=S, label=ifelse(district==1,as.character(region_name),'')),box.padding = .3, point.padding = .5, segment.color ='grey50') + ggtitle("Food Prices in Tanzania (2012)")
  }
  else if(plot_type=="housing_prices") {
    if (missing(e)){
      e <- minimum_needs_cost_per_head(ll = ll, c2010 = c2010, c2012 = c2012, c2014 = c2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)
    }
    costs_data_map <- merge(map_data,e$df2012)
    ggplot()+geom_polygon(data=tnz_map, aes(x=long, y=lat, group=group), 
                          colour="light yellow", fill="light yellow") + geom_point(data=costs_data_map,aes(x=E, y=S, size = housing_cost ))+ scale_size(range = c(.1, 10), name="housing price") + geom_label_repel(data=map_data, aes(x=E,y=S, label=ifelse(district==1,as.character(region_name),'')),box.padding = .3, point.padding = .5, segment.color ='grey50') + ggtitle("Housing Prices in Tanzania (2012)")
  }
  
  else {
    stop("Unknown Plot type")
  }
  
  #ggplot()+geom_polygon(data=tnz_map, aes(x=long, y=lat, group=group), 
  #                      colour="light green", fill="light green") +geom_point(data=map_data,aes(x=E, y=S, size = mean_a))
  
  
  # + geom_text(data=map_data, aes(x=E,y=S, label=ifelse(district==1,as.character(region_name),'')),hjust=0,vjust=0)
  
}


income_estimation_stata_input <- function(pseudop){
  # get income estimate from the RE estimator results obtained from pseudo-panel - xtreg lntotinc i.max_education_rank i.max_occupation_rank i.expensiveregion, re
  pseudop <- merge(get_housing_cost_df(),pseudop,by=c("housingstatus")) %>% mutate (is_higheduc = as.integer(max_education_rank==4))
  return(pseudop)
}
get_stata_income_re_results <- function(){
  #xtreg lntotinc i.is_higheduc i.max_occupation_rank i.expensiveregion, re
  eduf <- data.frame(stringsAsFactors = FALSE)
  eduf <- rbind(eduf,data.frame(is_higheduc=1,edufcoef=1.269024))
  eduf <- rbind(eduf,data.frame(is_higheduc=0,edufcoef=0))
  
  
  occf <- data.frame(stringsAsFactors = FALSE)
  occf <- rbind(occf,data.frame(occupation_rank= 0,occcoef=0	))
  occf <- rbind(occf,data.frame(occupation_rank= 1,occcoef=-3.0891	))
  occf <- rbind(occf,data.frame(occupation_rank= 2,occcoef=2.152399))
  occf <- rbind(occf,data.frame(occupation_rank= 3,occcoef=2.767533))
  
  expf <- data.frame(stringsAsFactors = FALSE)
  expf <- rbind(expf,data.frame(expensiveregion=1,expcoeff=	.6500749))
  expf <- rbind(expf,data.frame(expensiveregion=0,expcoeff=	0))
  cons <- 11.49585
  
  res=list()
  res[["eduf"]] = eduf
  res[["occf"]] = occf
  res[["expf"]] = expf
  res[["const"]] = cons
  
  return(res)
}

perception_data_analysis <- function(o2012, ignore_split_hhids)
{
  use_discrete <- F
  dirprefix <- "../"
  fu <- fu
  ln <- lsms_normalizer
  ll <- ll
  k2012 <- ll@load_perception_data(year=2012,dirprefix=dirprefix,fu=fu,ln=ln)
  
  if (use_discrete){
    ko2012 <- merge(k2012,o2012, by =c("hhid","personid"))
    ko2012$healthOK <- sapply(ko2012$health_perception,get_perception_rank)
    ko2012$financeOK <- sapply(ko2012$finance_perception,get_perception_rank)
    ko2012$lifeOK <- sapply(ko2012$life_perception,get_perception_rank)
    
    kh <- ddply(unique(ko2012[,c("hhid","lifeOK","financeOK","healthOK")]),.(hhid),summarise,healthOK=max(healthOK),financeOK=max(financeOK),lifeOK=max(lifeOK))
  } else {
    
    kh2012 <- ddply(unique(k2012[,c("hhid","life_perception","finance_perception","health_perception")]),.(hhid),summarise,health_perception=min(health_perception),finance_perception=min(finance_perception),life_perception=min(life_perception))
    
  }
  
  kh2012 <- merge(plyr::rename(kh2012,c("hhid"="hhid2012")),mapping_hhids_2010_2012(o2012))
  if (ignore_split_hhids){
    non_split_hhids <- subset(ddply(kh2012,.(hhid2010),summarise,n=length(life_perception)), n==1)$hhid2010
    kh2012 <- subset(kh2012,is.element(hhid2010,non_split_hhids))
  }
  return(kh2012)
  
}

check_shortnames <- function(dat,categs,ignore_list){
  if(length(setdiff( setdiff(unique(dat$shortname),categs), ignore_list)) >0){
    stop(paste("Could not find ",toString(setdiff( setdiff(unique(dat$shortname),categs), ignore_list))))
  }
}

polarisation <- function(means,populations){
  shares<-populations/sum(populations)
  rho<-data.frame(means=means,shares=shares)
  alpha<-1
  return(polarisation.ER(alpha,rho,comp=FALSE)$P)
}

get_split_costs <- function (categs_a , categs_b, dat , group_field) {
  if(is.element("w_a", colnames(dat))  ){
    stop("dat already has a columnn with the name_a")
  }
  if(is.element("w_b", colnames(dat))  ){
    stop("dat already has a columnn with the name_b")
  }
  
  
  if (length(setdiff(unique(dat[,group_field]),unique(union(categs_a,categs_b))))>0){
    stop(paste("Could not find : ",setdiff(unique(dat[,group_field]),unique(union(categs_a,categs_b)))," in the items-code-mapping"))
  }
  
  # using logical variable to indicate which group_field values occur in the respective categories
  dat$w_a <- is.element(dat[,group_field],categs_a)
  dat$w_b <- is.element(dat[,group_field],categs_b)
  
  # selecting and summing data from respective categories
  dat_a <- ddply(subset(plyr::rename(dat[,c("hhid","cost","w_a")],c("cost"="cost_a")),w_a==TRUE), .(hhid),summarise,cost_a=sum(cost_a))
  dat_b <- ddply(subset(plyr::rename(dat[,c("hhid","cost","w_b")],c("cost"="cost_b")),w_b=TRUE), .(hhid),summarise,cost_b=sum(cost_b))
  
  # sum up costs from different categories
  datres <- merge(dat_a,dat_b,by=c("hhid"))
  return(datres)
}

zero_nas <- function(dat){
  print("Zeroing NAs for ne-aggregation")
  if (nrow(dat[is.na(dat$cost_ne_food),])>0)
  {
    dat[is.na(dat$cost_ne_food),]$cost_ne_food <- 0
  }
  if (nrow(dat[is.na(dat$cost_ne_nonfood),])>0)
  {
    dat[is.na(dat$cost_ne_nonfood),]$cost_ne_nonfood <- 0
  }
  if (nrow(dat[is.na(dat$cost_asset_costs),])>0)
  {
    dat[is.na(dat$cost_asset_costs),]$cost_asset_costs <- 0
  }
  if (F){
    if (nrow(dat[is.na(dat$cost_assets),])>0)
    {
      dat[is.na(dat$cost_assets),]$cost_assets <- 0
    }
  }
  return(dat)
}

get_nonparametric_df <- function(ll,ln, food_analysis, use_ea, o2010, o2012, o2014, a2010, a2012, a2014, c2010, c2012, c2014 ){
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
    
    
    food_costs <- subset(all_costs, is.element(group,c("needs")))
    excess_costs <- subset(all_costs, is.element(group,c("excess")))
    ignore_costs <- unique(union(subset(all_costs,is.element(group,c("asset_costs","assets")))$shortname, c("kerosene","electricity","petrol","charcoal")))
    asset_costs <- subset(all_costs, is.element(group,c("asset_costs")))
    assets_diary_costs <- subset(all_costs, is.element(group,c("assets")))
    
    check_shortnames(dat=c2010,categs=union(food_costs$shortname,excess_costs$shortname),ignore_list = ignore_costs)
    check_shortnames(dat=c2012,categs=union(food_costs$shortname,excess_costs$shortname),ignore_list = ignore_costs)
    check_shortnames(dat=c2014,categs=union(food_costs$shortname,excess_costs$shortname),ignore_list = ignore_costs)
    
    food2010 <- plyr::rename(ddply(subset(c2010,is.element(shortname,food_costs$shortname)),.(hhid),summarise,cost_ne_food=sum(cost)),c("hhid"="hhid2010"))
    food2012 <- plyr::rename(ddply(subset(c2012,is.element(shortname,food_costs$shortname)),.(hhid),summarise,cost_ne_food=sum(cost)),c("hhid"="hhid2012"))
    food2014 <- plyr::rename(ddply(subset(c2014,is.element(shortname,food_costs$shortname)),.(hhid),summarise,cost_ne_food=sum(cost)),c("hhid"="hhid2014"))
    
    excess2010 <- plyr::rename(ddply(subset(c2010,is.element(shortname,excess_costs$shortname)),.(hhid),summarise,cost_ne_nonfood=sum(cost)),c("hhid"="hhid2010"))
    excess2012 <- plyr::rename(ddply(subset(c2012,is.element(shortname,excess_costs$shortname)),.(hhid),summarise,cost_ne_nonfood=sum(cost)),c("hhid"="hhid2012"))
    excess2014 <- plyr::rename(ddply(subset(c2014,is.element(shortname,excess_costs$shortname)),.(hhid),summarise,cost_ne_nonfood=sum(cost)),c("hhid"="hhid2014"))
    
    asset_costs2010 <- plyr::rename(ddply(subset(c2010,is.element(shortname,asset_costs$shortname)),.(hhid),summarise,cost_asset_costs=sum(cost)),c("hhid"="hhid2010"))
    asset_costs2012 <- plyr::rename(ddply(subset(c2012,is.element(shortname,asset_costs$shortname)),.(hhid),summarise,cost_asset_costs=sum(cost)),c("hhid"="hhid2012"))
    asset_costs2014 <- plyr::rename(ddply(subset(c2014,is.element(shortname,asset_costs$shortname)),.(hhid),summarise,cost_asset_costs=sum(cost)),c("hhid"="hhid2014"))

    #assets_diary_2010 <- plyr::rename(ddply(subset(c2010,is.element(shortname,assets_diary_costs$shortname)),.(hhid),summarise,cost_assets=sum(cost)),c("hhid"="hhid2010"))
    #assets_diary_2012 <- plyr::rename(ddply(subset(c2012,is.element(shortname,assets_diary_costs$shortname)),.(hhid),summarise,cost_assets=sum(cost)),c("hhid"="hhid2012"))
    #assets_diary_2014 <- plyr::rename(ddply(subset(c2014,is.element(shortname,assets_diary_costs$shortname)),.(hhid),summarise,cost_assets=sum(cost)),c("hhid"="hhid2014"))
    
    ne2010_noac_noa <- merge(food2010,excess2010,by=c("hhid2010"),all=T)
    #ne2010_noac <- merge(ne2010_noac_noa,assets_diary_2010,by=c("hhid2010"),all=T)
    ne2010 <- zero_nas(merge(ne2010_noac_noa,asset_costs2010,by=c("hhid2010"),all=T))
    
    ne2012_noac_noa <- merge(food2012,excess2012,by=c("hhid2012"),all=T)
    #ne2012_noac <- merge(ne2012_noac_noa,assets_diary_2012,by=c("hhid2012"),all=T)
    ne2012 <- zero_nas(merge(ne2012_noac_noa,asset_costs2012,by=c("hhid2012"),all=T))
    
    ne2014_noac_noa <- merge(food2014,excess2014,by=c("hhid2014"),all=T)
    #ne2014_noac <- merge(ne2014_noac_noa,assets_diary_2014,by=c("hhid2014"),all=T)
    ne2014 <- zero_nas(merge(ne2014_noac_noa,asset_costs2014,by=c("hhid2014"),all=T))
    
    
  }
  
  incomedat2010 <- hh_income_data(ll = ll,yr = 2010,dirprefix =dirprefix,fu = fu,ln = ln)
  incomedat2012 <- hh_income_data(ll = ll,yr = 2012,dirprefix =dirprefix,fu = fu,ln = ln)
  incomedat2014 <- hh_income_data(ll = ll,yr = 2014,dirprefix =dirprefix,fu = fu,ln = ln)
  
  
  perception_columns <- c("life_perception"="hh_life_perception" , "finance_perception"="hh_finance_perception", "richness_perception"="hh_richness_perception","housing_perception"="hh_housing_perception","health_perception"="hh_health_perception")
  hhead_columns <- c("hhid"="hhid","years_community"="hh_years_community","age"="hh_age","highest_educ"="hh_highest_educ","occupation_rank"="hh_occupation_rank","litlang"="hh_litlang")

  #total consumption
  relevant_fields <-c("hhid","region","district","ward","ea","isrural","expensiveregion","S","E","population","ypay","lnY")
  # 2010
  ohs2010_wi <- subset(o2010,!is.na(region))
  
  ohs2010 <- merge(ohs2010_wi, incomedat2010,by=c("hhid"),all.x=T)
  
  hsizes2010 <- ddply(ohs2010[,c("hhid","personid","educexpense")],.(hhid),summarise,hsize=length(personid), toteducexpense=sum(educexpense[!is.na(educexpense)]))
  # to use consu: call hsizes2010=ll@get_hsize(ohs2010)
  hs2010 <- unique(merge(unique(ohs2010[,relevant_fields]),hsizes2010 , by = c("hhid")))
  chosenchars2010 <- ddply(ohs2010[,c("hhid","education_rank","occupation_rank","litlang","father_educ_rank","mother_educ_rank","age","outoffood")],.(hhid),summarise,
                           max_education_rank = choose_max_non_na(education_rank) , 
                           max_occupation_rank = choose_max_non_na(occupation_rank) , 
                           father_educ_rank=choose_max_non_na(father_educ_rank),
                           mother_educ_rank=choose_max_non_na(mother_educ_rank),
                           litlang = choose_max_litlang(litlang) , 
                           age = choose_max_non_na(age), outoffood=max(outoffood))
  #  perception_columns
  hhead2010 <- plyr::rename(subset(o2010,household_status==1)[,names(hhead_columns)],hhead_columns )
  chosencharshead2010 <- merge(chosenchars2010,hhead2010, all.x=T)
  hswithchars2010 <- merge(hs2010,chosencharshead2010,all.x = T)
  # -6.727135 39.14395
  
  
  # 2012
  ohs2012_wi <- subset(o2012,!is.na(region))
  ohs2012 <- merge(ohs2012_wi, incomedat2012,by=c("hhid"),all.x=T)
  hsizes2012 <- ddply(ohs2012[,c("hhid","personid","educexpense")],.(hhid),summarise,hsize=length(personid), toteducexpense=sum(educexpense[!is.na(educexpense)]))
  hs2012 <- unique(merge(unique(ohs2012[,relevant_fields]), hsizes2012, by = c("hhid")))
  chosenchars2012 <- ddply(ohs2012[,c("hhid","education_rank","occupation_rank","father_educ_rank","mother_educ_rank","age","litlang","outoffood")],.(hhid),summarise,
                           max_education_rank = choose_max_non_na(education_rank) , 
                           max_occupation_rank = choose_max_non_na(occupation_rank) , 
                           father_educ_rank=choose_max_non_na(father_educ_rank),
                           mother_educ_rank=choose_max_non_na(mother_educ_rank),
                           litlang = choose_max_litlang(litlang) , 
                           age = choose_max_non_na(age), outoffood=max(outoffood) )
  
  if (length(setdiff(names(perception_columns),colnames(o2012)))==0){
    hhead_columns_w_percept <- c(hhead_columns,perception_columns)
  }
  
  hhead2012 <- plyr::rename(subset(o2012,household_status==1)[,names(hhead_columns_w_percept)],hhead_columns_w_percept )
  
  chosencharshead2012 <- merge(chosenchars2012,hhead2012, all.x=T)
  hswithchars2012 <- merge(hs2012,chosencharshead2012,all.x = T)
  
  #2014
  ohs2014_wi <- subset(o2014,!is.na(region))
  ohs2014 <- merge(ohs2014_wi, incomedat2014,by=c("hhid"),all.x=T)
  hsizes2014 <- ddply(ohs2014[,c("hhid","personid","educexpense")],.(hhid),summarise,hsize=length(personid), toteducexpense=sum(educexpense[!is.na(educexpense)]))
  hs2014 <- unique(merge(unique(ohs2014[,relevant_fields]), hsizes2014, by = c("hhid")))
  chosenchars2014 <- ddply(ohs2014[,c("hhid","education_rank","occupation_rank","father_educ_rank","mother_educ_rank","age","litlang","outoffood")],.(hhid),summarise,
                           max_education_rank = choose_max_non_na(education_rank) , 
                           max_occupation_rank = choose_max_non_na(occupation_rank) ,
                           father_educ_rank=choose_max_non_na(father_educ_rank),
                           mother_educ_rank=choose_max_non_na(mother_educ_rank),
                           litlang = choose_max_litlang(litlang), 
                           age = choose_max_non_na(age), 
                           outoffood=max(outoffood))
  
  hhead2014 <- plyr::rename(subset(o2014,household_status==1)[,names(hhead_columns)],hhead_columns )
  
  chosencharshead2014 <- merge(chosenchars2014,hhead2014, all.x=T)
  
  hswithchars2014 <- merge(hs2014,chosencharshead2014,all.x = T)
  
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
    
    
    if(use_ea){
      
      dflist <- list()
      dflist[["indivdat2010"]] <- indivdat2010
      dflist[["indivdat2012"]] <- indivdat2012
      dflist[["indivdat2014"]] <- indivdat2014
      
      for (year in c(2010,2012,2014)){
        dfdat <- dflist[[paste0("indivdat",year)]]
        
        dfdat$hhid <- dfdat[,paste0("hhid",year)]
        datfields_wo_outoffood <- ddply(dfdat,.(region,district,ward,ea),summarise,mean_cost_ne_food_x=mean(cost_ne_food/hsize), q30_cost_ne_food_x = quantile(cost_ne_food/hsize,.3), q70_cost_ne_food_x = quantile(cost_ne_food/hsize,.7) ,  mean_cost_ne_nonfood_x=mean(cost_ne_nonfood/hsize),q30_cost_ne_nonfood_x=quantile(cost_ne_nonfood/hsize,.3) ,q70_cost_ne_nonfood_x=quantile(cost_ne_nonfood/hsize,.7) ,  mean_A0=mean(A0), n_ea= length(unique(hhid))) 
        
        out_of_food_at_district_level <- get_outoffood_at_district_level(dfdata=dfdat)
        
          
        dfdat$hhid <- NULL
        print("Using merge based on closest district with consumer having run out of food")
        datfields <- merge(datfields_wo_outoffood,out_of_food_at_district_level,by=c("region","district"), all.x=T) %>% mutate (r = log(mean_A0))
        
        if (nrow(subset(datfields,is.na(min_ne_food_x )))>0){
          stop("Missing outoffood data for some districts or wards")
        }
        
        rd <- merge(datfields, dfdat, by=c("region","district","ward","ea"))
        rd <- rd %>% mutate (Ar=lnA0-r)
        
        print(paste("Number of households ignored because of missing r:",length(unique(subset(rd,is.na(r))[,paste0("hhid",year)])),"/",length(unique(rd[,paste0("hhid",year)]))))
        
        rd <-subset(rd,!is.na(r))
        
        print(paste("Number of households ignored because of less than 2 households in the ea:",length(unique(subset(rd,n_ea<2)[,paste0("hhid",year)])),"/",
                    length(unique(rd[,paste0("hhid",year)]))  ) )
                    
        rd <- subset(rd,n_ea>=2)
        
        res[[paste0("df",year)]] <- rd
      }
      
      
      
    } else{ # not using ea
      
      indivdat2010$ea <- NULL
      indivdat2012$ea <- NULL
      indivdat2014$ea <- NULL
      
      indivdat2010$P1 <- paste(indivdat2010$region,indivdat2010$district,sep="-")
      indivdat2012$P1 <- paste(indivdat2012$region,indivdat2012$district,sep="-")
      indivdat2014$P1 <- paste(indivdat2014$region,indivdat2014$district,sep="-")
      
      # in the desired data-frame we would have hhdis with their region-id in P2 (which also included P1). So that pi(r) is the same for all consumers in the P2. 
      # the output would be the pi(r) for all hhid 
      dflist <- list()
      dflist[["indivdat2010"]] <- indivdat2010
      dflist[["indivdat2012"]] <- indivdat2012
      dflist[["indivdat2014"]] <- indivdat2014
      res=list()
      for (year in c(2010,2012,2014)){
        dfdat <- dflist[[paste0("indivdat",year)]]
        dfdat <- dfdat %>% mutate( high_educ = as.integer(max_education_rank>educ_pivot) , high_occup = as.integer(max_occupation_rank>occup_pivot))
        
        dfdat$hhid <- dfdat[,paste0("hhid",year)]
        out_of_food_at_district_level <- get_outoffood_at_district_level(dfdata=dfdat)
        
        print("Using merge based on closest district with consumer having run out of food")
        dfdat <- merge(dfdat,out_of_food_at_district_level,by=c("region","district"), all.x=T)
        
        if (nrow(subset(dfdat,is.na(min_ne_food_x )))>0){
          stop("Missing outoffood data for some districts or wards")
        }
        
        dfdat$hhid <- NULL
        
        bubble_distances <- get_bubble_distances(dat=dfdat, distance_threshold = .06)
        dat_over_bubbles <- get_bubble_aggregated_df(input_dat = dfdat,bubble_distances = bubble_distances)
        
        bubble_fields <- ddply(dat_over_bubbles,.(B),summarise,mean_cost_ne_food_x=mean(cost_ne_food/hsize), q30_cost_ne_food_x = quantile(cost_ne_food/hsize,.3), q70_cost_ne_food_x = quantile(cost_ne_food/hsize,.7) ,  mean_cost_ne_nonfood_x=mean(cost_ne_nonfood/hsize),q30_cost_ne_nonfood_x=quantile(cost_ne_nonfood/hsize,.3) ,q70_cost_ne_nonfood_x=quantile(cost_ne_nonfood/hsize,.7) ,  mean_A0=mean(A0)) 
        #bubble_occup <- ddply(dat_over_bubbles,.(B,high_occup),summarise,mean_occup_cost_ne_food_x=mean(cost_ne_food/hsize), mean_occup_cost_ne_nonfood_x = mean(cost_ne_nonfood/hsize), mean_occup_A0=mean(A0))
        #bubble_educ <- ddply(dat_over_bubbles,.(B,high_educ),summarise,mean_educ_cost_ne_food_x=mean(cost_ne_food/hsize),mean_educ_cost_ne_nonfood_x=mean(cost_ne_nonfood/hsize),mean_educ_A0=mean(A0))
        
        bubble_fields_w_P1 <- merge(bubble_distances,bubble_fields,by=c('B'))
        
        rd_bubble <- merge(bubble_fields_w_P1, dfdat, by="P1")
        #rd_bubble_weduc <- merge(rd_bubble,bubble_educ, by = c("B","high_educ"))
        #rd_bubble_weducoccup <- merge(rd_bubble_weduc,bubble_occup, by = c("B","high_occup"))
        
        
        rd <- rd_bubble %>% mutate(x_ne_food = cost_ne_food/hsize) %>% mutate(x_ne_nonfood = cost_ne_nonfood/hsize) %>% mutate(logx_ne_food=log(x_ne_food+1e-7),logx_ne_nonfood=log(x_ne_nonfood+1e-7)) %>% mutate(   x_ac = cost_asset_costs/hsize)
        rd <- rd %>% mutate (r = log(mean_A0)) %>% mutate (Ar=lnA0-r)
        #rd <- rd %>% mutate (r_occup = log(mean_occup_A0)) %>%  mutate (Ar_occup=lnA0-r_occup)
        #rd <- rd %>% mutate (r_educ = log(mean_educ_A0)) %>% mutate (Ar_educ=lnA0-r_educ)
        rd <-subset(rd,!is.na(r))
        print(paste("Number of households ignored because of missing r:",length(unique(subset(rd,is.na(r))[,paste0("hhid",year)]))))
        res[[paste0("df",year)]] <- rd
      }
      
      #test
      #print(summary(lm(data=dat2010, nu~ r + max_occupation_rank + max_education_rank)))
      
    }# endif use_ea
    
  } # endif food_analysis
  
  
  return(res)
}

get_outoffood_at_district_level <- function(dfdata) {
  out_of_foodhhs <- subset(dfdata[,c("hhid","region","district","ward","outoffood","cost_ne_nonfood","cost_ne_food","hsize")],outoffood==1)

  districts_data <- subset(ddply(out_of_foodhhs,.(region,district),summarise,n_outoffood=length(hhid), min_ne_food_x=mean(cost_ne_food/hsize),min_ne_nonfood=mean(cost_ne_nonfood)), n_outoffood>=1)
  districts_data$loc <- mapply(function(r,d) { jsonlite::toJSON( c(r,d) ) } ,districts_data$region, districts_data$district )
  
  #ensure that S and E are not more refined than at district level
  district_coordinates <- ddply(unique(dfdata[,c("region","district","S","E")]),.(region,district),summarise,S=mean(S),E=mean(E))
  district_coordinates$loc <- mapply(function(r,d) { jsonlite::toJSON( c(r,d) ) } ,district_coordinates$region, district_coordinates$district )
  
  # merge on the basis of closest district in the mapping m
  out_of_food_at_district_level <- closest_loc_data (a=district_coordinates,m=districts_data,data_field="min_ne_food_x")
  
  out_of_food_at_district_level$region <- with ( out_of_food_at_district_level, sapply(as.character(loc),function(x){jsonlite::fromJSON(x)[1]}))
  out_of_food_at_district_level$district <- with (out_of_food_at_district_level, sapply(as.character(loc),function(x){jsonlite::fromJSON(x)[2]}))
  
  return(out_of_food_at_district_level)
}


plot_pi_r_against_r <- function(dflist){
  #years <- c("2010","2012")
  years <- c("2012")
  par(mfrow = c(1,length(years)))
  for (year in years ){
    print(year)
    data = dflist[[paste0("df",year)]]
    data <- data %>% mutate( lc = log(mean_cost_ne_x))
    plot(data=data,lc ~ r , xlab =latex2exp::TeX("$r$") , ylab=latex2exp::TeX("$log(\\pi(r))$") , main = year)
    abline(lm(data=data, lc ~ r))
  }
  par(mfrow = c(1,1))
  
  #plot(data=nfa[["df2010"]] %>% mutate(dA = lnA0- r, lnu = log(nu)), lnu ~ dA, xlim = c (-10,5))
  #abline(lm(data=nfa[["df2012"]] %>% mutate(dA = lnA0- r, lnu = log(nu)), lnu ~ dA))
  
  ## also check : hist((nfa[["df2012"]] %>% mutate(dA = lnA0- r, lnu = log(nu)))$lnu)
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

save_data <- function(dfslist,use_ea)
{
  if (use_ea){
    write_dta(dfslist[['df2010']],'../lsms/data/tn_df_ea2010.dta')
    write_dta(dfslist[['df2012']],'../lsms/data/tn_df_ea2012.dta')
    write_dta(dfslist[['df2014']],'../lsms/data/tn_df_ea2014.dta')  
  } else {
    write_dta(dfslist[['df2010']],'../lsms/data/tn_df2010.dta')
    write_dta(dfslist[['df2012']],'../lsms/data/tn_df2012.dta')
    write_dta(dfslist[['df2014']],'../lsms/data/tn_df2014.dta')
  }
}

load_data <- function(use_ea)
{
  if (use_ea){
    tndf2010 <- read_dta('../lsms/data/tn_df_ea2010.dta')
    tndf2012 <- read_dta('../lsms/data/tn_df_ea2012.dta')
    tndf2014 <- read_dta('../lsms/data/tn_df_ea2014.dta')
    return(add_fields_to_data(tndf2010=tndf2010,tndf2012=tndf2012,tndf2014=tndf2014,use_ea=T))
  } else{
    tndf2010 <- read_dta('../lsms/data/tn_df2010.dta')
    tndf2012 <- read_dta('../lsms/data/tn_df2012.dta')
    tndf2014 <- read_dta('../lsms/data/tn_df2014.dta')
    return(add_fields_to_data(tndf2010=tndf2010,tndf2012=tndf2012,tndf2014=tndf2014,use_ea=F))
  }
}

log_zeroed <- function(x){
  if (x<0){
    return(0)
  } else {
    return(log(1e-7+x))
  }
}
add_fields_to_data<-function(use_ea,tndf2010,tndf2012,tndf2014){
  res = list()
  
  
  res[['df2010']] <- tndf2010 %>% mutate ( has_nu = as.integer(cost_ne_food+cost_ne_nonfood> min_ne_food_x*hsize), log_q_ne = sapply(cost_ne_nonfood + cost_ne_food - min_ne_food_x*hsize,log_zeroed) , logx =log(cost_ne_food + cost_asset_costs  +cost_ne_nonfood) , mean_cost_ne = log(mean_cost_ne_food_x + mean_cost_ne_nonfood_x) , log_mean_A0 = log(mean_A0) , log_mean_cost_ne = log(mean_cost_ne+1e-7))
  res[['df2010']] <- res[['df2010']] %>% mutate ( log_q_ne_nonfood = log(1e-7 + cost_ne_nonfood), log_q_ne_food = log(1e-7 + cost_ne_food), log_mean_cost_ne_food = log(mean_cost_ne_food_x+1e-7), log_mean_cost_ne_nonfood = log(mean_cost_ne_nonfood_x+1e-7), w_food_ne = cost_ne_food/(cost_ne_food+cost_ne_nonfood) , w_nonfood_ne = cost_ne_nonfood/(cost_ne_food+cost_ne_nonfood))
  
  res[['df2010']] <- res[['df2010']] %>% mutate (w_ne = (cost_ne_food+cost_ne_nonfood)/(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)), w_A = (exp(lnA0))/(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)), w_educ = (toteducexpense)/(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)), lnX = log(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)))
  
  # adding quantiles
  res[['df2010']] <- res[['df2010']] %>% mutate ( log_q30_cost_ne_food = log(q30_cost_ne_food_x+1e-7), log_q30_cost_ne_nonfood = log(q30_cost_ne_nonfood_x+1e-7) , log_q70_cost_ne_food = log(q70_cost_ne_food_x+1e-7), log_q70_cost_ne_nonfood = log(q70_cost_ne_nonfood_x+1e-7) )
  
  res[['df2012']] <- tndf2012 %>% mutate ( has_nu = as.integer(cost_ne_food+cost_ne_nonfood> min_ne_food_x*hsize), log_q_ne = sapply(cost_ne_nonfood + cost_ne_food - min_ne_food_x*hsize,log_zeroed) , logx =log(cost_ne_food + cost_asset_costs  +cost_ne_nonfood) , mean_cost_ne = log(mean_cost_ne_food_x + mean_cost_ne_nonfood_x) , log_mean_A0 = log(mean_A0) , log_mean_cost_ne = log(mean_cost_ne+1e-7))
  res[['df2012']] <- res[['df2012']] %>% mutate ( log_q_ne_nonfood = log(1e-7 + cost_ne_nonfood), log_q_ne_food = log(1e-7 + cost_ne_food), log_mean_cost_ne_food = log(mean_cost_ne_food_x+1e-7), log_mean_cost_ne_nonfood = log(mean_cost_ne_nonfood_x+1e-7), w_food_ne = cost_ne_food/(cost_ne_food+cost_ne_nonfood) , w_nonfood_ne = cost_ne_nonfood/(cost_ne_food+cost_ne_nonfood))
  
  res[['df2012']] <- res[['df2012']] %>% mutate (w_ne = (cost_ne_food+cost_ne_nonfood)/(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)), w_A = (exp(lnA0))/(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)), w_educ = (toteducexpense)/(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)), lnX = log(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)))
  # adding quantiles
  res[['df2012']] <- res[['df2012']] %>% mutate ( log_q30_cost_ne_food = log(q30_cost_ne_food_x+1e-7), log_q30_cost_ne_nonfood = log(q30_cost_ne_nonfood_x+1e-7) , log_q70_cost_ne_food = log(q70_cost_ne_food_x+1e-7), log_q70_cost_ne_nonfood = log(q70_cost_ne_nonfood_x+1e-7) )
  
  res[['df2014']] <- tndf2014 %>% mutate ( has_nu = as.integer(cost_ne_food+cost_ne_nonfood> min_ne_food_x*hsize), log_q_ne = sapply(cost_ne_nonfood + cost_ne_food - min_ne_food_x*hsize,log_zeroed) , logx =log(cost_ne_food + cost_asset_costs  +cost_ne_nonfood) , mean_cost_ne = log(mean_cost_ne_food_x + mean_cost_ne_nonfood_x) , log_mean_A0 = log(mean_A0) , log_mean_cost_ne = log(mean_cost_ne+1e-7))
  res[['df2014']] <- res[['df2014']] %>% mutate ( log_q_ne_nonfood = log(1e-7 + cost_ne_nonfood), log_q_ne_food = log(1e-7 + cost_ne_food), log_mean_cost_ne_food = log(mean_cost_ne_food_x+1e-7), log_mean_cost_ne_nonfood = log(mean_cost_ne_nonfood_x+1e-7), w_food_ne = cost_ne_food/(cost_ne_food+cost_ne_nonfood) , w_nonfood_ne = cost_ne_nonfood/(cost_ne_food+cost_ne_nonfood))
  res[['df2014']] <- res[['df2014']] %>% mutate (w_ne = (cost_ne_food+cost_ne_nonfood)/(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)), w_A = (exp(lnA0))/(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)), w_educ = (toteducexpense)/(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)), lnX = log(cost_ne_food+cost_ne_nonfood + toteducexpense + exp(lnA0)))
  # adding quantiles
  res[['df2014']] <- res[['df2014']] %>% mutate ( log_q30_cost_ne_food = log(q30_cost_ne_food_x+1e-7), log_q30_cost_ne_nonfood = log(q30_cost_ne_nonfood_x+1e-7) , log_q70_cost_ne_food = log(q70_cost_ne_food_x+1e-7), log_q70_cost_ne_nonfood = log(q70_cost_ne_nonfood_x+1e-7) )
  
  res[['df2010']]$rural_wards <- NULL
  res[['df2012']]$rural_wards <- NULL
  res[['df2014']]$rural_wards <- NULL
  
  if (use_ea){
    #adding polarisation
    res[["df2010"]]$ER <- NULL
    pol2010<- ddply(unique(res[["df2010"]][,c("hhid2010","region","district","ward","ea","lnA0")]),.(region,district,ward,ea),summarise,ER=polarisation(lnA0,rep(1,length(hhid2010))))
    res[["df2010"]] <- merge(res[["df2010"]],pol2010,by=c("region","district","ward","ea"),all.x=T)
    
    res[["df2012"]]$ER <- NULL
    pol2012<- ddply(unique(res[["df2012"]][,c("hhid2012","region","district","ward","ea","lnA0")]),.(region,district,ward,ea),summarise,ER=polarisation(lnA0,rep(1,length(hhid2012))))
    res[["df2012"]] <- merge(res[["df2012"]],pol2012,by=c("region","district","ward","ea"),all.x=T)
    
    res[["df2014"]]$ER <- NULL
    pol2014<- ddply(unique(res[["df2014"]][,c("hhid2014","region","district","ward","ea","lnA0")]),.(region,district,ward,ea),summarise,ER=polarisation(lnA0,rep(1,length(hhid2014))))
    res[["df2014"]] <- merge(res[["df2014"]],pol2014,by=c("region","district","ward","ea"),all.x=T)

    return(res)
  } else{
    
    
    res[['df2010']] <- add_rural_mapping_for_districts(res,2010)
    res[['df2012']] <- add_rural_mapping_for_districts(res,2012)
    res[['df2014']] <- add_rural_mapping_for_districts(res,2014)
    
    
    res[["df2010"]]$ER <- NULL
    pol2010<- ddply(unique(res[["df2010"]][,c("hhid2010","region","district","lnA0")]),.(region,district),summarise,ER=polarisation(lnA0,rep(1,length(hhid2010))))
    res[["df2010"]]<- merge(res[["df2010"]],pol2010,by=c("region","district"),all.x=T)
    
    res[["df2012"]]$ER <- NULL
    pol2012<- ddply(unique(res[["df2012"]][,c("hhid2012","region","district","lnA0")]),.(region,district),summarise,ER=polarisation(lnA0,rep(1,length(hhid2012))))
    res[["df2012"]]<- merge(res[["df2012"]],pol2012,by=c("region","district"),all.x=T)
    
    res[["df2014"]]$ER <- NULL
    pol2014<- ddply(unique(res[["df2014"]][,c("hhid2014","region","district","lnA0")]),.(region,district),summarise,ER=polarisation(lnA0,rep(1,length(hhid2014))))
    res[["df2014"]]<- merge(res[["df2014"]],pol2014,by=c("region","district"),all.x=T)
    
    return(res)
  }
  
}

calculate_mean_over_bubbles <- function(input_dat,bubble_distances, field){
  res<- array()
  populations <- array()
  pb <- txtProgressBar(min = 0, max = dim(bubble_distances)[1], style = 3)
  for (i in seq(dim(bubble_distances)[1])){
    tempdat <- subset(input_dat %>% mutate(found=sapply(input_dat$P1,function(x){ length(grep(x,bubble_distances[i,]$B))>0})) , found==T)
    res[i]=mean(tempdat[,field])
    #mean can be calculated over 
    #tempdat %>% mutate(high_occup = as.integer(max_occupation_rank>1))
    #tempdat %>% mutate(high_educ = as.integer(max_education_rank>1))
    populations[i] = nrow(tempdat)
    setTxtProgressBar(pb, i)
  }
  resdf <- data.frame(m=res,N=populations)
  
  colnames(resdf)<- c(paste("mean",field,sep="_"), "N")
  resdf$P1 <- bubble_distances$P1
  return(resdf)
}

get_missing_isrural_mapping_for_2014 <- function(tn){
  
  
  a<-unique(tn$df2012[,c("region","district","B","S","E")])
  b<-unique(tn$df2014[,c("region","district","B","S","E")])
  a$B2012 <- a$B
  b$B2014 <- b$B
  a$B <- NULL
  b$B <- NULL
  k <- expand.grid(a$B2012,b$B2014)
  colnames(k) <- c("B2012","B2014")
  k <- plyr::rename(merge(k,a,all=T,by=c("B2012")),c("S"="S2012","E"="E2012"))
  k <- plyr::rename(merge(k,b,all=T,by=c("B2014")),c("S"="S2014","E"="E2014"))
  k$distance <- mapply(function(s1,e1,s2,e2) { sqrt((s1-s2)**2 + (e1-e2)**2) } , k$S2012,k$E2012,k$S2014,k$E2014)
  
  # start with a small distance and eliminate B2014 values
  
  mapping <- subset(k,distance==0)[,c("B2012","B2014")]
  #res = list()
  #res[["remaining"]] = subset(k,is.element(B2014,setdiff(b$B2014,mapping$B2014)) & !is.element(B2014,mapping$B2014))
  
  #for (distance in c(.3,.1,.05,.01,.005,.0005,.0001,.00005,.00001)){
  #  res = get_mapping_to_add( remaining_k=res[["remaining"]], distance_threshold=distance)
  #  mapping <- rbind(mapping,res[["mapping_to_add"]])
  #  
  #}
  
  missing_mapping_items <- unique(as.character(subset(k,is.element(B2014,setdiff(b$B2014,mapping$B2014)) & !is.element(B2014,mapping$B2014))$B2014))
  for (missing_item in missing_mapping_items){
    m<- subset(k,B2014==missing_item)
    mapping <- rbind(mapping,m[order(m$distance),][1,][,c("B2012","B2014")])
  }
  missing_mapping_items <- unique(as.character(subset(k,is.element(B2014,setdiff(b$B2014,mapping$B2014)) & !is.element(B2014,mapping$B2014))$B2014))
  
  if (length(missing_mapping_items)>0){
    stop("Could not map missing elements")
  }
  
  return(unique(mapping))
}

add_rural_mapping_for_districts <- function(tn,year)
{
  if (year == 2010){
    rural_wards_df = ddply(unique(tn[["df2010"]][c("B","region","district","ward","isrural")]),.(B),summarise,rural_wards=sum(isrural)/length(isrural))
    result = merge(tn$df2010 , rural_wards_df,by=c("B"),all.x=T)
    if (nrow(subset(result,is.na(rural_wards))) >0){
      stop(paste("Missing rural_wards data for year:",year))
    }
    return(result)
  } 
  if (year == 2012){
    rural_wards_df = ddply(unique(tn[["df2012"]][c("B","region","district","ward","isrural")]),.(B),summarise,rural_wards=sum(isrural)/length(isrural))
    result = merge(tn$df2012 , rural_wards_df,by=c("B"),all.x=T)
    if (nrow(subset(result,is.na(rural_wards))) >0){
      stop(paste("Missing rural_wards data for year:",year))
    }
    return(result)
  }
  
  if (year == 2014){
    rural_wards_df_2012 = plyr::rename(ddply(unique(tn[["df2012"]][c("B","region","district","ward","isrural")]),.(B),summarise,rural_wards=sum(isrural)/length(isrural)), c("B"="B2012"))
    B2012_2014_mapping <- get_missing_isrural_mapping_for_2014(tn)
    B2012_2014_rural_wards <- merge(rural_wards_df_2012,B2012_2014_mapping,by=c("B2012"))
    
    result = plyr::rename(merge( plyr::rename(tn[["df2014"]],c("B"="B2014")), B2012_2014_rural_wards,by=c("B2014"),all.x=T),c("B2014"="B"))
    result$B2012 <- NULL
    if (nrow(subset(result,is.na(rural_wards))) >0){
      stop(paste("Missing rural_wards data for year:",year))
    }
    return(result)
  }
  stop(paste("Unknown year:",year))
}

get_mapping_to_add <- function(remaining_k,distance_threshold){
  curk<- subset(remaining_k,distance<distance_threshold)
  exact_matching_B2014 = subset( ddply(curk[,c("B2014","B2012")],.(B2014),summarise,n=length(B2012)), n==1)$B2014
  res = list()
  res[["remaining"]] = subset(remaining_k,!is.element(B2014,exact_matching_B2014))
  res[["mapping_to_add"]] = unique(subset(curk, is.element(B2014,exact_matching_B2014))[,c("B2012","B2014")])
  return(res)
}


get_bubble_distances <- function(dat,distance_threshold,popdistance_threshold){
  # the average of consumption of consumers within a given population-distance becomes pi(r), the total asset value becomes r, the total expenditure is cost_ne
  # remember we have distances only of consumers 
  
  loc_cols <- c("region","district","S","E","population")
  
  all_points <- unique(dat[,loc_cols])
  
  all_points$point <- paste(all_points$region,all_points$district,sep="-")
  all_distances <- expand.grid(all_points$point,all_points$point)
  colnames(all_distances) <- c("P1","P2")
  all_distances <- plyr::rename(merge(plyr::rename(all_points,c("point"="P1")),all_distances,by=c("P1")) ,c("S"="S1","E"="E1","region"="region1","district"="district1","population"="population1") )
  all_distances <- plyr::rename(merge(plyr::rename(all_points,c("point"="P2")),all_distances,by=c("P2")) ,c("S"="S2","E"="E2","region"="region2","district"="district2","population"="population2") )
  
  # The distances between two points that are populous would be lower than two points that are less populous
  # The distances are still symmetric - because even if one is significantly more populous than the other - they're closer than they would be when they're not populous.
  all_distances$distance <- mapply(function(s1,e1,s2,e2) { sqrt((s1-s2)**2 + (e1-e2)**2) } , all_distances$S1,all_distances$E1,all_distances$S2,all_distances$E2)
  all_distances$pop_distance <- mapply(function(s1,e1,s2,e2,N1,N2) {(1e+6/(N1+N2))* sqrt((s1-s2)**2 + (e1-e2)**2) } , all_distances$S1,all_distances$E1,all_distances$S2,all_distances$E2,all_distances$population1,all_distances$population2)
  
  if (missing(popdistance_threshold) ){
    if (missing(distance_threshold)){
      stop("Must provide either distance_threshold or popdistance_threshold")  
    } else{
      filtered_distances <- subset(all_distances,distance<distance_threshold)
    }
  } else if (missing(distance_threshold)) {
    filtered_distances <- subset(all_distances,pop_distance<popdistance_threshold)
  } else{
    stop("Cannot use both distance_threshold and popdistance_threshold")
  }
  
  bubble_distances <- ddply(unique(filtered_distances[,c("P1","P2")]),.(P1),summarise,B=toJSON(P2))
  return(bubble_distances)
}


#x <- build_xt_df(dat2010 = nf[["df2010"]], dat2012= nf[["df2012"]],dat2014 = nf[["df2014"]])
build_xt_df <- function(dflist)
{
  dat2010 <- dflist[["df2010"]]
  dat2012 <- dflist[["df2012"]]
  dat2014 <- dflist[["df2014"]]
  
  if (is.element("hhid",colnames(dat2010))  && !is.element("hhid2010",colnames(dat2010))){
    dat2010$hhid2010 <- dat2010$hhid
  } else if (!is.element("hhid",colnames(dat2010))  && is.element("hhid2010",colnames(dat2010))){
    dat2010$hhid <- dat2010$hhid2010
  }
  
  if (is.element("hhid",colnames(dat2012))  && !is.element("hhid2012",colnames(dat2012))){
    dat2012$hhid2012 <- dat2012$hhid
  } else if (!is.element("hhid",colnames(dat2012))  && is.element("hhid2012",colnames(dat2012))){
    dat2012$hhid <- dat2012$hhid2012
  }

  if (is.element("hhid",colnames(dat2014))  && !is.element("hhid2014",colnames(dat2014))){
    dat2014$hhid2014 <- dat2014$hhid
  } else if (!is.element("hhid",colnames(dat2014))  && is.element("hhid2014",colnames(dat2014))){
    dat2014$hhid <- dat2014$hhid2014
  }

    
  hhid2012_hhid10_mapping <- mapping_hhids_2010_2012(o2012 = o2012)
  hhid2014_hhid12_mapping <- mapping_hhids_2012_2014(o2014 = o2014)
  common_cols_2010_2012_2014 <- intersect(colnames(dat2014),intersect(colnames(dat2010),colnames(dat2012) ))
  df2010_2012 <- merge(hhid2012_hhid10_mapping , plyr::rename(dat2012[,common_cols_2010_2012_2014],c("hhid"="hhid2012"))) %>% mutate(year = 2012)
  df2010_2012$hhid2012 <- NULL
  df2010_2012 <- plyr::rename(df2010_2012,c("hhid2010"="hhid"))
  df2010_2012 <- rbind(df2010_2012,dat2010[,common_cols_2010_2012_2014] %>% mutate(year =2010))
  df2010_2012$hhid <- as.factor(df2010_2012$hhid)
  split_hhids2010_2012 <- unique(subset(ddply(df2010_2012,.(hhid,year),summarise,n=length(hsize)),n>1)$hhid)
  
  print(paste("Ignoring split",length(split_hhids2010_2012),"/",length(unique(df2010_2012$hhid)),"households"))
  df2010_2012 <- subset(df2010_2012,!is.element(hhid,split_hhids2010_2012))
  
  ####
  df2012_2014 <- merge(hhid2014_hhid12_mapping , plyr::rename(dat2014[,common_cols_2010_2012_2014],c("hhid"="hhid2014"))) %>% mutate(year = 2014)
  df2012_2014$hhid2014 <- NULL
  df2012_2014 <- plyr::rename(df2012_2014,c("hhid2012"="hhid"))
  df2012_2014 <- rbind(df2012_2014,dat2012[,common_cols_2010_2012_2014] %>% mutate(year =2012))
  df2012_2014$hhid <- as.factor(df2012_2014$hhid)
  split_hhids2012_2014 <- unique(subset(ddply(df2012_2014,.(hhid,year),summarise,n=length(hsize)),n>1)$hhid)
  warning("The year 3 hhids are missing in a lot of records for 2014 but a significant number of households in the extension file do have this field") 
  print(paste("Ignoring split",length(split_hhids2012_2014),"/",length(unique(df2012_2014$hhid)),"households"))
  df2012_2014 <- subset(df2012_2014,!is.element(hhid,split_hhids2012_2014))
  
  res=list()
  res[["df2010_2012"]] <- df2010_2012
  res[["df2012_2014"]] <- df2012_2014
  return(res)
}

choose_min_distance_with_data <- function(distances,datvec){
  ret = data.frame(distance=distances,data=datvec)
  result = subset(ret,!is.na(data)) %>% filter(distance==min(distance))
  return (result[1,]$data)
}

closest_loc_data <-function(a,m,data_field,use_test_data){
  m$data <- m[,data_field]
  m <- m[,c("loc","data")]
  
  if(missing(use_test_data)){
    use_test_data <- F
  }
  if (use_test_data){
    a <- data.frame(loc=c("A","B","C"),S=c(1,2,3),E=c(3,4,5))
    m <- data.frame(loc=c("B","C"),data=c("X","Y"))
  }
  
  k <- expand.grid(a$loc,a$loc)
  colnames(k) <- c("src","tgt")
  b <- plyr::rename(merge(k,plyr::rename(a,c("loc"="src")),by=c("src")), c("S"="src.S","E"="src.E") )
  b <- plyr::rename(merge(b,plyr::rename(a,c("loc"="tgt")),by=c("tgt")), c("S"="tgt.S","E"="tgt.E") )
  
  b$distance <- mapply(function(s1,e1,s2,e2) { sqrt((s1-s2)**2 + (e1-e2)**2) } , b$src.S,b$src.E,b$tgt.S,b$tgt.E)
  b <- merge(b,plyr::rename(m,c("loc"="tgt")),by=c("tgt"),all.x=T)
  b <- b[order(b$src),]
  result <- ddply(b[,c("src","distance","data")],.(src),summarise, data=choose_min_distance_with_data(distance,data))
  result <- plyr::rename(result,c("data"=data_field,"src"="loc"))
  return(result)
}

test_search_cluster <- function(){
  tdf <- data.frame(P1=c("1-1","2-4"),B=c("2-2, 1-1","2-4, 1-2"))
  dat <- data.frame(hhid=seq(4),P1=c("2-4","1-1","2-2","1-2"))
  resdf<-NULL
  for (i in seq(nrow(tdf))){
  resdf <- rbind(resdf,subset(dat %>% mutate(found=sapply(dat$P1,function(x){ length(grep(x,tdf[i,]))>0})),found==T) %>% mutate(B=tdf[i,]$B , found=NULL))
  }
  return(resdf)
}

run_non_parametric_regression_for_food_vs_nonfood <- function(ll,dfslist,year,sp)
{
  select_df=paste0("df",year)
  
  S <- with(dfslist[[select_df]], seq(min(S), max(S), len=25))
  E <- with(dfslist[[select_df]], seq(min(E), max(E), len=25))
  newdata <- expand.grid(S=S, E=E)
  #mod.lo_cpaa <- loess(cpA_a ~ S + E , span=sp, degree=1, data=dfslist[[select_df]])
  #mod.lo_cpab <- loess(cpA_b ~ S + E , span=sp, degree=1, data=dfslist[[select_df]])
  
  mod.lo_ca <- loess(cost_a ~ S + E , span=sp, degree=1, data=dfslist[[select_df]])
  mod.lo_cb <- loess(cost_b ~ S + E , span=sp, degree=1, data=dfslist[[select_df]])
  
  #fit.cpaa <- matrix(predict(mod.lo_cpaa, newdata), 25, 25)
  #fit.cpab <- matrix(predict(mod.lo_cpab, newdata), 25, 25)
  
  fit.ca <- matrix(predict(mod.lo_ca, newdata), 25, 25)
  fit.cb <- matrix(predict(mod.lo_cb, newdata), 25, 25)
  
  par(mfrow=c(1,2))
  
  #persp(S, E, fit.cpaa, theta=10, phi=20, ticktype="detailed", expand=2/3,shade=0.5,main = "cpA a")
  #persp(S, E, fit.cpab, theta=10, phi=20, ticktype="detailed", expand=2/3,shade=0.5,main = "cpA b")
  persp(S, E, fit.ca, theta=30, phi=20, ticktype="detailed", expand=2/3,shade=0.5,main = latex2exp::TeX("$x_{food}$") , zlab="")
  persp(S, E, fit.cb, theta=30, phi=20, ticktype="detailed", expand=2/3,shade=0.5,main = latex2exp::TeX("$x_{non-food}$"), zlab="")
  
  
}

run_non_parametric_regression_for_nu_vs_r <- function(ll,dfslist,year,sp,r_type)
{
  select_df=paste0("df",year)
  
  S <- with(dfslist[[select_df]], seq(min(S), max(S), len=25))
  E <- with(dfslist[[select_df]], seq(min(E), max(E), len=25))
  newdata <- expand.grid(S=S, E=E)
  if (r_type == "educ") {
    plot_data <- dfslist[[select_df]] %>% mutate (excess_unit_asset = nu_educ/r_educ)
  } else if (r_type == "occup") {
    plot_data <- dfslist[[select_df]] %>% mutate (excess_unit_asset = nu_occup/r_occup)
  } else if (r_type == "all") {
    plot_data <- dfslist[[select_df]] %>% mutate (excess_unit_asset = nu/r)
  } else {
    stop("Unknown r_type")
  }
  
  mod.lo_nu_r <- loess(excess_unit_asset ~ S + E , span=sp, degree=1, data=plot_data)
  
  fit.nu_r <- matrix(predict(mod.lo_nu_r, newdata), 25, 25)

  par(mfrow=c(1,1))
  
  persp(S, E, fit.nu_r, theta=10, phi=20, ticktype="detailed", expand=2/3,shade=0.5,main = latex2exp::TeX("$\\nu/r$"), zlab = "")
  
}

run_non_parametric_regression_for_A <- function(ll,dfslist,year,sp,theta,phi)
{
  #Also plot - plot(data=nf[["df2012"]] %>% mutate(log_cost=log(mean_cost_ne)), log_cost ~ r, xlab=latex2exp::TeX("$r_t$"),ylab = latex2exp::TeX("$log(x_t)$"))")

  select_df=paste0("df",year)
  
  S <- with(dfslist[[select_df]], seq(min(S), max(S), len=25))
  E <- with(dfslist[[select_df]], seq(min(E), max(E), len=25))
  newdata <- expand.grid(S=S, E=E)
  mod.lo_lnA <- loess(lnA0 ~ S + E , span=sp, degree=1, data=dfslist[[select_df]] )
  
  fit.lo_lnA <- matrix(predict(mod.lo_lnA, newdata), 25, 25)
  
  par(mfrow=c(1,1))
  if (missing(theta)){
    theta <- 10
  }
  if (missing(phi)){
    phi <- 20
  }
  persp(S, E, fit.lo_lnA, theta=theta, phi=phi, ticktype="detailed", expand=2/3,shade=0.5,main = latex2exp::TeX("$log(A)$"), zlab = "")
  
}

run_non_parametric_regression_for_food_nonfood_ne <- function(ll,dfslist,year,sp,theta,phi)
{
  #Also plot - plot(data=nf[["df2012"]] %>% mutate(log_cost=log(mean_cost_ne)), log_cost ~ r, xlab=latex2exp::TeX("$r_t$"),ylab = latex2exp::TeX("$log(x_t)$"))")
  
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


run_non_parametric_regression_for_hsize <- function(ll,dfslist,year,sp,theta,phi)
{
  #Also plot - plot(data=nf[["df2012"]] %>% mutate(log_cost=log(mean_cost_ne)), log_cost ~ r, xlab=latex2exp::TeX("$r_t$"),ylab = latex2exp::TeX("$log(x_t)$"))")
  
  select_df=paste0("df",year)
  
  S <- with(dfslist[[select_df]], seq(min(S), max(S), len=25))
  E <- with(dfslist[[select_df]], seq(min(E), max(E), len=25))
  newdata <- expand.grid(S=S, E=E)
  mod.lo_hsize <- loess(hsize ~ S + E , span=sp, degree=1, data=dfslist[[select_df]] )
  
  fit.lo_hsize <- matrix(predict(mod.lo_hsize, newdata), 25, 25)
  
  par(mfrow=c(1,1))
  if (missing(theta)){
    theta <- 10
  }
  if (missing(phi)){
    phi <- 20
  }
  persp(S, E, fit.lo_hsize, theta=theta, phi=phi, ticktype="detailed", expand=2/3,shade=0.5,main = latex2exp::TeX("$\\psi$"), zlab = "", zlim=c(0,7))
  #
  
}


run_non_parametric_regression_for_perception <- function(ll,dfslist,year,sp)
{
  select_df=paste0("df",year)
  
  S <- with(dfslist[[select_df]], seq(min(S), max(S), len=25))
  E <- with(dfslist[[select_df]], seq(min(E), max(E), len=25))
  newdata <- expand.grid(S=S, E=E)
  
  mod.lo_life_percept <- loess(hh_life_perception ~ S + E , span=sp, degree=1, data=dfslist[[select_df]] )
  mod.lo_rich_percept <- loess(hh_richness_perception ~ S + E , span=sp, degree=1, data=dfslist[[select_df]] )
  mod.lo_housing_percept <- loess(hh_housing_perception ~ S + E , span=sp, degree=1, data=dfslist[[select_df]] )
  mod.lo_finance_percept <- loess(hh_finance_perception ~ S + E , span=sp, degree=1, data=dfslist[[select_df]] )
  #hh_finance_perception, hh_richness_perception, hh_housing_perception, hh_health_perception
  par(mfrow=c(2,2))
  
  fit.lo_life_percept <- matrix(predict(mod.lo_life_percept, newdata), 25, 25)
  fit.lo_rich_percept <- matrix(predict(mod.lo_rich_percept, newdata), 25, 25)
  fit.lo_housing_percept <- matrix(predict(mod.lo_housing_percept, newdata), 25, 25)
  fit.lo_finance_percept <- matrix(predict(mod.lo_finance_percept, newdata), 25, 25)
  
  persp(S, E, fit.lo_life_percept, theta=10, phi=20, ticktype="detailed", expand=2/3,shade=0.5,main = latex2exp::TeX("$perception_{life}$"), zlab = "")
  persp(S, E, fit.lo_rich_percept, theta=10, phi=20, ticktype="detailed", expand=2/3,shade=0.5,main = latex2exp::TeX("$perception_{richness}$"), zlab = "")
  persp(S, E, fit.lo_housing_percept, theta=10, phi=20, ticktype="detailed", expand=2/3,shade=0.5,main = latex2exp::TeX("$perception_{housing}$"), zlab = "")
  persp(S, E, fit.lo_finance_percept, theta=10, phi=20, ticktype="detailed", expand=2/3,shade=0.5,main = latex2exp::TeX("$perception_{finance}$"), zlab = "")
  
  
}


estimation_df_budget_quantile<- function(ll,e)
{
  inc_houserent = F
  inc_educexpense = F
  #stop("Missing houserent calculation from ohs data")
  if (missing(e)){
    e <- minimum_household_needs_wo_usage(ll = ll, c2010 = c2010, c2012 = c2012, c2014 = c2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)
  }
  
  #if (missing(pares)){
  #  pares <- plain_asset_differences_2012_2014(a2012 = a2012, a2014 = a2014, o2012 = o2012, o2014 = o2014, pivot_asset = "stove_other")
  #}
  
  #needs don't include housing fee (transport fee must also be removed)
  psiA2010 <- plyr::rename(subset(e$df,year==2010), c("hhid"="hhid2010"))
  
  needs2012 <- plyr::rename(subset(e$df,year==2012), c("hhid"="hhid2012"))
  psiA2012 <- needs2012 # merge(pares$df,needs2012,all.y=TRUE)
  #psiA2012[is.na(psiA2012$netmtm_fdelta),]$netmtm_fdelta <-0
  
  psiA2014 <- plyr::rename(subset(e$df,year==2014), c("hhid"="hhid2014"))
  
  
  # 1. we can't rely on regions too much because the house-rents are missing in some regions
  # 2. We can treat all habits as quality (as they are indistinguishable)
  #    The cost of asset owners in every neighborhood can be substracted. Psi should be presented not 
  #    as the cost of assets but rather as classification of elements of non-durable consumption i.e. c.
  #    The criteria of quality or habit cannot be decided subjectively - so we can set this up as something doing away with which would reduce A
  #    that we don't consider part of quality. Empirically this also avoids us having to
  #    calculate (and depend on) usage or expected costs.
  #  3. What about marriage and funeral? Do we ignore them? They should be seen as liability - but they are not asset costs.
  #  4. We don't know the basic needs - there is no point in even having that argument - is bicycle repair addressing a basic need (depends on the job that person has). We
  #     don't know what needs mean at personal levels - we can say in general the bicycle repairs must be a need for those who have a bicycle. A few questions we consider
  #     If people spend a lot more on food let's say in a particular region then they would stand
  #     out the idea is to understand quality except the burden of assets.
  
  #     4.1. Do liability costs increase with the relevant asset (s)?	(this can't be false for something that's in Psi)
  #     4.2. Is consumption necessary given the asset level where one is? (if it is necessary purely due to inertia reasons then it cannot be a part of Psi) - it must be
  #          clarified (as the write-up doesn't) that who considers something "necessary". We're saying on one hand that all necessities must go in Psi but at 
  #          the same time letting what consumers considers necessary to be a part of Psi as well. Only what assets bear must be in Psi - rest (quality habit) should go into 
  #          P_nu*nu. One argument that we would need to consider with this empirical method is that individuals can't really wear a gudadi when they have a car - so why
  #          are we counting their clothing expenditure as conscpicuous consumption - aren't needs changed when people are richer. In terms of the model, Psi is changed 
  #          for richer people so they automatically have a higher reference point - they face a higher price of relative quality (reference point being Psi (A)). A rich guy
  #          would obviously shell out more cash for marriage, we've looking at P_nu defined only among those with similar Psi. The alternative would
  #          be to consider Psi + a regional- average (if were to estimate P_nu elasticities)
  #     4.3. Does increasing consumption improve quality? (this has to be true - but we don't worry about what's perceived within Psi  - all perceptions go into P_nu*nu).
  #  5. Finally it does look like we have to bring back the required quantities for food - because this is a quality where there is never an imposition due to an asset (only habit) - so 
  #     we subtract the food basket cost as part of needs. There is no other way other than to impose a calorific value or weight(which is not appropriate).
  #     5.1. For Food, this is the recq we already had
  #     5.2. For energy, if somebody spent a certain amount on electricity - we consider that Psi (so no problem arises). We don't use the assume_assets - because the bills are already included.
  #  6. region-wide is as accurate since the weights for smaller regions is less
  #  
  #
  
  
  perception_columns <- c("life_perception"="hh_life_perception" , "finance_perception"="hh_finance_perception", "richness_perception"="hh_richness_perception","housing_perception"="hh_housing_perception","health_perception"="hh_health_perception")
  hhead_columns <- c("hhid"="hhid","years_community"="hh_years_community","age"="hh_age","highest_educ"="hh_highest_educ","occupation_rank"="hh_occupation_rank","litlang"="hh_litlang")
  #total consumption
  
  # 2010
  ohs2010 <- subset(o2010,!is.na(region))
  hs2010 <- unique(merge(unique(ohs2010[,c("hhid","region","district","ward","isrural","expensiveregion")]), ll@get_hsize(ohs2010), by = c("hhid")))
  chosenchars2010 <- ddply(ohs2010[,c("hhid","education_rank","occupation_rank","litlang","age")],.(hhid),summarise,max_education_rank = choose_max_non_na(education_rank) , max_occupation_rank = choose_max_non_na(occupation_rank) , litlang = choose_max_litlang(litlang), age=choose_max_non_na(age))
  
  #  perception_columns
  
  hhead2010 <- plyr::rename(subset(o2010,household_status==1)[,names(hhead_columns)],hhead_columns )
  chosencharshead2010 <- merge(chosenchars2010,hhead2010, all.x=T)
  
  
  hswithchars2010 <- merge(hs2010,chosencharshead2010,all.x = T)
  psiAregion2010 <- merge(plyr::rename(hswithchars2010,c("hhid"="hhid2010")),psiA2010,by=c("hhid2010"))
  ct2010<- plyr::rename(ll@get_total_expenditures(hh = c2010, ohs = ohs2010, include_education=inc_educexpense, include_houserent = inc_houserent), c("hhid"="hhid2010","total_expenditure"="ct"))
  psiAregionct2010 <- (merge(psiAregion2010,ct2010,by=c("hhid2010")))
  
  # 2012
  
  
  ohs2012 <- subset(o2012,!is.na(region))
  hs2012 <- unique(merge(unique(ohs2012[,c("hhid","region","district","ward","isrural","expensiveregion")]), ll@get_hsize(ohs2012), by = c("hhid")))
  chosenchars2012 <- ddply(ohs2012[,c("hhid","education_rank","occupation_rank","age","litlang")],.(hhid),summarise,max_education_rank = choose_max_non_na(education_rank) , max_occupation_rank = choose_max_non_na(occupation_rank) , litlang = choose_max_litlang(litlang), age = choose_max_non_na(age))
  
  if (length(setdiff(names(perception_columns),colnames(o2012)))==0){
    hhead_columns_w_percept <- c(hhead_columns,perception_columns)
  }
  
  hhead2012 <- plyr::rename(subset(o2012,household_status==1)[,names(hhead_columns_w_percept)],hhead_columns_w_percept )
  
  chosencharshead2012 <- merge(chosenchars2012,hhead2012, all.x=T)
  hswithchars2012 <- merge(hs2012,chosencharshead2012,all.x = T)
  
  psiAregion2012 <- merge(plyr::rename(hswithchars2012,c("hhid"="hhid2012")),psiA2012,by=c("hhid2012"))
  ct2012<- plyr::rename(ll@get_total_expenditures(hh = c2012, ohs = ohs2012, include_education = inc_educexpense, include_houserent = inc_houserent), c("hhid"="hhid2012","total_expenditure"="ct"))
  psiAregionct2012 <- (merge(psiAregion2012,ct2012,by=c("hhid2012")))
  
  #2014
  
  ohs2014 <- subset(o2014,!is.na(region))
  hs2014 <- unique(merge(unique(ohs2014[,c("hhid","region","district","ward","isrural","expensiveregion")]), ll@get_hsize(ohs2014), by = c("hhid")))
  chosenchars2014 <- ddply(ohs2014[,c("hhid","education_rank","occupation_rank","age","litlang")],.(hhid),summarise,max_education_rank = choose_max_non_na(education_rank) , max_occupation_rank = choose_max_non_na(occupation_rank) , litlang = choose_max_litlang(litlang), age=choose_max_non_na(age))
  
  hhead2014 <- plyr::rename(subset(o2014,household_status==1)[,names(hhead_columns)],hhead_columns )
  
  chosencharshead2014 <- merge(chosenchars2014,hhead2014, all.x=T)
  
  hswithchars2014 <- merge(hs2014,chosencharshead2014,all.x = T)
  psiAregion2014 <- merge(plyr::rename(hswithchars2014,c("hhid"="hhid2014")),psiA2014,by=c("hhid2014"))
  ct2014<- plyr::rename(ll@get_total_expenditures(hh = c2014, ohs = ohs2014,include_education = inc_educexpense, include_houserent = inc_houserent), c("hhid"="hhid2014","total_expenditure"="ct"))
  psiAregionct2014 <- (merge(psiAregion2014,ct2014,by=c("hhid2014")))
  
  # use to infer Psi and categories_non_basic_wassets
  psi_groups <- subset(lsms_normalizer()@categories_non_basic_wassets(),group=="asset_costs")
  
  psi_data_2010    <- plyr::rename(ddply(merge(c2010,psi_groups,by=c("shortname"))[c("hhid","shortname","cost")],.(hhid),summarise,Psi=sum(cost)), c("hhid"="hhid2010"))
  psi_data_2012    <- plyr::rename(ddply(merge(c2012,psi_groups,by=c("shortname"))[c("hhid","shortname","cost")],.(hhid),summarise,Psi=sum(cost)), c("hhid"="hhid2012"))
  psi_data_2014    <- plyr::rename(ddply(merge(c2014,psi_groups,by=c("shortname"))[c("hhid","shortname","cost")],.(hhid),summarise,Psi=sum(cost)), c("hhid"="hhid2014"))
  
  if (inc_educexpense){
    print("Adding education expense")
    psi_data_2010    <- (merge(psi_data_2010, psiAregionct2010[,c("hhid2010","toteducexpense")],by=c("hhid2010")) %>%  mutate(Psi=Psi+toteducexpense))[c("hhid2010","Psi")]
    psi_data_2012    <- (merge(psi_data_2012, psiAregionct2012[,c("hhid2012","toteducexpense")],by=c("hhid2012")) %>%  mutate(Psi=Psi+toteducexpense))[c("hhid2012","Psi")]
    psi_data_2014    <- (merge(psi_data_2014, psiAregionct2014[,c("hhid2014","toteducexpense")],by=c("hhid2014")) %>%  mutate(Psi=Psi+toteducexpense))[c("hhid2014","Psi")]
  }
  if (inc_houserent){
    print("Adding houserent")
    psi_data_2010    <- (merge(psi_data_2010, psiAregionct2010[,c("hhid2010","tothouserent")],by=c("hhid2010")) %>% mutate(Psi=Psi+tothouserent))[c("hhid2010","Psi")]
    psi_data_2012    <- (merge(psi_data_2012, psiAregionct2012[,c("hhid2012","tothouserent")],by=c("hhid2012")) %>% mutate(Psi=Psi+tothouserent))[c("hhid2012","Psi")]
    psi_data_2014    <- (merge(psi_data_2014, psiAregionct2014[,c("hhid2014","tothouserent")],by=c("hhid2014")) %>% mutate(Psi=Psi+tothouserent))[c("hhid2014","Psi")]
    
  }
  
  
  
  #2010
  psiAregionctPsiExcess2010 <- merge(psiAregionct2010,psi_data_2010)
  cA2010 <- ddply(plyr::rename(merge(c2010,subset(lsms_normalizer()@categories_non_basic_wassets(),group=="assets"))[,c("hhid","shortname","cost")],c("hhid"="hhid2010","cost"="cA")) , .(hhid2010),summarise,cA=sum(cA))
  psiAregionctPsi2010 <- merge(psiAregionctPsiExcess2010,cA2010)
  
  #2012
  psiAregionctPsiExcess2012 <- merge(psiAregionct2012,psi_data_2012)
  cA2012 <- ddply(plyr::rename(merge(c2012,subset(lsms_normalizer()@categories_non_basic_wassets(),group=="assets"))[,c("hhid","shortname","cost")],c("hhid"="hhid2012","cost"="cA")) , .(hhid2012),summarise,cA=sum(cA))
  psiAregionctPsi2012 <- merge(psiAregionctPsiExcess2012,cA2012)
  
  #2014
  psiAregionctPsiExcess2014 <- merge(psiAregionct2014,psi_data_2014)
  cA2014 <- ddply(plyr::rename(merge(c2014,subset(lsms_normalizer()@categories_non_basic_wassets(),group=="assets"))[,c("hhid","shortname","cost")],c("hhid"="hhid2014","cost"="cA")) , .(hhid2014),summarise,cA=sum(cA))
  psiAregionctPsi2014 <- merge(psiAregionctPsiExcess2014,cA2014)
  
  ############## Assigning weights ####
  
  psiAregionctPsi2010$dA <- with(psiAregionctPsi2010, cA) #cA+ netmtm_fdelta)
  psiAregionctPsi2010$w_A <- with(psiAregionctPsi2010,(dA)/(ct-basic_needs_cost-Psi+dA))
  psiAregionctPsi2010$w_nu <- with(psiAregionctPsi2010,(ct-basic_needs_cost-Psi)/(ct-basic_needs_cost-Psi+dA))
  
  psiAregionctPsi2012$dA <- with(psiAregionctPsi2012, cA) #cA+ netmtm_fdelta)
  psiAregionctPsi2012$w_A <- with(psiAregionctPsi2012,(dA)/(ct-basic_needs_cost-Psi+dA))
  psiAregionctPsi2012$w_nu <- with(psiAregionctPsi2012,(ct-basic_needs_cost-Psi)/(ct-basic_needs_cost-Psi+dA))
  
  psiAregionctPsi2014$dA <- with(psiAregionctPsi2014, cA) #cA+ netmtm_fdelta)
  psiAregionctPsi2014$w_A <- with(psiAregionctPsi2014,(dA)/(ct-basic_needs_cost-Psi+dA))
  psiAregionctPsi2014$w_nu <- with(psiAregionctPsi2014,(ct-basic_needs_cost-Psi)/(ct-basic_needs_cost-Psi+dA))
  
  
  ##############
  
  # setting lnA0
  ##2010
  hhids2010_2012 <- mapping_hhids_2010_2012(o2012)
  
  asset_mtms_2012 = asset_mtms(a2012,"bed","2012")
  asset_mtms_2010 <- plyr::rename(merge(hhids2010_2012,asset_mtms_2012),c("mtm.2012"="mtm.2010","cost.2012"="cost.2010","number.2012"="number.2010"))[,c("hhid2010","hhid2012","shortname","number.2010","mtm.2010","cost.2010")]
  
  assetslog2010 <- ddply(asset_mtms_2010,.(hhid2010),summarise,lnA0=log(sum(number.2010*mtm.2010)),A0=(sum(number.2010*mtm.2010)))
  psiAregionctPsiA02010 <- merge(psiAregionctPsi2010,assetslog2010,all.x = T)
  if (nrow(subset(psiAregionctPsiA02010,is.na(lnA0)))>0){
    psiAregionctPsiA02010[is.na(psiAregionctPsiA02010$lnA0),]$lnA0 <- 0
  }
  
  print(paste("Ignoring ",nrow(subset(psiAregionctPsiA02010,w_nu<0 | w_A<0)),"rows"))
  
  ##2012
  
  assetslog2012 <- ddply(asset_mtms_2012,.(hhid2012),summarise,lnA0=log(sum(number.2012*mtm.2012)),A0=sum(number.2012*mtm.2012))
  psiAregionctPsiA02012 <- merge(psiAregionctPsi2012,assetslog2012,all.x = T)
  if (nrow(subset(psiAregionctPsiA02012,is.na(lnA0)))>0){
    psiAregionctPsiA02012[is.na(psiAregionctPsiA02012$lnA0),]$lnA0 <- 0
  }
  
  print(paste("Ignoring ",nrow(subset(psiAregionctPsiA02012,w_nu<0 | w_A<0)),"rows"))
  
  #2014
  
  asset_mtms_2014 = asset_mtms(a2014,"bed","2014")
  assetslog2014 <- ddply(asset_mtms_2014,.(hhid2014),summarise,lnA0=log(sum(number.2014*mtm.2014)),A0=sum(number.2014*mtm.2014))
  psiAregionctPsiA02014 <- merge(psiAregionctPsi2014,assetslog2014,all.x = T)
  if (nrow(subset(psiAregionctPsiA02014,is.na(lnA0)))>0){
    psiAregionctPsiA02014[is.na(psiAregionctPsiA02014$lnA0),]$lnA0 <- 0
  }
  
  print(paste("Ignoring ",nrow(subset(psiAregionctPsiA02014,w_nu<0 | w_A<0)),"rows"))
  # Asset and excess-per-heads plots
  #plot_type='p';yy=2e+7; par(mfrow=c(3,1)); plot(psiAregionctPsiA02010$lnA0,with(psiAregionctPsiA02010,(ct-basic_needs_cost-Psi)),ylim=c(0,yy),xlim=c(0,20),type=plot_type); plot(psiAregionctPsiA02012$lnA0,with(psiAregionctPsiA02012,(ct-basic_needs_cost-Psi)),ylim=c(0,yy),xlim=c(0,20),type=plot_type);  plot(psiAregionctPsiA02014$lnA0,with(psiAregionctPsiA02014,(ct-basic_needs_cost-Psi)),ylim=c(0,yy),xlim=c(0,20),type=plot_type)
  #yy=9e+6; par(mfrow=c(3,1)); plot(psiAregionctPsiA02010$A0,with(psiAregionctPsiA02010,(ct-basic_needs_cost-Psi)),ylim=c(0,yy),xlim=c(0,2e+5)); plot(psiAregionctPsiA02012$A0,with(psiAregionctPsiA02012,(ct-basic_needs_cost-Psi)),ylim=c(0,yy),xlim=c(0,2e+5));  plot(psiAregionctPsiA02014$A0,with(psiAregionctPsiA02014,(ct-basic_needs_cost-Psi)),ylim=c(0,yy),xlim=c(0,2e+5))
  
  #yy=1e+7; par(mfrow=c(3,1)); plot(psiAregionctPsiA02010$lnA0,with(psiAregionctPsiA02010,(Psi)),ylim=c(0,yy),xlim=c(0,20)); plot(psiAregionctPsiA02012$lnA0,with(psiAregionctPsiA02012,(Psi)),ylim=c(0,yy),xlim=c(0,20));  plot(psiAregionctPsiA02014$lnA0,with(psiAregionctPsiA02014,(Psi)),ylim=c(0,yy),xlim=c(0,20))
  #yy=1e+7; par(mfrow=c(3,1)); plot(psiAregionctPsiA02010$lnA0,with(psiAregionctPsiA02010,(dA)),ylim=c(0,yy),xlim=c(0,20)); plot(psiAregionctPsiA02012$lnA0,with(psiAregionctPsiA02012,(dA)),ylim=c(0,yy),xlim=c(0,20));  plot(psiAregionctPsiA02014$dA,with(psiAregionctPsiA02014,(Psi)),ylim=c(0,yy),xlim=c(0,20))
  
  ## Visually, w_nu seems to represent the higher excess in the middle income groups:
  #par(mfrow=c(2,1)) ; plot(psiAregionctPsiA02012$lnA0, with(psiAregionctPsiA02012,(ct-basic_needs_cost-Psi))); plot(psiAregionctPsiA02012$lnA0, with(psiAregionctPsiA02012,(w_nu)))
  
  #### fix absence of isrural psiAregionctPsiA02014 ######
  region_identifier_cols <- c("region","district","ward")
  region_identifier_data2012 <- unique(psiAregionctPsiA02012[,region_identifier_cols])
  if (nrow(subset(region_identifier_data2012,is.na(ward)))>0){
    region_identifier_data2012[is.na(region_identifier_data2012$ward)]$ward <- -1
  }
  isrural_info_available <- unique(psiAregionctPsiA02012[,c(region_identifier_cols,"isrural")])
  # treating urban areas with rural wards as urban since min(nonrural=0,isrural=1)= nonrural(0)
  isrural_info_available <- ddply(isrural_info_available,.(region,district),summarise,isrural=min(isrural))
  psiAregionctPsiA02014$isrural <- NULL
  if(nrow(subset(isrural_info_available,is.na(isrural)))){
    stop(paste("No info found for ",nrow(subset(isrural_info_available,is.na(isrural))),toString(region_identifier_cols)))
  }
  
  psiAregionctPsiA02014 <- merge(psiAregionctPsiA02014,isrural_info_available,by=c("region","district"))
  ################ end psiAregionctPsiA02014 isrural fix #####
  
  
  ############## 2010
  
  psiAregionctPsiA02010$logx <- sapply(psiAregionctPsiA02010$ct-psiAregionctPsiA02010$basic_needs_cost-psiAregionctPsiA02010$Psi+psiAregionctPsiA02010$dA, function(x) { if(x<0) { -log(-x) } else {log(x)} } )
  psiAregionctPsiA02010$log_needs_price <- sapply(psiAregionctPsiA02010$needs_price, function(x) { if(x<0) { -log(-x) } else {log(x)} } )
  
  psiAregionctPsiA02010 <- subset(psiAregionctPsiA02010,w_nu>=0 & w_A>=0)
  A_bands <- c(.3,.65,1)
  A_band_boundaries <- quantile(psiAregionctPsiA02010$lnA0,A_bands)
  psiAregionctPsiA02010$A_band <- sapply(psiAregionctPsiA02010$lnA0,function(x) { get_band(c(-Inf,A_band_boundaries[1],A_band_boundaries[2]),c(A_band_boundaries[1],A_band_boundaries[2],A_band_boundaries[3]),x) })
  
  psiAregionctPsiA02010$group <- paste0("A",psiAregionctPsiA02010$A_band,"E",psiAregionctPsiA02010$expensiveregion,"R",psiAregionctPsiA02010$isrural)
  psiAregionctPsiA02010$group_code <- with(psiAregionctPsiA02010,100*A_band+ 10*expensiveregion + isrural)
  
  
  ############## 2012
  
  psiAregionctPsiA02012$logx <- sapply(psiAregionctPsiA02012$ct-psiAregionctPsiA02012$basic_needs_cost-psiAregionctPsiA02012$Psi+psiAregionctPsiA02012$dA, function(x) { if(x<0) { -log(-x) } else {log(x)} } )
  psiAregionctPsiA02012$log_needs_price <- sapply(psiAregionctPsiA02012$needs_price, function(x) { if(x<0) { -log(-x) } else {log(x)} } )
  
  psiAregionctPsiA02012 <- subset(psiAregionctPsiA02012,w_nu>=0 & w_A>=0)
  A_bands <- c(.3,.65,1)
  A_band_boundaries <- quantile(psiAregionctPsiA02012$lnA0,A_bands)
  psiAregionctPsiA02012$A_band <- sapply(psiAregionctPsiA02012$lnA0,function(x) { get_band(c(-Inf,A_band_boundaries[1],A_band_boundaries[2]),c(A_band_boundaries[1],A_band_boundaries[2],A_band_boundaries[3]),x) })
  
  psiAregionctPsiA02012$group <- paste0("A",psiAregionctPsiA02012$A_band,"E",psiAregionctPsiA02012$expensiveregion,"R",psiAregionctPsiA02012$isrural)
  psiAregionctPsiA02012$group_code <- with(psiAregionctPsiA02012,100*A_band+ 10*expensiveregion + isrural)
  
  ############## 2014
  
  psiAregionctPsiA02014$logx <- sapply(psiAregionctPsiA02014$ct-psiAregionctPsiA02014$basic_needs_cost-psiAregionctPsiA02014$Psi+psiAregionctPsiA02014$dA, function(x) { if(x<0) { -log(-x) } else {log(x)} } )
  psiAregionctPsiA02014$log_needs_price <- sapply(psiAregionctPsiA02014$needs_price, function(x) { if(x<0) { -log(-x) } else {log(x)} } )
  
  psiAregionctPsiA02014 <- subset(psiAregionctPsiA02014,w_nu>=0 & w_A>=0)
  A_bands <- c(.3,.65,1)
  A_band_boundaries <- quantile(psiAregionctPsiA02014$lnA0,A_bands)
  psiAregionctPsiA02014$A_band <- sapply(psiAregionctPsiA02014$lnA0,function(x) { get_band(c(-Inf,A_band_boundaries[1],A_band_boundaries[2]),c(A_band_boundaries[1],A_band_boundaries[2],A_band_boundaries[3]),x) })
  
  psiAregionctPsiA02014$group <- paste0("A",psiAregionctPsiA02014$A_band,"E",psiAregionctPsiA02014$expensiveregion,"R",psiAregionctPsiA02014$isrural)
  psiAregionctPsiA02014$group_code <- with(psiAregionctPsiA02014,100*A_band+ 10*expensiveregion + isrural)
  
  #############
  
  # also report log of A - that's the band we're look at - i.e. people within the same band and those within the same rural / urban group - rural people care about rural with english-speaking. But looking at
  # rural district people with the assets within the band (take whatever comes within an arbitrary percentage range so that are some people within it).
  bw <- log(2);
  lw <- .2
  
  #psiAregionctPsiA02010$area_richness <- mapply(function(r,d){log( median(exp(subset(psiAregionctPsiA02010,region==r & district == d )$lnA0))  / median(exp(psiAregionctPsiA02010$lnA0))) },psiAregionctPsiA02010$region,psiAregionctPsiA02010$district)
  #ddply(psiAregionctPsiA02010,.(region),summarise,n=median(band_richness))
  
  res=list();
  res[["psiAregionctPsiA02010"]]=psiAregionctPsiA02010
  res[["psiAregionctPsiA02012"]]=psiAregionctPsiA02012
  res[["psiAregionctPsiA02014"]]=psiAregionctPsiA02014
  return(res)
  #prepare_results_with_band_metrics(psiAregionctPsiA02010=res[["psiAregionctPsiA02010"]],psiAregionctPsiA02012=res[["psiAregionctPsiA02012"]],psiAregionctPsiA02014=res[["psiAregionctPsiA02014"]],bw=bw,lw=lw,o2012=o2012,o2014=o2014)
  #file.remove('c:/temp/df2010_2012.dta');file.remove('c:/temp/df2012_2014.dta'); file.remove('c:/temp/resnu.tex');write_dta(df$df2010_2012,path = 'c:/temp/df2010_2012.dta');write_dta(df$df2012_2014,path = 'c:/temp/df2012_2014.dta')
}

prepare_results_with_band_metrics <- function(psiAregionctPsiA02010,psiAregionctPsiA02012,psiAregionctPsiA02014,bw,lw,o2012,o2014,expensive_filter,rural_filter){
  if (!missing(expensive_filter)){
    psiAregionctPsiA02010 <- subset(psiAregionctPsiA02010,expensiveregion==expensive_filter)
    psiAregionctPsiA02012 <- subset(psiAregionctPsiA02012,expensiveregion==expensive_filter)
    psiAregionctPsiA02014 <- subset(psiAregionctPsiA02014,expensiveregion==expensive_filter)
  }
  if (!missing(rural_filter)){
    psiAregionctPsiA02010 <- subset(psiAregionctPsiA02010,isrural==rural_filter)
    psiAregionctPsiA02012 <- subset(psiAregionctPsiA02012,isrural==rural_filter)
    psiAregionctPsiA02014 <- subset(psiAregionctPsiA02014,isrural==rural_filter)
  }
  psiAregionctPsiA02010 <- get_band_metrics(dat=plyr::rename(psiAregionctPsiA02010,c("hhid2010"="hhid")),b = bw, l=lw)
  psiAregionctPsiA02012 <- get_band_metrics(dat=plyr::rename(psiAregionctPsiA02012,c("hhid2012"="hhid")),b = bw, l=lw)
  psiAregionctPsiA02014 <- get_band_metrics(dat=plyr::rename(psiAregionctPsiA02014,c("hhid2014"="hhid")),b = bw, l=lw)
  
  
  res = list()
  res[["df2010"]] <- psiAregionctPsiA02010
  res[["df2012"]] <- psiAregionctPsiA02012
  res[["df2014"]] <- psiAregionctPsiA02014
  ## 2010 2012 mapping
  hhid2012_hhid10_mapping <- mapping_hhids_2010_2012(o2012 = o2012)
  psiAregionctPsiA_common_cols_2010_2012 <- intersect(colnames(psiAregionctPsiA02010),colnames(psiAregionctPsiA02012) )
  df2010_2012 <- merge(hhid2012_hhid10_mapping , plyr::rename(psiAregionctPsiA02012[,psiAregionctPsiA_common_cols_2010_2012],c("hhid"="hhid2012"))) %>% mutate(year = 2012)
  df2010_2012$hhid2012 <- NULL
  df2010_2012 <- plyr::rename(df2010_2012,c("hhid2010"="hhid"))
  df2010_2012 <- rbind(df2010_2012,psiAregionctPsiA02010[,psiAregionctPsiA_common_cols_2010_2012] %>% mutate(year =2010))
  df2010_2012$hhid <- as.factor(df2010_2012$hhid)
  split_hhids2010_2012 <- unique(subset(ddply(df2010_2012,.(hhid,year),summarise,n=length(consu)),n>1)$hhid)
  
  print(paste("Ignoring split",length(split_hhids2010_2012),"/",length(unique(df2010_2012$hhid)),"households"))
  df2010_2012 <- subset(df2010_2012,!is.element(hhid,split_hhids2010_2012))
  res[["df2010_2012"]] <- df2010_2012
  
  ##
  ## 2010 2012 mapping
  psiAregionctPsiA_common_cols_2012_2014 <- intersect(colnames(psiAregionctPsiA02012),colnames(psiAregionctPsiA02014) )
  hhid2014_hhid12_mapping <- mapping_hhids_2012_2014(o2014 = o2014)
  df2012_2014 <- merge(hhid2014_hhid12_mapping , plyr::rename(psiAregionctPsiA02014[,psiAregionctPsiA_common_cols_2012_2014],c("hhid"="hhid2014"))) %>% mutate(year = 2014)
  df2012_2014$hhid2014 <- NULL
  df2012_2014 <- plyr::rename(df2012_2014,c("hhid2012"="hhid"))
  df2012_2014 <- rbind(df2012_2014,psiAregionctPsiA02012[,psiAregionctPsiA_common_cols_2012_2014] %>% mutate(year =2012))
  df2012_2014$hhid <- as.factor(df2012_2014$hhid)
  split_hhids2012_2014 <- unique(subset(ddply(df2012_2014,.(hhid,year),summarise,n=length(consu)),n>1)$hhid)
  
  print(paste("Ignoring split",length(split_hhids2012_2014),"/",length(unique(df2012_2014$hhid)),"households"))
  df2012_2014 <- subset(df2012_2014,!is.element(hhid,split_hhids2012_2014))
  res[["df2012_2014"]] <- df2012_2014
  print("DONE")
  return(res)
}

get_band_metrics <- function(dat,b,l){
  
  maxLocalAssets <- ddply(dat,.(region,district),summarise,maxLocallnA0=max(lnA0))
  
  dat$maxLocallnA0 <- NULL
  
  dat <- merge(dat,maxLocalAssets,by=c("region","district"))
  
  dat <- dat %>% mutate(A_left=lnA0-b) %>% mutate(A_right_ub=lnA0+b) %>% mutate(A_2right_ub=lnA0+2*b)
  dat$A_right <- NULL
  dat$A_2right <- NULL
  dat <- merge( dat, ddply(dat,.(hhid), summarise, A_right = min(maxLocallnA0,A_right_ub), A_2right = min(maxLocallnA0,A_2right_ub)), by = c("hhid"))
  
  dat$band_average <- mapply(function(x,y,r,d){log(median(exp(subset(dat,lnA0 > x & lnA0 <= y & region == r & district == d)$lnA0))) },dat$A_left,dat$A_right,dat$region,dat$district)
  
  #next-rich-band average is the band average for the richest band
  dat$nextrich_band_average <- mapply(function(x,y,r,d){log(median(exp(subset(dat,lnA0 >= x & lnA0 <= y & region == r & district == d)$lnA0))) },dat$A_right,dat$A_2right,dat$region,dat$district)
  dat$band_nat_average <- mapply(function(x,y){log(median(exp(subset(dat,lnA0 > x & lnA0 <= y )$lnA0))) },dat$A_left,dat$A_right)
  dat$band_cardinality <- mapply(function(x,y,r,d){nrow(subset(dat,lnA0 > x & lnA0 <= y  & region == r & district == d)) },dat$A_left,dat$A_right,dat$region,dat$district)
  #dat$w_nu_area <- mapply(function(h,x,y,r,d){mean(subset(dat,hhid != h & lnA0 > x & lnA0 <= y  & region == r & district == d)$w_nu) },dat$hhid, dat$A_left,dat$A_right,dat$region,dat$district)
  
  
  dat$band_richness <- with(dat,band_average-band_nat_average)
  dat$outofplace <- with(dat,nextrich_band_average-band_nat_average)
  dat$has_english <- dat$litlang==2 | dat$litlang==3
  #dat$same_lang_share <- mapply(function(x,y,r,d,h){sum(exp(subset(dat,lnA0 > x & lnA0 <= y  & region == r & district == d & has_english == h)$lnA0))/sum(exp(subset(dat,lnA0 > x & lnA0 <= y  & region == r & district == d)$lnA0)) },dat$A_left,dat$A_right,dat$region,dat$district, dat$has_english)
  dat$same_lang_share <- mapply(function(x,y,r,d,l){sum(exp(subset(dat,lnA0 > x & lnA0 <= y  & region == r & district == d & litlang == l)$lnA0))/sum(exp(subset(dat,lnA0 > x & lnA0 <= y  & region == r & district == d)$lnA0)) },dat$A_left,dat$A_right,dat$region,dat$district, dat$litlang)
  ##
  #KISWAHILI...1
  #KISWAHILI & ENGLISH.....2
  #ENGLISH...3
  #ANY OTHER LANGUAGE..4
  #NO .. 5
  
  # alternative formulation of band_richness : divide my asset value by the quantile-asset-value in the national asset hierarhcy where quantile is my AV in the local asset hierarchy. This  would tell me how rich an area I am in
  # If I am in a rich area, then my quantile's AV in the national hierarhcy is much lower (so I have a high score)
  # Similarly, if I am in a poor area, then my quantile's AV in the national hierarhcy is much high (so I have a low score)
  # If I am surrounded by rich people then I might feel miserable even though folks in the country are poorer than me i.e. low relative position locally but high national position.
  get_hhid_ranks_for_region_district <- function(r,d) {
    dd=subset(dat,region==r & district==d); 
    ranks =ecdf(dd$lnA0)(dd$lnA0) ; 
    next_ranks = ranks + l
    local_outofplace_ranks = sapply(next_ranks, function(x){quantile(dd$lnA0,min(x,1))}) - dd$lnA0
    return(data.frame(hhid=dd$hhid,local_rank=ranks, local_outofplace= local_outofplace_ranks))
  }
  # don't consider percentiles for less than 3 number of points
  dat <- subset(dat,band_cardinality>2)
  rds <- unique(dat[,c("region","district")])
  hhids_with_local_ranks <- NULL
  for (irow in seq(nrow(rds))){
    reg = rds[irow,]$region
    dis = rds[irow,]$district
    hhids_with_local_ranks <- rbind(hhids_with_local_ranks,get_hhid_ranks_for_region_district(r=reg,d=dis))
  }
  dat <- merge(dat,hhids_with_local_ranks,by=c("hhid"))
  dat$nat_asset_rankval <- sapply(dat$local_rank, function(x) { quantile(dat$lnA0,x)} )
  aranks <- ecdf(dat$lnA0)(dat$lnA0)
  next_aranks <- ecdf(dat$lnA0)(dat$lnA0) + l
  nat_outofplace_ranks = sapply(next_aranks, function(x){quantile(dat$lnA0,min(x,1))}) - dat$lnA0
  dat$nat_outofplace_rank <- nat_outofplace_ranks
  dat$richness_rank <- with(dat,lnA0-nat_asset_rankval)
  return (dat)
}

get_ABand_code_mapping <-function() {
  x <- data.frame()
  x <- rbind(x,data.frame(group='A1E0R0',group_code=100))
  x <- rbind(x,data.frame(group='A1E0R1',group_code=101))
  x <- rbind(x,data.frame(group='A1E1R0',group_code=110))
  x <- rbind(x,data.frame(group='A1E1R1',group_code=111))
  
  x <- rbind(x,data.frame(group='A2E0R0',group_code=200))
  x <- rbind(x,data.frame(group='A2E0R1',group_code=201))
  x <- rbind(x,data.frame(group='A2E1R0',group_code=210))
  x <- rbind(x,data.frame(group='A2E1R1',group_code=211))
  
  x <- rbind(x,data.frame(group='A3E0R0',group_code=300))
  x <- rbind(x,data.frame(group='A3E0R1',group_code=301))
  x <- rbind(x,data.frame(group='A3E1R0',group_code=310))
  x <- rbind(x,data.frame(group='A3E1R1',group_code=311))
  return(x)
}

get_band <-function (band_left_limits, band_right_limits,x) {
  if (length(band_left_limits)!=length(band_right_limits)){
    stop("band_right_limits must be of the same size as band_left_limits")
  }
  for (i in seq(length(band_left_limits))){
    left_limit = band_left_limits[i]
    right_limit = band_right_limits[i]
    if (x<=right_limit && x > left_limit){
      return (i)
    }
    
  }
  if (x==-Inf){
    return(1)
  } 
  if (x==Inf){
    return(length(band_right_limits))
  }
  
  stop(paste0("Not found:",i))
}

#e <- minimum_household_needs_wo_usage(ll = ll, c2010 = c2010, c2012 = c2012, c2014 = c2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)
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

get_next_band_hhid <- function(d){
  dat <- d
  ranks <- ecdf(dat$lnA0)(dat$lnA0)
  next_ranks <- ranks+ .2
  dat$next_aval_extrapolated <- (sapply(next_ranks, function(x){quantile(dat$lnA0,min(x,1))}))
  next_band_df = data.frame()
  for ( i in seq(nrow(dat)) ) {
    cur_next_aval <- dat[i,]$next_aval_extrapolated
    next_band_set <- (dat %>% mutate(absdiff = abs(cur_next_aval - lnA0)) %>% filter( absdiff == min(absdiff)))
    next_band_df <- rbind(next_band_df,plyr::rename(next_band_set[1,][,c("lnA0","Psi")], c("lnA0"="next_band_aval","Psi"="next_band_psi") ))
  }
  return(cbind(dat,next_band_df))
}

estimation_df <-function( ll, pares, e, a2010, a2012, a2014, o2010, o2012, o2014, c2010, c2012, c2014 ){
  if (missing(e)){
    e <- minimum_needs_cost_per_head(ll = ll, c2010 = c2010, c2012 = c2012, c2014 = c2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)
  }
  if (missing(pares)){
    pares <- plain_asset_differences_2012_2014(a2012 = a2012, a2014 = a2014, o2012 = o2012, o2014 = o2014,pivot_asset="bed")
  }
  #if (missing(pseudop)){
  #  pseudop <- prepare_pseudo_panels_2010_2012_2014(o2010 = o2010, o2012 = o2012, o2014 = o2014, ll =ll , dirprefix = "../", fu=fu, 
  #                                                  ln=lsms_normalizer,ncdifftol = 2, yobtol = 3, i2010 = i2010, i2012 = i2012, i2014 = i2014,
  #                                                  calibrate_needs=FALSE)   
  #}
  
  adiff2014                                  <- pares[["df"]]
  asum2012                                   <- ddply(subset(plyr::rename(pares[["dat0"]][,c("hhid","cost")],c("hhid"="hhid2012","cost"="cost.2012")), !is.na(cost.2012)),.(hhid2012),summarise,cost.2012=sum(cost.2012))
  df2012                                     <- merge(asum2012,adiff2014,by=c("hhid2012"),all.x=TRUE)
  df2012[is.na(df2012$netmtm_fdelta),]$netmtm_fdelta <- 0
  
  needscost2014      <- merge(plyr::rename(subset(e,year==2014),c("hhid"="hhid2014","needs_cost"="needs_cost_2014")), mapping_hhids_2012_2014(o2014 = o2014), by = c("hhid2014"))
  dfe2012            <- merge( df2012, needscost2014, by = c("hhid2012"))
  nonsplithhids2012  <- subset(ddply(dfe2012[,c("hhid2012","hhid2014")],.(hhid2012),summarise,n=length(unique(hhid2014))),n==1)$hhid2012
  dfe2012            <- subset(dfe2012,is.element(hhid2012,nonsplithhids2012))
  dfe2012            <- plyr::rename (dfe2012,c("cost.2012"="At","netmtm_fdelta"="dAt","needs_cost_2014"="Psit1"))
  
  #use o2014 data as that corresponds to i_{t+1}
  #take max_education_rank and max_occupation_rank 
  
  t1ranks            <- o2014[,c("hhid2012","expensiveregion","occupation_rank","education_rank")]
  t1rankseduc        <- unique(t1ranks[,c("hhid2012","education_rank")] %>% group_by(hhid2012) %>% filter(education_rank==choose_max_non_na(education_rank)))
  t1ranksoccu        <- unique(t1ranks[,c("hhid2012","occupation_rank")] %>% group_by(hhid2012) %>% filter(occupation_rank==max(occupation_rank)))
  t1ranksdat         <- merge(t1ranksoccu,t1rankseduc,by=c("hhid2012"))
  t0age              <- plyr::rename(subset(o2012[,c("hhid","age","household_status")],household_status==1),c("hhid"="hhid2012"))[,c("hhid2012","age")]
  
  dfe2012_dat        <- merge( t1ranksdat, dfe2012, by = c("hhid2012")) %>% mutate (is_higheduc = as.integer(education_rank==4))
  print(paste0("data after merging needs and max_education, max_occupation is with rows=",nrow(dfe2012_dat)))
  dfe2012_dat        <- merge(dfe2012_dat,unique(t1ranks[,c("hhid2012","expensiveregion")]))
  dfe2012_dat        <- merge(dfe2012_dat , t0age,by=c("hhid2012"))
  print(paste0("data after merging with age data is with rows=",nrow(dfe2012_dat)))
  
  statares2012       <- get_stata_income_re_results() 
  statares2012_eduf  <- statares2012[["eduf"]]
  statares2012_occf  <- statares2012[["occf"]]
  statares2012_expf  <- statares2012[["expf"]]
  statares2012_const <- statares2012[["const"]]
  dfe2012f1          <- merge(statares2012_occf,dfe2012_dat,by=c("occupation_rank"))
  if (dim(dfe2012f1[is.na(dfe2012f1$education_rank),])[1]>0){
    dfe2012f1[is.na(dfe2012f1$education_rank),]$education_rank <- 0
    dfe2012f1[(dfe2012f1$education_rank==0),]$is_higheduc <- 0
  }
  dfe2012f2          <- merge(statares2012_eduf,dfe2012f1,by=c("is_higheduc"))
  dfe2012f3          <- merge(statares2012_expf,dfe2012f2,by=c("expensiveregion"))
  dfe2012f3          <- dfe2012f3 %>% mutate(it1 = exp(expensiveregion*expcoeff + is_higheduc*edufcoef + occupation_rank*occcoef + statares2012_const))
  
  
  dfe2012f3$lt1       <- with(dfe2012f3,it1-Psit1)
  print("PENDING: Use only non-split households")
  xt1df               <- merge(plyr::rename( ddply(c2014,.(hhid),summarise,xt1=sum(cost)), c("hhid"="hhid2014") ), mapping_hhids_2012_2014(o2014), by=c("hhid2014"))
  dfe2012f4           <- merge(xt1df,dfe2012f3,by=c("hhid2012")) %>% mutate (nut1 = xt1-Psit1)
  
  res                <- list()
  res[["dAtres"]]    <- lm(data=dfe2012f4,dAt~At + lt1)
  res[["nut1res"]]   <- lm(data=dfe2012f4,nut1~At + lt1)
  res[["df"]]        <- dfe2012f4
  
  return (res)
}




get_asset_group <- function(){
  r <- data.frame()
  r=rbind(r,data.frame(shortname='bed' , asset_group='furniture'))
  r=rbind(r,data.frame(shortname='bike' , asset_group='nonelectric_transport'))
  r=rbind(r,data.frame(shortname='plough' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='sewingmachine' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='donkey' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='sofa' , asset_group='furniture'))
  r=rbind(r,data.frame(shortname='harrow' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='harvester' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='stove_electricgas' , asset_group='electric'))
  r=rbind(r,data.frame(shortname='tv' , asset_group='electronics'))
  r=rbind(r,data.frame(shortname='waterpump' , asset_group='electric'))
  r=rbind(r,data.frame(shortname='refrigerator' , asset_group='electric'))
  r=rbind(r,data.frame(shortname='animalcart' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='boat' , asset_group='nonelectric_transport'))
  r=rbind(r,data.frame(shortname='musicsystem' , asset_group='electronics'))
  r=rbind(r,data.frame(shortname='computer' , asset_group='electronics'))
  r=rbind(r,data.frame(shortname='trailer' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='handmill' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='motorbike' , asset_group='transport'))
  r=rbind(r,data.frame(shortname='house' , asset_group='property'))
  r=rbind(r,data.frame(shortname='powertiller' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='car' , asset_group='transport'))
  r=rbind(r,data.frame(shortname='tractor' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='cart' , asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='videoplayer' , asset_group='electronics'))
  r=rbind(r,data.frame(shortname='livestock' , asset_group='agricultural'))
  
  
  r=rbind(r,data.frame(shortname='mosquitonet',asset_group='furniture'))
  r=rbind(r,data.frame(shortname='stove_other',asset_group='electric'))
  r=rbind(r,data.frame(shortname='poultry',asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='watch',asset_group='electronics'))
  r=rbind(r,data.frame(shortname='chair',asset_group='furniture'))
  r=rbind(r,data.frame(shortname='iron',asset_group='electric'))
  r=rbind(r,data.frame(shortname='table',asset_group='furniture'))
  r=rbind(r,data.frame(shortname='coffeepulpingmachine',asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='waterheater',asset_group='electric'))
  r=rbind(r,data.frame(shortname='ac_fan',asset_group='electric'))
  r=rbind(r,data.frame(shortname='radio',asset_group='electronics'))
  r=rbind(r,data.frame(shortname='landline',asset_group='electronics'))
  r=rbind(r,data.frame(shortname='mobile',asset_group='electronics'))
  r=rbind(r,data.frame(shortname='wheelbarrow',asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='spraymachine',asset_group='agricultural'))
  r=rbind(r,data.frame(shortname='musicplayer',asset_group='electronics'))
  r=rbind(r,data.frame(shortname='dishtv',asset_group='electronics'))
  r=rbind(r,data.frame(shortname='engine_outboard',asset_group='electric'))
  
  
  
  #mapping:mosquitonet, stove_other, poultry, watch, chair, iron, table, coffeepulpingmachine, waterheater, ac_fan, radio, landline, mobile, wheelbarrow, spraymachine, musicplayer, dishtv, engine_outboard
  
  
  return(r)
}

comparator <- function(x,a,b) { if (is.na(x) || is.na(a) || is.na(b)) { return (NA)} else { if (x<a) { return(3)} else { if (x<b) {return(2)} else {return(1)} } } }

copyover_a <- function(a,b) { 
  if (is.na(a)){
    if (is.na(b)){
      stop("Cannot handle both NAs")
    } else {
      return (b)
    }
    
  } else {
    return (a)
  }
}

copyover_b <- function(a,b) { 
  if (is.na(b)){
    if (is.na(a)){
      stop("Cannot handle both NAs")
    } else {
      return (a)
    }
    
  } else {
    return (b)
  }
}


asset_mtms <- function(assets_dat,pivot_asset,year){
  
  assets <- subset(assets_dat,!is.na(cost) & cost >0 & number >0 & !is.na(number) )
  assets$shortname <- as.character(assets$shortname)
  
  assets_src    <- (dplyr::filter( merge(assets,ddply(assets,.(shortname),summarise,v=fu()@fv(cost)),all.x=TRUE) , cost < v))
  
  
  
  c0 <- ddply(subset(assets_src, number>0 & !is.na(cost) & cost>0), .(shortname), summarise , median_cost = median(cost), mean_cost = mean(cost), n = length(hhid))
  c0 <- c0[order(c0$mean_cost),]
  
  # anything ge expensive than pivot_asset is an asset
  
  all_assets              <- setdiff(subset(c0,median_cost>= c0[c0$shortname==pivot_asset,]$median_cost)$shortname,c()) #c("land","house")
  
  ag <- get_asset_group()
  if (length(setdiff( all_assets,unique(ag$shortname)) ) > 0) {
    stop(paste0("Missing Assets in the mapping:",toString(setdiff( all_assets,unique(ag$shortname)) )))
  }
  
  
  select_cols <- c("hhid","number","shortname","mtm","cost")
  a <- plyr::rename(subset(assets_src[,select_cols],number>0 & is.element(shortname,all_assets)),c("hhid"=paste0("hhid",year),"number"=paste0("number.",year),"mtm"=paste0("mtm.",year),"cost"=paste0("cost.",year)))
  return(a)
  
}

plain_asset_differences_2012_2014 <- function(a2012,a2014,o2012,o2014,pivot_asset){
  #a01_mapping could be mapping_hhids_2012_2014(o2014) for example
  # ignoring waterpump, musicplayer, sports_hobby, camera, phone because of low frequencies
  # ignoring the following as they stay within +1/-1 and are susceptible to recall error : watch
  # ignoring the following as they are susceptible to recall error: 'bed', 'chair',  'table', 'cupboard', 'sofa'
  
  
  #assetnames_transport   <- c('bike', 'motorbike', 'car')
  #assetnames_household   <- c(  'sewingmachine','land', 'house', 'bed', 'chair',  'table', 'cupboard', 'sofa',"house","livestock", "poultry","donkey")
  #assetnames_household   <- c('land', 'house')
  #assetnames_electric     <- c('mobile','videoplayer', 'ac_fan', 'waterpump', 'tv', 'dishtv', 'computer',  'refrigerator' )
  #assetnames_electric     <- c('mobile', 'waterheater', 'videoplayer', 'ac_fan', 'musicsystem', 'tv', 'dishtv', 'computer',  'refrigerator' , 'stove_electricgas')
  
  
  
  #all_assets             <- c(c(assetnames_electric,assetnames_household), assetnames_transport)
  
  
  a2012dat <- subset(a2012,!is.na(cost) & cost >0 & number >0 & !is.na(number) )
  a2012dat$shortname <- as.character(a2012dat$shortname)
  a2014dat <- subset(a2014,!is.na(cost) & cost >0 & number >0 & !is.na(number) )
  a2014dat$shortname <- as.character(a2014dat$shortname)
  
  a2012src    <- (dplyr::filter( merge(a2012dat,ddply(a2012dat,.(shortname),summarise,v=fu()@fv(cost)),all.x=TRUE) , cost < v))
  a2014src    <- (dplyr::filter( merge(a2014dat,ddply(a2014dat,.(shortname),summarise,v=fu()@fv(cost)),all.x=TRUE) , cost < v))
  
  
  #  a2012src    <- merge(a2012src, ddply(subset(a2012src,number>0 & cost>0 & !is.na(cost)),.(shortname),summarise, p30=quantile(cost,.3) , p60=quantile(cost,.6)), by=c("shortname"))
  #  a2014src    <- merge(a2014src, ddply(subset(a2014src,number>0 & cost>0 & !is.na(cost)),.(shortname),summarise, p30=quantile(cost,.3) , p60=quantile(cost,.6)), by=c("shortname"))
  
  c0 <- ddply(subset(a2012src, number>0 & !is.na(cost) & cost>0), .(shortname), summarise , median_cost = median(cost), mean_cost = mean(cost), n = length(hhid))
  c0 <- c0[order(c0$mean_cost),]
  c1 <- ddply(subset(a2014src, number>0 & !is.na(cost) & cost>0), .(shortname), summarise , median_cost = median(cost), mean_cost = mean(cost), n = length(hhid))
  c1 <- c1[order(c1$mean_cost),]
  
  # anything ge expensive than bed is an asset
  
  all_assets              <- setdiff(subset(c0,median_cost>= c0[c0$shortname==pivot_asset,]$median_cost)$shortname,c()) #c("land","house")
  
  ag <- get_asset_group()
  if (length(setdiff( all_assets,unique(ag$shortname)) ) > 0) {
    stop(paste0("Missing Assets in the mapping:",toString(setdiff( all_assets,unique(ag$shortname)) )))
  }
  
  a01_mapping <- mapping_hhids_2012_2014(o2014)
  select_cols <- c("hhid","number","shortname","mtm","cost")
  a0 <- plyr::rename(subset(a2012src[,select_cols],number>0 & is.element(shortname,all_assets)),c("hhid"="hhid2012","number"="number.2012","mtm"="mtm.2012","cost"="cost.2012"))
  a1 <- plyr::rename(subset(a2014src[,select_cols],number>0 & is.element(shortname,all_assets)),c("hhid"="hhid2014","number"="number.2014","mtm"="mtm.2014","cost"="cost.2014"))
  
  if (is.element("house",all_assets)){
    hs2012           <- unique(o2012[ ,c("hhid","housingstatus")]) 
    hs2012$has_house <- hs2012$housingstatus == 1
    hs2012           <- plyr::rename(hs2012,c("hhid"="hhid2012"))[,c("hhid2012","has_house")]
    
    hs2014 <- unique(o2014[ ,c("hhid","housingstatus")])
    hs2014$has_house <- hs2014$housingstatus ==1 
    hs2014 <- plyr::rename(hs2014,c("hhid"="hhid2014"))[,c("hhid2014","has_house")]
    
    b0 <- merge(hs2012[,c("has_house","hhid2012")],a0,all.y=TRUE)
    b0[b0$has_house==FALSE & b0$shortname == "house", ]$number.2012 <- 0
    a0 <- b0[,setdiff(colnames(b0),"has_house")]
    
    b1 <- merge(hs2014[,c("has_house","hhid2014")],a1,all.y=TRUE)
    b1[b1$has_house==FALSE & b1$shortname == "house", ]$number.2014 <- 0
    a1 <- b1[,setdiff(colnames(b1),"has_house")]
  }
  
  a1 <- merge(a1,a01_mapping)
  
  
  nonsplithhids <- subset(ddply(a01_mapping, .(hhid2012), summarise , n = length(hhid2014)),n==1)$hhid2012
  
  dat <- merge( subset(a0, is.element(hhid2012,nonsplithhids)), subset(a1,is.element(hhid2012,nonsplithhids)), all=T) 
  dat[is.na(dat$number.2012),]$number.2012 <-0
  dat[is.na(dat$mtm.2012),]$mtm.2012 <-0 
  dat[is.na(dat$number.2014),]$number.2014 <-0
  dat[is.na(dat$mtm.2014),]$mtm.2014 <-0 
  
  dat <- dat %>% mutate (delta = number.2014 - number.2012 )
  #get grade prices
  dat <- merge( dat, ddply(subset(a2012src,number>0 & cost>0 & !is.na(cost)),.(shortname),summarise, p15.2012=quantile(cost,.15), p30.2012=quantile(cost,.3), p45.2012=quantile(cost,.45) , p60.2012=quantile(cost,.6), p75.2012=quantile(cost,.75) ), by=c("shortname"))
  dat <- merge( dat, ddply(subset(a2014src,number>0 & cost>0 & !is.na(cost)),.(shortname),summarise, p15.2014=quantile(cost,.15), p30.2014=quantile(cost,.3), p45.2014=quantile(cost,.45) , p60.2014=quantile(cost,.6), p75.2014=quantile(cost,.75) ), by=c("shortname"))
  
  dat$grade.2012 <- mapply(comparator,dat$cost.2012,dat$p30.2012,dat$p60.2012)
  dat$grade.2014 <- mapply(comparator,dat$cost.2014,dat$p30.2014,dat$p60.2014)
  
  
  dat$grade.2012 <- mapply(copyover_a, dat$grade.2012, dat$grade.2014)
  dat$grade.2014 <- mapply(copyover_b, dat$grade.2012, dat$grade.2014)
  
  dat$icost.2014 <- (dat$grade.2014==1)*dat$p15.2014+(dat$grade.2014==2)*dat$p45.2014+(dat$grade.2014==3)*dat$p75.2014
  dat$fdelta     <- ((dat$number.2012>0) & (abs(dat$delta)>2)) | (( (dat$number.2012==0) | (dat$number.2014==0)) & (abs(dat$delta)!=0)); 
  dat$costdelta  <- (dat$fdelta==TRUE)*dat$delta*dat$icost.2014 + (dat$fdelta==FALSE)*0
  
  x              <- (ddply(dat, .(hhid2012), summarise, expenditure = sum(costdelta)))
  #merge to get the groups
  
  dat <- merge(dat, ag, all.x=TRUE) %>% mutate (netmtm.2014 = number.2014 * mtm.2014) %>% mutate(netmtm.2012 = number.2012 * mtm.2012 ) %>% mutate( netmtm.delta = netmtm.2014- netmtm.2012)
  
  if (any(is.na(dat$asset_group))){
    stop("Could not find asset group for every shortname")
  }
  
  dats               <- ddply(dat[c("shortname","hhid2012","number.2012","number.2014","netmtm.2012","netmtm.2014","asset_group","costdelta")],.(asset_group,hhid2012), summarise, number.2012 = sum(number.2012), number.2014 = sum(number.2014) , netmtm.2012 = sum(netmtm.2012), netmtm.2014 = sum(netmtm.2014), costdelta=sum(costdelta))
  dats               <- dats %>% mutate(delta = (number.2014-number.2012) , netmtm.delta = (netmtm.2014-netmtm.2012))
  dats               <- (dplyr::filter( merge(dats,ddply(dats,.(asset_group),summarise,v=fu()@fv10(netmtm.delta)),all.x=TRUE) , abs(netmtm.delta) < v))
  dats$fdelta        <- ((dats$number.2012>0) & ((dats$delta)>0)) | (( (dats$number.2012==0)) & ((dats$delta)>0))
  dats$netmtm_fdelta <- (dats$fdelta==FALSE)*0 + (dats$fdelta==TRUE)*dats$netmtm.delta
  
  d           <- subset(dat, is.element(shortname,all_assets))
  db          <- subset(d , number.2012 == 0 | number.2014 == 0 )
  do          <- subset(d , number.2012 > 0 & number.2014 > 0 )
  
  
  k           <- ddply(d,.(shortname),summarise, n= length(hhid2012) , median_change = median(delta), q85_change = quantile(delta,.85), q15_change = quantile(delta,.15))
  
  k           <- k[order(k$n),]
  
  kb          <- ddply(db,.(shortname),summarise, n= length(hhid2012) , median_change = median(delta), q85_change = quantile(delta,.85), q15_change = quantile(delta,.15))
  kb          <- kb[order(kb$n),]
  
  ko          <- ddply(do,.(shortname),summarise, n= length(hhid2012) , median_change = median(delta), q85_change = quantile(delta,.85), q15_change = quantile(delta,.15))
  ko          <- kb[order(kb$n),]
  
  ku0         <- ddply (subset(a2012src, number>0 & !is.na(cost) & cost>0) , .(shortname), summarise, k = moments::kurtosis(cost), s = moments::skewness(cost))
  ku1         <- ddply (subset(a2014src, number>0 & !is.na(cost) & cost>0) , .(shortname), summarise, k = moments::kurtosis(cost), s = moments::skewness(cost))
  
  datsres     <- ddply(dats, .(hhid2012), summarise, netmtm_fdelta = sum(netmtm_fdelta))
  
  
  
  
  res = list()
  res[["dat0"]] <- a2012src
  res[["dat1"]] <- a2014src
  res[["a0"]] <- a0
  res[["a1"]] <- a1
  res[["d"]] <- d
  res[["x"]] <- x
  res[["s"]] <- dats
  res[["df"]] <- datsres
  res[["k"]] <- k
  res[["kb"]] <- kb
  res[["ko"]] <- ko
  res[["db"]] <- db
  res[["do"]] <- do
  res[["c0"]] <- c0
  res[["c1"]] <- c1
  res[["ku0"]] <- ku0
  res[["ku1"]] <- ku1
  return(res)
}

logx <- function(x) { if (x==0) return (0) else { if (x<0) { return (-log(-x)) } else { return(log(x))} } }

tsne_analysis <- function(res){
  ss                 <- res[["s"]]
  ss                 <- ss[,c("asset_group","hhid2012","netmtm.delta")]
  ss$lognetmtm.delta <- sapply(ss$netmtm.delta, logx)
  ss                 <- ss[,c("asset_group","hhid2012","lognetmtm.delta")]
  ss                 <- ss %>% spread(asset_group, lognetmtm.delta)
  ss[is.na(ss)]      <- 0 
  rownames(ss)       <- ss$hhid2012
  ss$hhid2012        <- NULL
  tsres              <- tsne(ss, perplexity=50)
  tsres              <- as.data.frame(tsres)
  colnames(tsres)    <- c("X","Y")
  ss$X               <- tsres$X
  ss$Y               <- tsres$Y
  ss$hhid2012        <- row.names(ss)
  return(ss)
}

tsres_plot <- function (ss,o2012,a2012,color_field,coloffset){
  if (color_field =="housemtm"){
    ss2 <- merge ( plyr::rename ( subset(a2012,shortname=="house" & number>0 & !is.na(mtm) & mtm>0), c("hhid"="hhid2012", "mtm"="housemtm"))[,c("hhid2012","housemtm")], ss, by=c("hhid2012"))
    ss2$housemtm <- as.integer(log(ss2$housemtm))
  } else {
    ss2 <- subset(merge(plyr::rename(o2012,c("hhid"="hhid2012"))[,c("hhid2012","region",color_field)], ss), !is.na(region))
  }
  plot(ss2$X,ss2$Y,col=ss2[,color_field]+coloffset)
}
get_ownership_change_breakdown_from_plain_assets_res <- function(res)
{
  asset_groups<- unique(as.character(get_asset_group()$asset_group))
  r <- data.frame()
  for (ag in asset_groups){
    r <- rbind(r, data.frame ( asset_group = ag, lost_to_0 = nrow(subset(res[["s"]], asset_group == ag  & number.2012>0 & number.2014 == 0)), 
                               lost_to_nonzero = nrow(subset(res[["s"]], asset_group == ag  & number.2012>0 & number.2014 > 0 ) ),
                               gained_from_zero = nrow(subset(res[["s"]], asset_group == ag  & number.2012==0 & number.2014 > 0 ) ),
                               kept_same_nzcount = nrow(subset(res[["s"]], asset_group == ag  & number.2012>0 & delta==0 ) )
    ) )
    
    
  }
  return(r)
}

plot_costs_from_plain_asset_result <- function(sname,res,is_mtm){
  par(mfrow=c(2,1))
  if(is_mtm){
    p0 <- subset(res[["dat0"]],shortname==sname & !is.na(mtm) & mtm>0)$mtm
    p1 <- subset(res[["dat1"]],shortname==sname & !is.na(mtm) & mtm>0)$mtm
    
  } else {
    p0 <- (subset(res[["dat0"]],shortname==sname & !is.na(cost) & cost>0)$cost )
    p1 <- (subset(res[["dat1"]],shortname==sname & !is.na(cost) & cost>0)$cost )
  }
  hist( p0, breaks=20)
  hist( p1, breaks=20 )
  # pdat = res[["c0"]] ; pdat <- pdat[order(pdat$median_cost),]; par(mar=c(10,2,2,2)) ; barplot(pdat$median_cost, names.arg=as.character(pdat$shortname), las=2)
  #boxplot(data=subset(res[["dat1"]],!is.element(shortname,c('house')))[,c("shortname","mtm")],mtm~shortname,horizontal=F, las=2)
}


minimum_needs_cost_per_head <- function(ll, c2010, c2012, c2014, o2010, o2012, o2014, mktprices2010,mktprices2012,mktprices2014, housing_fee){
  # provide a mapping - per region per district i.e. (region,district,characteristic) -> cost of per-head need per year
  #food - (protein, carb, fat, fruitsveg)
  #1200/1500 kcal
  #carbs - 250g 50% i.e. 600/750 kcal  (250g)
  #protein - 50g per day
  #veg - 1/2 volume of carbs - 500g
  #fat - 50g per day
  
  #TODO: graphs and table
  #graph 1. assets in descreasing order of occurrence frequency (done)
  #graph 2. region-dependency and hsize-depndency on rent (most renters are in 7 - occupation and education rank have little effect)
  #graph 3: variation in rent for houses by regions 
  
  DAYS_IN_YEAR<- 365
  MONTHS_IN_YEAR <- 12
  
  if (missing(c2010)){
    c2010 <- ll@load_diary_file(dirprefix = "../",year = 2010, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  }
  if (missing(c2012)){
    c2012 <- ll@load_diary_file(dirprefix = "../",year = 2012, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  }
  if (missing(c2014)){
    c2014 <- ll@load_diary_file(dirprefix = "../",year = 2014, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  }
  if (missing(o2010)){
    o2010 <- ll@load_ohs_file(year = 2010, dirprefix = "../",fu=fu, ln=lsms_normalizer)
  }
  if (missing(o2012)){
    o2012 <- ll@load_ohs_file(year = 2012, dirprefix = "../",fu=fu, ln=lsms_normalizer)
  }
  if (missing(o2014)){
    o2014 <- ll@load_ohs_file(year = 2014, dirprefix = "../",fu=fu, ln=lsms_normalizer)
  }
  
  fooddiarydata2010      <- subset(c2010,as.integer(as.character(item))>10000)
  fooddiarydata2012      <- subset(c2012,as.integer(as.character(item))>10000)
  fooddiarydata2014      <- subset(c2014,as.integer(as.character(item))>10000)
  
  if (missing(mktprices2010)){
    mktprices2010 <- ll@load_market_prices(year = 2010, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  }
  if (missing(mktprices2012)){
    mktprices2012 <- ll@load_market_prices(year = 2012, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  }
  if (missing(mktprices2014)){
    mktprices2014 <- ll@load_market_prices(year = 2014, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  }
  
  hhp2010 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2010,ohsdata=o2010,ddata=fooddiarydata2010)
  hhp2010 <- merge(lsms_normalizer()@categories_needs_based(),hhp2010)
  regionfoodprice2010 <- hhp2010 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2010 <- ddply( unique(regionfoodprice2010[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2010 <- ddply(basket_constituent_costs2010, .(region,district), summarise, basket_cost = sum(rec_cost)) 
  #regionfoodbasketcosts2010 <- subset(ddply(basket_costs2010,.(region),summarise,basket_cost = min(basket_cost)),!is.na(region))
  #barplot(regionfoodbasketcosts2010$basket_cost,names.arg = regionfoodbasketcosts2010$region, las=2 , xlab= "region" , ylab="cost of food basket" , main="Basket costs across regions (2010)")
  
  hhp2012 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2012,ohsdata=o2012,ddata=fooddiarydata2012)
  hhp2012 <- merge(lsms_normalizer()@categories_needs_based(),hhp2012)
  regionfoodprice2012 <- hhp2012 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2012 <- ddply( unique(regionfoodprice2012[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2012 <- ddply(basket_constituent_costs2012, .(region,district), summarise, basket_cost = sum(rec_cost)) 
  #regionfoodbasketcosts2012 <- subset(ddply(basket_costs2012,.(region),summarise,basket_cost = min(basket_cost)),!is.na(region))
  
  hhp2014 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2014,ohsdata=o2014,ddata=fooddiarydata2014)
  hhp2014 <- merge(lsms_normalizer()@categories_needs_based(),hhp2014)
  
  regionfoodprice2014 <- hhp2014 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2014 <- ddply( unique(regionfoodprice2014[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2014 <- ddply(basket_constituent_costs2014, .(region,district), summarise, basket_cost = sum(rec_cost)) 
  #regionfoodbasketcosts2014 <- subset(ddply(basket_costs2014,.(region),summarise,basket_cost = min(basket_cost)),!is.na(region))
  
  #energy - load cheapest energy prices
  groups <- lsms_normalizer()@categories_needs_based()
  energy_sources <- c("kerosene","electricity","charcoal","petrol")
  
  miscdiarydata2010  <- subset(c2010,is.element(shortname,subset(groups , category =="energy")$shortname))
  hhpm2010       <- ll@add_market_price_to_misc_diary (curyear = 2010, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2010,ohsdata=o2010,ddata=miscdiarydata2010)
  if (setequal(unique(paste(subset(hhpm2010, shortname=="kerosene")$region,subset(hhpm2010, shortname=="kerosene")$district)), unique(paste(hhpm2010$region,hhpm2010$district)))==FALSE){
    stop("Kerosene not available in all regions")
  }
  
  energy_prices2010 <- merge(groups,subset(hhpm2010, is.element(shortname,energy_sources)))
  
  ###
  miscdiarydata2012  <- subset(c2012,is.element(shortname,subset(groups , category =="energy")$shortname))
  
  hhpm2012       <- ll@add_market_price_to_misc_diary (curyear = 2012, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2012,ohsdata=o2012,ddata=miscdiarydata2012)
  if (setequal(unique(paste(subset(hhpm2012, shortname=="kerosene")$region,subset(hhpm2012, shortname=="kerosene")$district)), unique(paste(hhpm2012$region,hhpm2012$district)))==FALSE){
    stop("Kerosene not available in all regions")
  }
  
  energy_prices2012 <- merge(groups,subset(hhpm2012, is.element(shortname,energy_sources)))
  
  ####
  
  miscdiarydata2014  <- subset(c2014,is.element(shortname,subset(groups , category =="energy")$shortname))
  hhpm2014       <- ll@add_market_price_to_misc_diary (curyear = 2014, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2014,ohsdata=o2014,ddata=miscdiarydata2014)
  if (setequal(unique(paste(subset(hhpm2014, shortname=="kerosene")$region,subset(hhpm2014, shortname=="kerosene")$district)), unique(paste(hhpm2014$region,hhpm2014$district)))==FALSE){
    print("Kerosene not available in all regions")
    #median(subset(mktprices2014, shortname=="kerosene")$median_price)
  }
  
  energy_prices2014 <- merge(groups,subset(hhpm2014, is.element(shortname,energy_sources)))
  
  #recq from normaliser reports recq for  every asset level (e.g. for  computer,refrigerator etc.). These levels are matched with those generated from the asset ownership data
  
  #min_energy_prices <- energy_prices2010 [ ,c("shortname","category","region","district","recq","price","assetlevel" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  a2010 <- ll@read_assets_file(year = 2010, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  a2012 <- ll@read_assets_file(year = 2012, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  a2014 <- ll@read_assets_file(year = 2014, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  # assume that cheapest  option (kerosense) for lighting and cooking
  assumed2010     <- assume_assets(o2010)
  
  # 
  assetlevels2010 <- merge(subset(a2010, number>0), asset_levels_for_name())[,c("hhid","assetlevel")]
  assetlevels2010net <- rbind(assetlevels2010,assumed2010)
  energybasketconstituents2010 <- merge(assetlevels2010net,energy_prices2010 ) %>% mutate(rec_cost = DAYS_IN_YEAR * price * recq)
  energybasket2010 <- ddply(energybasketconstituents2010, .(hhid), summarise, basket_cost = sum(rec_cost)) 
  pubtransport2010 <- plyr::rename(unique(subset(c2010, shortname=="public_transport")[,c("hhid","cost")]),c("cost"="pubtrans.cost"))
  energybasket2010 <- merge(pubtransport2010,energybasket2010,by=c("hhid"),all.y=T) 
  energybasket2010[is.na(energybasket2010$pubtrans.cost),]$pubtrans.cost <- 0 
  energybasket2010 <- energybasket2010 %>% mutate ( basket_cost = basket_cost + pubtrans.cost)
  
  assumed2012     <- assume_assets(o2012)
  assetlevels2012 <- merge(subset(a2012, number>0), asset_levels_for_name())[,c("hhid","assetlevel")]
  assetlevels2012net <- rbind(assetlevels2012,assumed2012)
  energybasketconstituents2012 <- merge(assetlevels2012net,energy_prices2012 ) %>% mutate(rec_cost = DAYS_IN_YEAR * price * recq)
  energybasket2012 <- ddply(energybasketconstituents2012, .(hhid), summarise, basket_cost = sum(rec_cost)) 
  pubtransport2012 <- plyr::rename(unique(subset(c2012, shortname=="public_transport")[,c("hhid","cost")]),c("cost"="pubtrans.cost"))
  energybasket2012 <- merge(pubtransport2012,energybasket2012,by=c("hhid"),all.y=T) 
  energybasket2012[is.na(energybasket2012$pubtrans.cost),]$pubtrans.cost <- 0 
  energybasket2012 <- energybasket2012 %>% mutate ( basket_cost = basket_cost + pubtrans.cost)
  
  assumed2014     <- assume_assets(o2014)
  assetlevels2014 <- merge(subset(a2014, number>0), asset_levels_for_name())[,c("hhid","assetlevel")]
  assetlevels2014net <- rbind(assetlevels2014,assumed2014)
  energybasketconstituents2014 <- merge(assetlevels2014net,energy_prices2014 ) %>% mutate(rec_cost = DAYS_IN_YEAR * price * recq)
  energybasket2014 <- ddply(energybasketconstituents2014, .(hhid), summarise, basket_cost = sum(rec_cost)) 
  pubtransport2014 <- plyr::rename(unique(subset(c2014, shortname=="public_transport")[,c("hhid","cost")]),c("cost"="pubtrans.cost"))
  energybasket2014 <- merge(pubtransport2014,energybasket2014,by=c("hhid"),all.y=T) 
  energybasket2014[is.na(energybasket2014$pubtrans.cost),]$pubtrans.cost <- 0 
  energybasket2014 <- energybasket2014 %>% mutate ( basket_cost = basket_cost + pubtrans.cost)
  
  #household needs: mensclothes, womensclothes, childrensclothes, mensshoes, womensshoes, childrensshoes and rent 
  clothing <- get_local_clothing_expenditure(o2012 = o2012, c2012 = c2012, o2014 = o2014, c2014 = c2014, ld = ldat())
  
  #The following can compare how housing maintenance costs are affected by change in field
  #ah2010 <- analyse_house_maintenance_2010(c2010 = c2010, o2010 = o2010)
  #ah2012 <- analyse_house_maintenance_2012_2014(cdat = c2012, odat = o2012)
  #ah2014 <- analyse_house_maintenance_2012_2014(cdat = c2014, odat = o2014)
  #compare2010 <- merge ( plyr::rename(merge(ah2012, unique(o2012[,c("hhid2010","hhid")]),by="hhid"), c("med_maint"="med_maint_2012" ))[,c("hhid2010","med_maint_2012")] , plyr::rename(ah2010, c("med_maint"="med_maint_2010" , "hhid"="hhid2010")) , by = c("hhid2010"))
  
  housing_costs <- assign_house_maintenance(a2010 = a2010, a2012 = a2012, a2014 = a2014, o2010 = o2010, o2012 = o2012, o2014 = o2014, truncateval = 1e+7, fee=housing_fee)
  
  hc2010 <- as.data.frame(housing_costs["hc2010"]) %>% mutate( hc2010.running_cost = MONTHS_IN_YEAR * hc2010.running_cost)
  hc2012 <- as.data.frame(housing_costs["hc2012"]) %>% mutate( hc2012.running_cost = MONTHS_IN_YEAR * hc2012.running_cost)
  hc2014 <- as.data.frame(housing_costs["hc2014"]) %>% mutate( hc2014.running_cost = MONTHS_IN_YEAR * hc2014.running_cost)
  
  basket_costs2010       <- plyr::rename(basket_costs2010,c("basket_cost"="foodbasket_cost"))
  hsize2010              <- unique(merge(o2010[,c("hhid","region","district")], ll@get_hsize(o2010), by = c("hhid")))
  foodbasket2010 <- merge(hsize2010,basket_costs2010, by = c("region","district")) %>% mutate(foodbasket_cost = consu*foodbasket_cost) 
  foodbasket2010 <- foodbasket2010[ ,c("hhid","region","district","foodbasket_cost")]  
  #foodbasket2010   <- merge(basket_costs2010, unique(o2010[,c("hhid","region","district")]), by = c("region","district"))
  energybasket2010 <- plyr::rename(energybasket2010, c("basket_cost"="energybasket_cost"))
  hc2010           <- plyr::rename(hc2010, c("hc2010.running_cost"="housing_cost", "hc2010.hhid2010"="hhid"))
  clothing2010     <- plyr::rename(merge(clothing, unique(o2010[,c("hhid","region")]), by = c("region")) [ ,c("hhid","avcost2010")], c("avcost2010"="clothingcost"))
  allcosts2010     <- merge(clothing2010, merge(foodbasket2010,merge(hc2010, energybasket2010, by = c("hhid")), by=c("hhid")),by=c("hhid") ) %>% mutate (needs_cost = foodbasket_cost + housing_cost + clothingcost + energybasket_cost)
  
  
  basket_costs2012 <- plyr::rename(basket_costs2012,c("basket_cost"="foodbasket_cost"))
  hsize2012        <- unique(merge(o2012[,c("hhid","region","district")], ll@get_hsize(o2012), by = c("hhid")))
  foodbasket2012   <- merge(hsize2012,basket_costs2012, by = c("region","district")) %>% mutate(foodbasket_cost = consu*foodbasket_cost)
  foodbasket2012   <- foodbasket2012[ ,c("hhid","region","district","foodbasket_cost")]
  
  #foodbasket2012   <- merge(basket_costs2012, unique(o2012[,c("hhid","region","district")]), by = c("region","district"))
  energybasket2012 <- plyr::rename(energybasket2012, c("basket_cost"="energybasket_cost"))
  hc2012           <- plyr::rename(hc2012, c("hc2012.running_cost"="housing_cost", "hc2012.hhid"="hhid"))
  clothing2012     <- plyr::rename(merge(clothing, unique(o2012[,c("hhid","region")]), by = c("region")) [ ,c("hhid","avcost2012")], c("avcost2012"="clothingcost"))
  allcosts2012     <- merge(clothing2012, merge(foodbasket2012,merge(hc2012, energybasket2012, by = c("hhid")), by=c("hhid")), by=c("hhid"))  %>% mutate (needs_cost = foodbasket_cost + housing_cost + clothingcost + energybasket_cost)
  
  
  basket_costs2014 <- plyr::rename(basket_costs2014,c("basket_cost"="foodbasket_cost"))
  hsize2014        <- unique(merge(o2014[,c("hhid","region","district")], ll@get_hsize(o2014), by = c("hhid")))
  foodbasket2014   <- merge(hsize2014,basket_costs2014, by = c("region","district")) %>% mutate(foodbasket_cost = consu*foodbasket_cost)
  foodbasket2014   <- foodbasket2014[ ,c("hhid","region","district","foodbasket_cost")]
  
  #foodbasket2014   <- merge(basket_costs2014, unique(o2014[,c("hhid","region","district")]), by = c("region","district"))
  energybasket2014 <- plyr::rename(energybasket2014, c("basket_cost"="energybasket_cost"))
  hc2014           <- plyr::rename(hc2014, c("hc2014.running_cost"="housing_cost", "hc2014.hhid"="hhid"))
  clothing2014     <- plyr::rename(merge(clothing, unique(o2014[,c("hhid","region")]), by = c("region")) [ ,c("hhid","avcost2014")], c("avcost2014"="clothingcost"))
  allcosts2014     <- merge(clothing2014, merge(foodbasket2014,merge(hc2014, energybasket2014, by = c("hhid")), by=c("hhid")), by = c("hhid")) %>% mutate (needs_cost = foodbasket_cost + housing_cost + clothingcost + energybasket_cost)
  
  select_cols      <- c("hhid","needs_cost")
  r                <- data.frame()
  r                <- rbind(r, allcosts2010[,select_cols] %>% mutate(year=2010))
  r                <- rbind(r, allcosts2012[,select_cols] %>% mutate(year=2012)) 
  r                <- rbind(r, allcosts2014[,select_cols] %>% mutate(year=2014))
  
  res              <- list()
  res[["df"]]      <- r
  res[["df2010"]]  <- allcosts2010
  res[["df2012"]]  <- allcosts2012
  res[["df2014"]]  <- allcosts2014
  
  return(res) 
  
  # transport - load petrol prices and load public transport prices
  # household - rent and clothes
  
  # sum up all the costs - this total cost should be seen as p(A_{t-1},\rho) x needs_cost
  
  
}



needs_of_household <- function(ll, c2010, c2012, c2014, o2010, o2012, o2014, mktprices2010,mktprices2012,mktprices2014, housing_fee){
  # provide a mapping - per region per district i.e. (region,district,characteristic) -> cost of per-head need per year
  #food - (protein, carb, fat, fruitsveg)
  #1200/1500 kcal
  #carbs - 250g 50% i.e. 600/750 kcal  (250g)
  #protein - 50g per day
  #veg - 1/2 volume of carbs - 500g
  #fat - 50g per day
  
  #TODO: graphs and table
  #graph 1. assets in descreasing order of occurrence frequency (done)
  #graph 2. region-dependency and hsize-depndency on rent (most renters are in 7 - occupation and education rank have little effect)
  #greph 3: variation in rent for houses by regions 
  
  if (missing(c2010)){
    c2010 <- ll@load_diary_file(dirprefix = "../",year = 2010, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  }
  if (missing(c2012)){
    c2012 <- ll@load_diary_file(dirprefix = "../",year = 2012, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  }
  if (missing(c2014)){
    c2014 <- ll@load_diary_file(dirprefix = "../",year = 2014, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  }
  if (missing(o2010)){
    o2010 <- ll@load_ohs_file(year = 2010, dirprefix = "../",fu=fu, ln=lsms_normalizer)
  }
  if (missing(o2012)){
    o2012 <- ll@load_ohs_file(year = 2012, dirprefix = "../",fu=fu, ln=lsms_normalizer)
  }
  if (missing(o2014)){
    o2014 <- ll@load_ohs_file(year = 2014, dirprefix = "../",fu=fu, ln=lsms_normalizer)
  }
  
  fooddiarydata2010      <- subset(c2010,as.integer(as.character(item))>10000)
  fooddiarydata2012      <- subset(c2012,as.integer(as.character(item))>10000)
  fooddiarydata2014      <- subset(c2014,as.integer(as.character(item))>10000)
  
  ### HERE, the expenditure would be just replaced with minimum cost item - for food
  # For energy, we consider that the owner of gadgets would have their bills counting as needs as well - so we do nothing.
  # For transport, public transport is not extrapolated - it's used only when it's available. Those who have cars can have petrol and that becomes needs too.
  # For housing, the owner of the house faces no cost but the renter would - renters are probably in a different setting (urban).
  
  if (missing(mktprices2010)){
    mktprices2010 <- ll@load_market_prices(year = 2010, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  }
  if (missing(mktprices2012)){
    mktprices2012 <- ll@load_market_prices(year = 2012, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  }
  if (missing(mktprices2014)){
    mktprices2014 <- ll@load_market_prices(year = 2014, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  }
  
  hhp2010 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2010,ohsdata=o2010,ddata=fooddiarydata2010)
  hhp2010 <- merge(lsms_normalizer()@categories_needs_based(),hhp2010)
  regionfoodprice2010 <- hhp2010 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2010 <- ddply( unique(regionfoodprice2010[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2010 <- ddply(basket_constituent_costs2010, .(region,district), summarise, basket_cost = sum(rec_cost)) 
  #regionfoodbasketcosts2010 <- subset(ddply(basket_costs2010,.(region),summarise,basket_cost = min(basket_cost)),!is.na(region))
  #barplot(regionfoodbasketcosts2010$basket_cost,names.arg = regionfoodbasketcosts2010$region, las=2 , xlab= "region" , ylab="cost of food basket" , main="Basket costs across regions (2010)")
  
  hhp2012 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2012,ohsdata=o2012,ddata=fooddiarydata2012)
  hhp2012 <- merge(lsms_normalizer()@categories_needs_based(),hhp2012)
  regionfoodprice2012 <- hhp2012 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2012 <- ddply( unique(regionfoodprice2012[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2012 <- ddply(basket_constituent_costs2012, .(region,district), summarise, basket_cost = sum(rec_cost)) 
  #regionfoodbasketcosts2012 <- subset(ddply(basket_costs2012,.(region),summarise,basket_cost = min(basket_cost)),!is.na(region))
  
  hhp2014 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2014,ohsdata=o2014,ddata=fooddiarydata2014)
  hhp2014 <- merge(lsms_normalizer()@categories_needs_based(),hhp2014)
  
  regionfoodprice2014 <- hhp2014 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2014 <- ddply( unique(regionfoodprice2014[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price*DAYS_IN_YEAR) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2014 <- ddply(basket_constituent_costs2014, .(region,district), summarise, basket_cost = sum(rec_cost)) 
  #regionfoodbasketcosts2014 <- subset(ddply(basket_costs2014,.(region),summarise,basket_cost = min(basket_cost)),!is.na(region))
  
  #energy - load cheapest energy prices
  groups <- lsms_normalizer()@categories_needs_based()
  energy_sources <- c("kerosene","electricity","charcoal","petrol")
  
  miscdiarydata2010  <- subset(c2010,is.element(shortname,subset(groups , category =="energy")$shortname))
  hhpm2010       <- ll@add_market_price_to_misc_diary (curyear = 2010, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2010,ohsdata=o2010,ddata=miscdiarydata2010)
  if (setequal(unique(paste(subset(hhpm2010, shortname=="kerosene")$region,subset(hhpm2010, shortname=="kerosene")$district)), unique(paste(hhpm2010$region,hhpm2010$district)))==FALSE){
    stop("Kerosene not available in all regions")
  }
  
  energy_prices2010 <- merge(groups,subset(hhpm2010, is.element(shortname,energy_sources)))
  
  ###
  miscdiarydata2012  <- subset(c2012,is.element(shortname,subset(groups , category =="energy")$shortname))
  
  hhpm2012       <- ll@add_market_price_to_misc_diary (curyear = 2012, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2012,ohsdata=o2012,ddata=miscdiarydata2012)
  if (setequal(unique(paste(subset(hhpm2012, shortname=="kerosene")$region,subset(hhpm2012, shortname=="kerosene")$district)), unique(paste(hhpm2012$region,hhpm2012$district)))==FALSE){
    stop("Kerosene not available in all regions")
  }
  
  energy_prices2012 <- merge(groups,subset(hhpm2012, is.element(shortname,energy_sources)))
  
  ####
  
  miscdiarydata2014  <- subset(c2014,is.element(shortname,subset(groups , category =="energy")$shortname))
  hhpm2014       <- ll@add_market_price_to_misc_diary (curyear = 2014, dirprefix ="../", fu=fu, ln=lsms_normalizer, groups = groups, lgc=lgc,
                                                       ld = ld, marketpricesdata=mktprices2014,ohsdata=o2014,ddata=miscdiarydata2014)
  if (setequal(unique(paste(subset(hhpm2014, shortname=="kerosene")$region,subset(hhpm2014, shortname=="kerosene")$district)), unique(paste(hhpm2014$region,hhpm2014$district)))==FALSE){
    print("Kerosene not available in all regions")
    #median(subset(mktprices2014, shortname=="kerosene")$median_price)
  }
  
  energy_prices2014 <- merge(groups,subset(hhpm2014, is.element(shortname,energy_sources)))
  
  #recq from normaliser reports recq for  every asset level (e.g. for  computer,refrigerator etc.). These levels are matched with those generated from the asset ownership data
  
  #min_energy_prices <- energy_prices2010 [ ,c("shortname","category","region","district","recq","price","assetlevel" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  a2010 <- ll@read_assets_file(year = 2010, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  a2012 <- ll@read_assets_file(year = 2012, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  a2014 <- ll@read_assets_file(year = 2014, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  # assume that cheapest  option (kerosense) for lighting and cooking
  assumed2010     <- assume_assets(o2010)
  
  # 
  assetlevels2010 <- merge(subset(a2010, number>0), asset_levels_for_name())[,c("hhid","assetlevel")]
  assetlevels2010net <- rbind(assetlevels2010,assumed2010)
  energybasketconstituents2010 <- merge(assetlevels2010net,energy_prices2010 ) %>% mutate(rec_cost = DAYS_IN_YEAR * price * recq)
  energybasket2010 <- ddply(energybasketconstituents2010, .(hhid), summarise, basket_cost = sum(rec_cost)) 
  pubtransport2010 <- plyr::rename(unique(subset(c2010, shortname=="public_transport")[,c("hhid","cost")]),c("cost"="pubtrans.cost"))
  energybasket2010 <- merge(pubtransport2010,energybasket2010,by=c("hhid"),all.y=T) 
  energybasket2010[is.na(energybasket2010$pubtrans.cost),]$pubtrans.cost <- 0 
  energybasket2010 <- energybasket2010 %>% mutate ( basket_cost = basket_cost + pubtrans.cost)
  
  assumed2012     <- assume_assets(o2012)
  assetlevels2012 <- merge(subset(a2012, number>0), asset_levels_for_name())[,c("hhid","assetlevel")]
  assetlevels2012net <- rbind(assetlevels2012,assumed2012)
  energybasketconstituents2012 <- merge(assetlevels2012net,energy_prices2012 ) %>% mutate(rec_cost = DAYS_IN_YEAR * price * recq)
  energybasket2012 <- ddply(energybasketconstituents2012, .(hhid), summarise, basket_cost = sum(rec_cost)) 
  pubtransport2012 <- plyr::rename(unique(subset(c2012, shortname=="public_transport")[,c("hhid","cost")]),c("cost"="pubtrans.cost"))
  energybasket2012 <- merge(pubtransport2012,energybasket2012,by=c("hhid"),all.y=T) 
  energybasket2012[is.na(energybasket2012$pubtrans.cost),]$pubtrans.cost <- 0 
  energybasket2012 <- energybasket2012 %>% mutate ( basket_cost = basket_cost + pubtrans.cost)
  
  assumed2014     <- assume_assets(o2014)
  assetlevels2014 <- merge(subset(a2014, number>0), asset_levels_for_name())[,c("hhid","assetlevel")]
  assetlevels2014net <- rbind(assetlevels2014,assumed2014)
  energybasketconstituents2014 <- merge(assetlevels2014net,energy_prices2014 ) %>% mutate(rec_cost = DAYS_IN_YEAR * price * recq)
  energybasket2014 <- ddply(energybasketconstituents2014, .(hhid), summarise, basket_cost = sum(rec_cost)) 
  pubtransport2014 <- plyr::rename(unique(subset(c2014, shortname=="public_transport")[,c("hhid","cost")]),c("cost"="pubtrans.cost"))
  energybasket2014 <- merge(pubtransport2014,energybasket2014,by=c("hhid"),all.y=T) 
  energybasket2014[is.na(energybasket2014$pubtrans.cost),]$pubtrans.cost <- 0 
  energybasket2014 <- energybasket2014 %>% mutate ( basket_cost = basket_cost + pubtrans.cost)
  
  #household needs: mensclothes, womensclothes, childrensclothes, mensshoes, womensshoes, childrensshoes and rent 
  clothing <- get_local_clothing_expenditure(o2012 = o2012, c2012 = c2012, o2014 = o2014, c2014 = c2014, ld = ldat())
  
  #The following can compare how housing maintenance costs are affected by change in field
  #ah2010 <- analyse_house_maintenance_2010(c2010 = c2010, o2010 = o2010)
  #ah2012 <- analyse_house_maintenance_2012_2014(cdat = c2012, odat = o2012)
  #ah2014 <- analyse_house_maintenance_2012_2014(cdat = c2014, odat = o2014)
  #compare2010 <- merge ( plyr::rename(merge(ah2012, unique(o2012[,c("hhid2010","hhid")]),by="hhid"), c("med_maint"="med_maint_2012" ))[,c("hhid2010","med_maint_2012")] , plyr::rename(ah2010, c("med_maint"="med_maint_2010" , "hhid"="hhid2010")) , by = c("hhid2010"))
  
  housing_costs <- assign_house_maintenance(a2010 = a2010, a2012 = a2012, a2014 = a2014, o2010 = o2010, o2012 = o2012, o2014 = o2014, truncateval = 1e+7, fee=housing_fee)
  
  hc2010 <- as.data.frame(housing_costs["hc2010"]) %>% mutate( hc2010.running_cost = MONTHS_IN_YEAR * hc2010.running_cost)
  hc2012 <- as.data.frame(housing_costs["hc2012"]) %>% mutate( hc2012.running_cost = MONTHS_IN_YEAR * hc2012.running_cost)
  hc2014 <- as.data.frame(housing_costs["hc2014"]) %>% mutate( hc2014.running_cost = MONTHS_IN_YEAR * hc2014.running_cost)
  
  basket_costs2010       <- plyr::rename(basket_costs2010,c("basket_cost"="foodbasket_cost"))
  hsize2010              <- unique(merge(o2010[,c("hhid","region","district")], ll@get_hsize(o2010), by = c("hhid")))
  foodbasket2010 <- merge(hsize2010,basket_costs2010, by = c("region","district")) %>% mutate(foodbasket_cost = consu*foodbasket_cost) 
  foodbasket2010 <- foodbasket2010[ ,c("hhid","region","district","foodbasket_cost")]  
  #foodbasket2010   <- merge(basket_costs2010, unique(o2010[,c("hhid","region","district")]), by = c("region","district"))
  energybasket2010 <- plyr::rename(energybasket2010, c("basket_cost"="energybasket_cost"))
  hc2010           <- plyr::rename(hc2010, c("hc2010.running_cost"="housing_cost", "hc2010.hhid2010"="hhid"))
  clothing2010     <- plyr::rename(merge(clothing, unique(o2010[,c("hhid","region")]), by = c("region")) [ ,c("hhid","avcost2010")], c("avcost2010"="clothingcost"))
  allcosts2010     <- merge(clothing2010, merge(foodbasket2010,merge(hc2010, energybasket2010, by = c("hhid")), by=c("hhid")),by=c("hhid") ) %>% mutate (needs_cost = foodbasket_cost + housing_cost + clothingcost + energybasket_cost)
  
  
  basket_costs2012 <- plyr::rename(basket_costs2012,c("basket_cost"="foodbasket_cost"))
  hsize2012        <- unique(merge(o2012[,c("hhid","region","district")], ll@get_hsize(o2012), by = c("hhid")))
  foodbasket2012   <- merge(hsize2012,basket_costs2012, by = c("region","district")) %>% mutate(foodbasket_cost = consu*foodbasket_cost)
  foodbasket2012   <- foodbasket2012[ ,c("hhid","region","district","foodbasket_cost")]
  
  #foodbasket2012   <- merge(basket_costs2012, unique(o2012[,c("hhid","region","district")]), by = c("region","district"))
  energybasket2012 <- plyr::rename(energybasket2012, c("basket_cost"="energybasket_cost"))
  hc2012           <- plyr::rename(hc2012, c("hc2012.running_cost"="housing_cost", "hc2012.hhid"="hhid"))
  clothing2012     <- plyr::rename(merge(clothing, unique(o2012[,c("hhid","region")]), by = c("region")) [ ,c("hhid","avcost2012")], c("avcost2012"="clothingcost"))
  allcosts2012     <- merge(clothing2012, merge(foodbasket2012,merge(hc2012, energybasket2012, by = c("hhid")), by=c("hhid")), by=c("hhid"))  %>% mutate (needs_cost = foodbasket_cost + housing_cost + clothingcost + energybasket_cost)
  
  
  basket_costs2014 <- plyr::rename(basket_costs2014,c("basket_cost"="foodbasket_cost"))
  hsize2014        <- unique(merge(o2014[,c("hhid","region","district")], ll@get_hsize(o2014), by = c("hhid")))
  foodbasket2014   <- merge(hsize2014,basket_costs2014, by = c("region","district")) %>% mutate(foodbasket_cost = consu*foodbasket_cost)
  foodbasket2014   <- foodbasket2014[ ,c("hhid","region","district","foodbasket_cost")]
  
  #foodbasket2014   <- merge(basket_costs2014, unique(o2014[,c("hhid","region","district")]), by = c("region","district"))
  energybasket2014 <- plyr::rename(energybasket2014, c("basket_cost"="energybasket_cost"))
  hc2014           <- plyr::rename(hc2014, c("hc2014.running_cost"="housing_cost", "hc2014.hhid"="hhid"))
  clothing2014     <- plyr::rename(merge(clothing, unique(o2014[,c("hhid","region")]), by = c("region")) [ ,c("hhid","avcost2014")], c("avcost2014"="clothingcost"))
  allcosts2014     <- merge(clothing2014, merge(foodbasket2014,merge(hc2014, energybasket2014, by = c("hhid")), by=c("hhid")), by = c("hhid")) %>% mutate (needs_cost = foodbasket_cost + housing_cost + clothingcost + energybasket_cost)
  
  select_cols      <- c("hhid","needs_cost")
  r                <- data.frame()
  r                <- rbind(r, allcosts2010[,select_cols] %>% mutate(year=2010))
  r                <- rbind(r, allcosts2012[,select_cols] %>% mutate(year=2012)) 
  r                <- rbind(r, allcosts2014[,select_cols] %>% mutate(year=2014))
  
  res              <- list()
  res[["df"]]      <- r
  res[["df2010"]]  <- allcosts2010
  res[["df2012"]]  <- allcosts2012
  res[["df2014"]]  <- allcosts2014
  
  return(res) 
  
}


plot_needs <- function(e,year){
  par(mfrow=c(2,2))
  if (year == 2010){  
    hist(e[["df2010"]]$foodbasket_cost)
    hist(e[["df2010"]]$housing_cost)
    hist(e[["df2010"]]$energybasket_cost)
    hist(e[["df2010"]]$clothingcost)
  } 
  if (year == 2012){  
    hist(e[["df2012"]]$foodbasket_cost)
    hist(e[["df2012"]]$housing_cost)
    hist(e[["df2012"]]$energybasket_cost)
    hist(e[["df2012"]]$clothingcost)
  } 
  if (year == 2014){  
    hist(e[["df2014"]]$foodbasket_cost)
    hist(e[["df2014"]]$housing_cost)
    hist(e[["df2014"]]$energybasket_cost)
    hist(e[["df2014"]]$clothingcost)
  }
}
#{
#  data.frame(code=1,categ = "kerosene_lighting")
#  data.frame(code=2,categ = "kerosene_cooking")
#  data.frame(code=3,categ = "electric_lighting")
#  data.frame(code=4,categ = "electric_cooking")
#}
assume_assets_stove_based <- function(adat){
  has_electric_stove <- merge(data.frame(hhid=unique(adat$hhid), dummy=1), subset(adat,number>0 & shortname=="stove_electricgas")[,c("hhid","number")],all.x=TRUE)
  has_electric_stove$dummy <- NULL
  if (nrow(has_electric_stove[is.na(has_electric_stove$number),])>0){
    has_electric_stove[is.na(has_electric_stove$number),]$number <- 0
  }
  #hard to find somebody who would have an electric stove and use a kerosene lamp
  #"lightingfuel, cookingfuel
  has_electric_stove$lighting <- sapply(has_electric_stove$number, function(x){ if (x>0) {"elec_lighting"}else {"kerosene_lighting"}}) 
  has_electric_stove$cooking <- sapply(has_electric_stove$number, function(x){ if (x>0) {"elec_cooking"}else {"kerosene_cooking"}}) 
  
  a1<- plyr::rename(has_electric_stove[c("hhid","lighting")] %>% gather(hhid,lighting), c("lighting"="assetlevel"))
  a2<- plyr::rename(has_electric_stove[c("hhid","cooking")] %>% gather(hhid,cooking), c("cooking"="assetlevel"))
  res <- rbind(a1,a2)
  return(res)
}

lighting_fuel_mapping <- function(){
  edf <- data.frame()
  edf <- rbind(edf,data.frame(lightingfuel = c(1), lighting=c("elec_lighting")))
  edf <- rbind(edf,data.frame(lightingfuel = c(2), lighting=c("kerosene_lighting")))
  edf <- rbind(edf,data.frame(lightingfuel = c(3), lighting=c("kerosene_lighting")))
  edf <- rbind(edf,data.frame(lightingfuel = c(4), lighting=c("kerosene_lighting")))
  edf <- rbind(edf,data.frame(lightingfuel = c(5), lighting=c("kerosene_lighting")))
  edf <- rbind(edf,data.frame(lightingfuel = c(6), lighting=c("kerosene_lighting")))
  edf <- rbind(edf,data.frame(lightingfuel = c(7), lighting=c("kerosene_lighting")))
  edf <- rbind(edf,data.frame(lightingfuel = c(8), lighting=c("elec_lighting")))
  edf <- rbind(edf,data.frame(lightingfuel = c(9), lighting=c("kerosene_lighting")))
  
  return(edf)
}

cooking_fuel_mapping <- function(){
  
  edf <- data.frame()
  edf <- rbind(edf,data.frame(cookingfuel = c(1), cooking=c("kerosene_cooking")))
  edf <- rbind(edf,data.frame(cookingfuel = c(2), cooking=c("kerosene_cooking")))
  edf <- rbind(edf,data.frame(cookingfuel = c(3), cooking=c("elec_cooking")))
  edf <- rbind(edf,data.frame(cookingfuel = c(4), cooking=c("elec_cooking")))
  edf <- rbind(edf,data.frame(cookingfuel = c(5), cooking=c("charcoal_cooking")))
  edf <- rbind(edf,data.frame(cookingfuel = c(6), cooking=c("kerosene_cooking")))
  edf <- rbind(edf,data.frame(cookingfuel = c(7), cooking=c("kerosene_cooking")))
  edf <- rbind(edf,data.frame(cookingfuel = c(8), cooking=c("kerosene_cooking")))
  
  return(edf)
}

assume_assets <- function(odat){
  #"lightingfuel, cookingfuel
  #has_electric_stove$lighting <- sapply(has_electric_stove$number, function(x){ if (x>0) {"elec_lighting"}else {"kerosene_lighting"}}) 
  #has_electric_stove$cooking <- sapply(has_electric_stove$number, function(x){ if (x>0) {"elec_cooking"}else {"kerosene_cooking"}}) 
  lighting <- merge(subset(unique(odat[,c("hhid","lightingfuel")]),!is.na(lightingfuel) ), lighting_fuel_mapping())
  cooking <- merge(subset(unique(odat[,c("hhid","cookingfuel")]),!is.na(cookingfuel)), cooking_fuel_mapping())
  a1<- plyr::rename(lighting[c("hhid","lighting")] %>% gather(hhid,lighting), c("lighting"="assetlevel"))
  a2<- plyr::rename(cooking[c("hhid","cooking")] %>% gather(hhid,cooking), c("cooking"="assetlevel"))
  res <- rbind(a1,a2)
  return(res)
}


asset_levels_for_name <- function() {
  r <- data.frame()
  r <- rbind(r, data.frame( shortname='refrigerator', assetlevel='elec_fridge'))
  r <- rbind(r, data.frame( shortname='tv', assetlevel='elec_tvvideomusic'))
  r <- rbind(r, data.frame( shortname='videoplayer', assetlevel='elec_tvvideomusic'))
  r <- rbind(r, data.frame( shortname='computer', assetlevel='elec_computer'))
  r <- rbind(r, data.frame( shortname='iron', assetlevel='elec_iron'))
  r <- rbind(r, data.frame( shortname='stove_electricgas', assetlevel='elecgas_cooking'))
  r <- rbind(r, data.frame( shortname='stove_other', assetlevel='kerosene_cooking'))
  r <- rbind(r, data.frame( shortname='waterheater', assetlevel='elec_waterheating'))
  r <- rbind(r, data.frame( shortname='musicplayer', assetlevel='elec_tvvideomusic'))
  r <- rbind(r, data.frame( shortname='musicsystem', assetlevel='elec_tvvideomusic'))
  r <- rbind(r, data.frame( shortname='ac_fan', assetlevel='elec_acfan'))
  r <- rbind(r, data.frame( shortname='car', assetlevel='petrol_car'))
  r <- rbind(r, data.frame( shortname='motorbike', assetlevel='petrol_motorbike'))
  
  return(r)
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

stored_food_cpi <- function(){
  x <- data.frame(year=c(2010), food_cpi=c(100))
  x <- rbind(x,data.frame(year=c(2012), food_cpi=c(142.070554851631)))
  x <- rbind(x,data.frame(year=c(2014), food_cpi=c(135.859046554779)))
  return(x)
}
calculate_food_cpi <- function(c2010,g2010,o2010){
  
  cc <- merge(c2010,g2010[,c("hhid","total_expenditure")],by=c("hhid"))
  cc <- subset(cc,!is.na(cost))
  cc <- cc %>% mutate(w = cost/total_expenditure)
  s <- ddply(cc[,c("hhid","w","shortname")],.(shortname),summarise,wq = quantile(w,0.8))
  
  foodshortnames <- unique(as.character(subset(lsms_normalizer()@lsms_groups_qualitybased_2010_2012(), is.element(category,c("densefoods","nonfresh","fruitsveg","protein","complements")))$shortname))
  s <- subset(s,is.element(shortname,foodshortnames))
  s <- s[rev(order(s$wq)),]
  #
  mktprices2010 <- ll@load_market_prices(year = 2010, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  fooddiarydata2010      <- subset(c2010,as.integer(as.character(item))>10000)
  hhp <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2010,ohsdata=o2010,ddata=fooddiarydata2010)
  
  #1. get mean quantities of items in base year (2010)
  #B      = sum(p_2010*q_2010)
  #P_2012 = sum (p_2012*q_2010)/ B
  #P_2014 = sum (p_2014*q_2010)/ B
  #lnp <- lnp + data[[shareNames[i]]] * log(data[[priceNames[i]]]/basePrices[i])
  
  groups      <- subset( lsms_normalizer()@lsms_groups_qualitybased_2010_2012(), is.element(shortname,foodshortnames)  )
  hhp <- merge(hhp,groups,by=c("shortname"))
  
  minprices <- ddply(hhp[,c("shortname","price","category","region","district")],.(category,region,district),summarise,min_price=min(price))
  hhp <- merge(minprices,hhp)
  hhp$price_ratio <- with (hhp,price/min_price) 
  hhp$factor<-as.integer(hhp$lwp_unit==1)+as.integer(hhp$lwp_unit==2)/1000.0+as.integer(hhp$lwp_unit==3)+as.integer(hhp$lwp_unit==4)/1000.0+as.integer(hhp$lwp_unit==5)
  hhp$quantity <-with (hhp,lwp*factor)
  basket <- ddply(hhp[,c("shortname","region","district","quantity","price")],.(region,district,shortname), summarise, w = mean(quantity) , price2010 = mean(price))
  #totq <- ddply(unique(hhp[,c("hhid","shortname","category","quantity","price_ratio","min_price","cost")]),.(category,hhid),summarise,totq=sum(quantity), 
  #              qsum = sum (price_ratio*quantity), min_price=unique(min_price),tot_categ_exp = sum(cost))
  #totq$quality <- with(totq,qsum/totq)
  #totq <- merge(o2010[,c("region","district","hhid")], totq,by = c("hhid"))
  #res  <- ddply(totq,.(category),summarise,qall = mean(qsum[!is.na(qsum)]), costall = mean(tot_categ_exp[!is.na(tot_categ_exp)]))
  
  c2012         <- ll@load_diary_file(dirprefix = "../",year = 2012, fu = fu, ln = lsms_normalizer)
  o2012         <- ll@load_ohs_file(year = 2012, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  mktprices2012 <- ll@load_market_prices(year = 2012, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  fooddiarydata2012     <- subset(c2012,as.integer(as.character(item))>10000)
  hhp2012 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2012,ohsdata=o2012,ddata=fooddiarydata2012)
  hhp2012 <- merge(hhp2012,groups,by=c("shortname"))
  prices2012 <- plyr::rename(hhp2012[,c("region","district","shortname","price")],c("price"="price2012"))
  basket <- merge(basket,prices2012, by = c("region","district","shortname"))
  
  c2014         <- ll@load_diary_file(dirprefix = "../",year = 2014, fu = fu, ln = lsms_normalizer)
  o2014         <- ll@load_ohs_file(year = 2014, dirprefix = "../",fu = fu, ln = lsms_normalizer)
  mktprices2014 <- ll@load_market_prices(year = 2014, dirprefix = "../",fu = fu , ln = lsms_normalizer, use_pieces = FALSE)
  fooddiarydata2014     <- subset(c2014,as.integer(as.character(item))>10000)
  hhp2014 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2014,ohsdata=o2014,ddata=fooddiarydata2014)
  hhp2014 <- merge(hhp2014,groups,by=c("shortname"))
  prices2014 <- plyr::rename(hhp2014[,c("region","district","shortname","price")],c("price"="price2014"))
  basket <- merge(basket,prices2014, by = c("region","district","shortname"))
  
  basket <- unique(basket) 
  basket <- ddply(basket, .(shortname),summarise, w = mean(w) , price2010 = mean(price2010[!is.na(price2010)]), price2012 = mean(price2012[!is.na(price2012)]),price2014 = mean(price2014[!is.na(price2014)]))
  basket <- basket%>% mutate( cost2012 = price2012*w , cost2014 = price2014*w , cost2010= price2010)
  cpi2012 <- mean(with(basket,(price2012*w)/(price2010*w)))
  cpi2014 <- mean(with(basket,(price2014*w)/(price2010*w)))
  df      <- data.frame( year = c(2010,2012,2014) , cpi = c(100,cpi2012*100, cpi2014*100))
  return(df)
  
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
  prepdat[is.na(prepdat$densefoods_quality),]$densefoods_quality <- 0
  prepdat[is.na(prepdat$nonfresh_quality),]$nonfresh_quality <- 0
  prepdat[is.na(prepdat$fruitsveg_quality),]$fruitsveg_quality <- 0
  prepdat[is.na(prepdat$protein_quality),]$protein_quality <- 0
  prepdat[is.na(prepdat$complements_quality),]$complements_quality <- 0
  
  prepdat[is.na(prepdat$w_densefoods),]$w_densefoods <- 0
  prepdat[is.na(prepdat$w_nonfresh),]$w_nonfresh <- 0
  prepdat[is.na(prepdat$w_fruitsveg),]$w_fruitsveg <- 0
  prepdat[is.na(prepdat$w_protein),]$w_protein <- 0
  prepdat[is.na(prepdat$w_complements),]$w_complements <- 0
  
  #densefoods_quality nonfresh_quality fruitsveg_quality protein_quality complements_quality
  #w_densefoods w_nonfresh w_fruitsveg w_protein w_complements
  prepdat <- prepdat %>% mutate(food_quality=densefoods_quality+nonfresh_quality+fruitsveg_quality+protein_quality+complements_quality)
  prepdat <- prepdat %>% mutate(w_food= w_densefoods+w_nonfresh+w_fruitsveg+w_protein+w_complements)
  prepdat$lnV_food <- with(prepdat,log(food_quality+1))
  
  foodcpilog <- ( stored_food_cpi() %>% mutate(lpfood = log(food_cpi)) ) [,c("year","lpfood")]
  prepdat <- merge(foodcpilog,prepdat,by=c("year"))
  
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

load_group <- function(dat,year,categories){
  if (missing(categories)){
    categories <- get_categories()
  }
  if (missing(dat)){
    dat <- ll@group_expenditure(year = year, dirprefix = "../",
                                fu = fu , ln = lsms_normalizer, lgc=lgc,
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
