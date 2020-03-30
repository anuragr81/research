library(dplyr)
library(moments) # for kurtosis
setwd('c:/local_files/research/consumption/datamapper/')
source('translation/frameutils.R');source('lsms/lsms_normalizer.r');source('lsms/lsms_loader.r');ll=lsms_loader(fu=fu,ln=lsms_normalizer,lgc=lgc)
source('lsms/lsms_group_collect.r'); source('lsms/lsms_datastorage.R')
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
choose_max_education_rank <- function (x) { arr = x[!is.na(x)] ; if (length(arr)>1) {return (max(arr))} else {return(0)}}
mean_of_nonzeros <- function (x) { arr = x[x!=0] ; if (length(arr)>0) {return (mean(arr))} else {return(0)}}

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
  ho2010YOB<- ddply(unique((o2010[,c("hhid2010",combine_cols)])),.(hhid2010),summarise, YOB_array=toJSON(sort(YOB)), max_education_rank = choose_max_education_rank(education_rank) , max_occupation_rank = max(occupation_rank) , region= unique(region), district = unique(district), housingstatus=unique(housingstatus), num_children=unique(num_children), sum_yearly_pay = sum(yearly_pay[!is.na(yearly_pay)]))
  ho2012YOB<- ddply(unique((o2012[,c("hhid2012",combine_cols)])),.(hhid2012),summarise, YOB_array=toJSON(sort(YOB)), max_education_rank = choose_max_education_rank(education_rank), max_occupation_rank = max(occupation_rank) , region= unique(region), district = unique(district), housingstatus=unique(housingstatus), num_children=unique(num_children), sum_yearly_pay = sum(yearly_pay[!is.na(yearly_pay)]))
  ho2014YOB<- ddply(unique((o2014[,c("hhid2014",combine_cols)])),.(hhid2014),summarise, YOB_array=toJSON(sort(YOB)), max_education_rank = choose_max_education_rank(education_rank), max_occupation_rank = max(occupation_rank) , region= unique(region), district = unique(district), housingstatus=unique(housingstatus), num_children=unique(num_children), sum_yearly_pay = sum(yearly_pay[!is.na(yearly_pay)]))
  
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
income_data <- function(ll,yr,dirprefix,fu,ln){
  idat            <- ll@load_income_file(year = yr, dirprefix = "../",fu = fu, ln = ln)
  idat            <- ddply(idat, .(hhid), summarise, totinc = sum(yearly_pay))
  idat            <- subset(idat,!is.na(totinc))
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
  # gcols obtained using: toString(paste("'",colnames(x),"'",sep=""))
  #gcols <- c('hhid', 'total_expenditure', 'toteducexpense', 'tothouserent',  'hsize', 'consu', 'highest_educ', 'age'
  #           , 'expensiveregion', 'popdensity','region','district','litlang',
  #           'isrural', 'isurbanp', 'occupation', 'occupation_rank', 'years_community', 
  #           'housingstatus', 'roomsnum',  'floormaterial', 'cookingfuel', 'is_resident', 'ln_tot_exp', 'year')
  #ag <- unique(allgroupsdat[,gcols])
  #agi <- merge(ag,i,all.x=TRUE,by=c("hhid","year","region", "district"))
  #ydat <- subset(agi,year==yr)
  #for (sn in as.character(unique(ydat$shortname))) { if (!is.na(sn)) { dat <- subset(ydat,shortname==sn); write_dta(dat,paste('c:/temp/dat',yr,'_',sn,'.dta',sep="")) } } 
  #return(agi)
  
  #return(tot)
  assetnames_transport   <- c('bike', 'motorbike', 'car')
  assetnames_household   <- c(  'sewingmachine', 'bed',  'watch',  'chair', 'table', 'cupboard', 'sofa','sports_hobby','land', 'house')
  assetnames_electric     <- c('mobile', 'waterheater','camera', 'phone', 'musicplayer', 'videoplayer', 'musicsystem', 'ac_fan', 'waterpump', 'tv', 'dishtv', 'computer',  'refrigerator' )
  m <- mapping_hhids_2012_2014(o2014)
  a1 <- subset(a2012, hhid=="2432-001" & number >0) [, c("hhid","number","shortname")]
  a1 <- plyr::rename(a1,c("hhid"="hhid2012","number"="number.2012"))
  a2 <- subset(a2014, hhid=="0553-001" & number >0 ) [, c("hhid","number","shortname")]
  a2 <- plyr::rename(a2,c("hhid"="hhid2014", "number"="number.2014"))
  a2 <- merge(m,a2)
  a3 <- merge(a1, a2, all=TRUE, by=c("hhid2012","shortname"))
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

assign_house_maintenance <- function (a2010, a2012, a2014, o2010, o2012, o2014) {
  fee <- .025
  homeownerhouses2010 <- merge ( subset( merge(a2012, unique(o2012[,c("hhid","hhid2010")]) ) , shortname=="house" & number >0) , plyr::rename(a2010,c("hhid"="hhid2010")) )
  
  oh2010   <- ddply(o2010, .(hhid), summarise, houserent = sum(houserent)) [ ,c("hhid","houserent")]
  rent2010     <- plyr::rename(subset(oh2010,houserent>0 & !is.na(houserent))[,c("hhid","houserent")] , c("houserent" = "running_cost", "hhid"="hhid2010"))
  norent2010wh <- subset((merge(plyr::rename(subset(oh2010,houserent==0 || is.na(houserent)), c("hhid"="hhid2010"))[,c("hhid2010","houserent")],homeownerhouses2010[,c("hhid2010","mtm")]) %>% mutate(running_cost = fee*mtm*1.2)) [ ,c("hhid2010","running_cost")] , !is.na(running_cost))
  runningcosts2010 <- rbind(rent2010,norent2010wh)
  
  oh2012               <- ddply(o2012, .(hhid), summarise, houserent = sum(houserent)) [ ,c("hhid","houserent")]
  homeownerhouses2012  <- subset( a2012,  shortname=="house" & number >0)
  rent2012             <- plyr::rename(subset(oh2012,houserent>0 & !is.na(houserent))[,c("hhid","houserent")] , c("houserent" = "running_cost"))
  norent2012wh         <- subset((merge(subset(oh2012,houserent==0 || is.na(houserent)) [,c("hhid","houserent")],homeownerhouses2012[,c("hhid","mtm")]) %>% mutate(running_cost = fee*mtm) ) [ ,c("hhid","running_cost")] , !is.na(running_cost))
  runningcosts2012 <- rbind(rent2012,norent2012wh)
  
  oh2014               <- ddply(o2014, .(hhid), summarise, houserent = sum(houserent)) [ ,c("hhid","houserent")]
  homeownerhouses2014  <- subset( a2014,  shortname=="house" & number >0)
  rent2014             <- plyr::rename(subset(oh2014,houserent>0 & !is.na(houserent))[,c("hhid","houserent")] , c("houserent" = "running_cost"))
  norent2014wh         <- subset( (merge(subset(oh2014,houserent==0 || is.na(houserent)) [,c("hhid","houserent")],homeownerhouses2014[,c("hhid","mtm")]) %>% mutate(running_cost = fee*mtm) ) [ ,c("hhid","running_cost")] , !is.na(running_cost))
  runningcosts2014     <- rbind(rent2014,norent2014wh)
  #the following gives a quantile comparison
  #data.frame(rent2010=quantile(rent2010$running_cost,seq(20)/20), norent2010 = quantile(norent2010wh$running_cost, seq(20)/20),rent2012=quantile(rent2012$running_cost,seq(20)/20), norent2012 = quantile(norent2012wh$running_cost, seq(20)/20) , rent2014=quantile(rent2014$running_cost,seq(20)/20), norent2014 = quantile(norent2014wh$running_cost, seq(20)/20))
  hc <- list()
  hc[["hc2010"]] <- runningcosts2010
  hc[["hc2012"]] <- runningcosts2012
  hc[["hc2014"]] <- runningcosts2014
  return(hc)
  
}
init_data <- function(){
  a2010 <- ll@read_assets_file(year = 2010, dirprefix = "../",fu = fu, ln = lsms_normalizer) ; 
  a2012 <- ll@read_assets_file(year = 2012, dirprefix = "../",fu = fu, ln = lsms_normalizer) ; 
  a2014 <- ll@read_assets_file(year = 2014, dirprefix = "../",fu = fu, ln = lsms_normalizer) ; 
  o2010 <- ll@load_ohs_file(year = 2010, dirprefix = "../",fu=fu, ln=lsms_normalizer) ; 
  o2012 <- ll@load_ohs_file(year = 2012, dirprefix = "../",fu=fu, ln=lsms_normalizer) ; 
  o2014 <- ll@load_ohs_file(year = 2014, dirprefix = "../",fu=fu, ln=lsms_normalizer) ;
  c2010 <- ll@load_diary_file(dirprefix = "../",year = 2010, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  c2012 <- ll@load_diary_file(dirprefix = "../",year = 2012, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  c2014 <- ll@load_diary_file(dirprefix = "../",year = 2014, fu = fu, ln =lsms_normalizer, load_cost = TRUE)
  i2010 <- read.csv('c:/temp/i2010.csv',stringsAsFactors = FALSE)
  i2012 <- read.csv('c:/temp/i2012.csv',stringsAsFactors = FALSE)
  i2014 <- read.csv('c:/temp/i2014.csv',stringsAsFactors = FALSE)
  #e <- minimum_needs_cost_per_head(c2010 = c2010, c2012 = c2012, c2014 = c2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)
  #res <- plain_asset_differences_2012_2014(a2012 = a2012, a2014 = a2014, o2012 = o2012, o2014 = o2014)
  #p <- prepare_pseudo_panels_2010_2012_2014(o2010 = o2010, o2012 = o2012, o2014 = o2014, ll =ll , dirprefix = "../", fu=fu, ln=lsms_normalizer,ncdifftol = 2, yobtol = 3, i2010 = i2010, i2012 = i2012, i2014 = i2014,calibrate_needs=FALSE) 
  #df <- estimation_df(pares = res, e = e, a2010=a2010,a2012= a2012, a2014 = a2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)
  #hist(sapply(res[["x"]]$expenditure,logx),breaks=100)
}

estimation_df <-function( pares, e, pseduop, a2010, a2012, a2014, o2010, o2012, o2014 ){
  if (missing(e)){
    e <- minimum_needs_cost_per_head(c2010 = c2010, c2012 = c2012, c2014 = c2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)
  }
  if (missing(pares)){
    pares <- plain_asset_differences_2012_2014(a2012 = a2012, a2014 = a2014, o2012 = o2012, o2014 = o2014)
  }
  
  adiff2012                                  <- pares[["df"]]
  asum2012                                   <- ddply(subset(plyr::rename(pares[["dat0"]][,c("hhid","cost")],c("hhid"="hhid2012","cost"="cost.2012")), !is.na(cost.2012)),.(hhid2012),summarise,cost.2012=sum(cost.2012))
  df                                         <- merge(asum2012,adiff2012,by=c("hhid2012"),all.x=TRUE)
  df[is.na(df$netmtm.fdelta),]$netmtm.fdelta <- 0
  
  dfe <- merge( df, plyr::rename(subset(e,year==2012),c("hhid"="hhid2012")))
  dfe2012 <- plyr::rename (dfe,c("cost.2012"="At","netmtm.fdelta"="dAt","needs_cost"="Psit"))
  
  # get income estimate from the RE estimator results obtained from pseudo-panel
  pseduop <- merge(get_housing_cost_df(),pseduop,by=c("housingstatus")) %>% mutate (is_higheduc = as.integer(max_education_rank==4))
  
  return (dfe2012)
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

plain_asset_differences_2012_2014 <- function(a2012,a2014,o2012,o2014){
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
  
  pivot_asset             <- "bed"
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
  dats$netmtm.fdelta <- (dats$fdelta==FALSE)*0 + (dats$fdelta==TRUE)*dats$netmtm.delta
  
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
  
  datsres     <- ddply(dats, .(hhid2012), summarise, netmtm.fdelta = sum(netmtm.fdelta))
  
  
  
  
  res = list()
  res[["dat0"]] <- a2012src
  res[["dat1"]] <- a2014src
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

minimum_needs_cost_per_head <- function(c2010, c2012, c2014, o2010, o2012, o2014, mktprices2010,mktprices2012,mktprices2014){
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
  basket_constituent_costs2010 <- ddply( unique(regionfoodprice2010[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2010 <- ddply(basket_constituent_costs2010, .(region,district), summarise, basket_cost = sum(rec_cost)) 
  #regionfoodbasketcosts2010 <- subset(ddply(basket_costs2010,.(region),summarise,basket_cost = min(basket_cost)),!is.na(region))
  #barplot(regionfoodbasketcosts2010$basket_cost,names.arg = regionfoodbasketcosts2010$region, las=2 , xlab= "region" , ylab="cost of food basket" , main="Basket costs across regions (2010)")
  
  hhp2012 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2012,ohsdata=o2012,ddata=fooddiarydata2012)
  hhp2012 <- merge(lsms_normalizer()@categories_needs_based(),hhp2012)
  regionfoodprice2012 <- hhp2012 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2012 <- ddply( unique(regionfoodprice2012[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price) , .(region,district,category), rec_cost = sum(rec_cost))
  basket_costs2012 <- ddply(basket_constituent_costs2012, .(region,district), summarise, basket_cost = sum(rec_cost)) 
  #regionfoodbasketcosts2012 <- subset(ddply(basket_costs2012,.(region),summarise,basket_cost = min(basket_cost)),!is.na(region))
  
  hhp2014 <- ll@add_market_price_to_fooddiary (lgc=lgc,ld=ld,marketpricesdata=mktprices2014,ohsdata=o2014,ddata=fooddiarydata2014)
  hhp2014 <- merge(lsms_normalizer()@categories_needs_based(),hhp2014)
  
  regionfoodprice2014 <- hhp2014 [ ,c("shortname","category","region","district","price","recq" )] %>% group_by(region,district,category) %>% filter(price==min(price))
  basket_constituent_costs2014 <- ddply( unique(regionfoodprice2014[,c("region","district","category","recq","price")]) %>% mutate( rec_cost = recq*price) , .(region,district,category), rec_cost = sum(rec_cost))
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
  
  assumed2010     <- assume_assets(o2010)
  assetlevels2010 <- merge(subset(a2010, number>0), asset_levels_for_name())[,c("hhid","assetlevel")]
  assetlevels2010net <- rbind(assetlevels2010,assumed2010)
  energybasketconstituents2010 <- merge(assetlevels2010net,energy_prices2010 ) %>% mutate(rec_cost = price * recq)
  energybasket2010 <- ddply(energybasketconstituents2010, .(hhid), summarise, basket_cost = sum(rec_cost)) 
  
  assumed2012     <- assume_assets(o2012)
  assetlevels2012 <- merge(subset(a2012, number>0), asset_levels_for_name())[,c("hhid","assetlevel")]
  assetlevels2012net <- rbind(assetlevels2012,assumed2012)
  energybasketconstituents2012 <- merge(assetlevels2012net,energy_prices2012 ) %>% mutate(rec_cost = price * recq)
  energybasket2012 <- ddply(energybasketconstituents2012, .(hhid), summarise, basket_cost = sum(rec_cost)) 
  
  assumed2014     <- assume_assets(o2014)
  assetlevels2014 <- merge(subset(a2014, number>0), asset_levels_for_name())[,c("hhid","assetlevel")]
  assetlevels2014net <- rbind(assetlevels2014,assumed2014)
  energybasketconstituents2014 <- merge(assetlevels2014net,energy_prices2014 ) %>% mutate(rec_cost = price * recq)
  energybasket2014 <- ddply(energybasketconstituents2014, .(hhid), summarise, basket_cost = sum(rec_cost)) 
  
  
  #household needs: mensclothes, womensclothes, childrensclothes, mensshoes, womensshoes, childrensshoes and rent 
  clothing <- get_local_clothing_expenditure(o2012 = o2012, c2012 = c2012, o2014 = o2014, c2014 = c2014, ld = ldat())
  
  #The following can compare how housing maintenance costs are affected by change in field
  #ah2010 <- analyse_house_maintenance_2010(c2010 = c2010, o2010 = o2010)
  #ah2012 <- analyse_house_maintenance_2012_2014(cdat = c2012, odat = o2012)
  #ah2014 <- analyse_house_maintenance_2012_2014(cdat = c2014, odat = o2014)
  #compare2010 <- merge ( plyr::rename(merge(ah2012, unique(o2012[,c("hhid2010","hhid")]),by="hhid"), c("med_maint"="med_maint_2012" ))[,c("hhid2010","med_maint_2012")] , plyr::rename(ah2010, c("med_maint"="med_maint_2010" , "hhid"="hhid2010")) , by = c("hhid2010"))
  
  housing_costs <- assign_house_maintenance(a2010 = a2010, a2012 = a2012, a2014 = a2014, o2010 = o2010, o2012 = o2012, o2014 = o2014)
  
  hc2010 <- as.data.frame(housing_costs["hc2010"])
  hc2012 <- as.data.frame(housing_costs["hc2012"])
  hc2014 <- as.data.frame(housing_costs["hc2014"])
  
  basket_costs2010 <- plyr::rename(basket_costs2010,c("basket_cost"="foodbasket_cost"))
  foodbasket2010   <- merge(basket_costs2010, unique(o2010[,c("hhid","region","district")]), by = c("region","district"))
  energybasket2010 <- plyr::rename(energybasket2010, c("basket_cost"="energybasket_cost"))
  hc2010           <- plyr::rename(hc2010, c("hc2010.running_cost"="housing_cost", "hc2010.hhid2010"="hhid"))
  clothing2010     <- plyr::rename(merge(clothing, unique(o2010[,c("hhid","region")]), by = c("region")) [ ,c("hhid","avcost2010")], c("avcost2010"="clothingcost"))
  allcosts2010     <- merge(clothing2010, merge(foodbasket2010,merge(hc2010, energybasket2010, by = c("hhid")), by=c("hhid")),by=c("hhid") ) %>% mutate (needs_cost = foodbasket_cost + housing_cost + clothingcost + energybasket_cost)
  
  basket_costs2012 <- plyr::rename(basket_costs2012,c("basket_cost"="foodbasket_cost"))
  foodbasket2012   <- merge(basket_costs2012, unique(o2012[,c("hhid","region","district")]), by = c("region","district"))
  energybasket2012 <- plyr::rename(energybasket2012, c("basket_cost"="energybasket_cost"))
  hc2012           <- plyr::rename(hc2012, c("hc2012.running_cost"="housing_cost", "hc2012.hhid"="hhid"))
  clothing2012     <- plyr::rename(merge(clothing, unique(o2012[,c("hhid","region")]), by = c("region")) [ ,c("hhid","avcost2012")], c("avcost2012"="clothingcost"))
  allcosts2012     <- merge(clothing2012, merge(foodbasket2012,merge(hc2012, energybasket2012, by = c("hhid")), by=c("hhid")), by=c("hhid"))  %>% mutate (needs_cost = foodbasket_cost + housing_cost + clothingcost + energybasket_cost)
  
  basket_costs2014 <- plyr::rename(basket_costs2014,c("basket_cost"="foodbasket_cost"))
  foodbasket2014   <- merge(basket_costs2014, unique(o2014[,c("hhid","region","district")]), by = c("region","district"))
  energybasket2014 <- plyr::rename(energybasket2014, c("basket_cost"="energybasket_cost"))
  hc2014           <- plyr::rename(hc2014, c("hc2014.running_cost"="housing_cost", "hc2014.hhid"="hhid"))
  clothing2014     <- plyr::rename(merge(clothing, unique(o2014[,c("hhid","region")]), by = c("region")) [ ,c("hhid","avcost2014")], c("avcost2014"="clothingcost"))
  allcosts2014     <- merge(clothing2014, merge(foodbasket2014,merge(hc2014, energybasket2014, by = c("hhid")), by=c("hhid")), by = c("hhid")) %>% mutate (needs_cost = foodbasket_cost + housing_cost + clothingcost + energybasket_cost)
  
  print("PENDING TODO:  ADD PUBLIC TRANSPORT AS NEED for where it is available")
  
  select_cols      <- c("hhid","needs_cost")
  r                <- data.frame()
  r                <- rbind(r, allcosts2010[,select_cols] %>% mutate(year=2010))
  r                <- rbind(r, allcosts2012[,select_cols] %>% mutate(year=2012)) 
  r                <- rbind(r, allcosts2014[,select_cols] %>% mutate(year=2014)) 
  
  return(r) 
  
  # transport - load petrol prices and load public transport prices
  # household - rent and clothes
  
  # sum up all the costs - this total cost should be seen as p(A_{t-1},\rho) x needs_cost
  
  
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
  print("TODO: <<<<<<<<<<<<<<<<<< change mapping to from asset based to section j based >>>>>>>>>>>>")
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
