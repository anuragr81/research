
inc_control<-function(inc){
  if (inc<=0){
    return(0)
  } else {
    return(log(inc));
  }
}


find_highinc_occupations_lsms<-function(){
  inc<-load_income_file("lsms",2010)
  ohs<-load_ohs_file("lsms",2010)
  highinc_occupations(inc=inc,ohs=ohs)
}
highinc_occupations<-function(inc,ohs){
  
  k<-ohs[!is.na(ohs$occupation),]
  k<-data.frame(hhid=k$hhid,personid=k$personid,occupation=k$occupation)
  i<-inc[!is.na(inc$yearly_pay),]
  m<-merge(i,k)
  #res=ddply(m,.(occupation),summarise,mpay=mean(yearly_pay),sdpay=sd(yearly_pay),n=length(hhid))
  res=ddply(m,.(occupation),summarise,mpay=median(yearly_pay),sdpay=sd(yearly_pay),n=length(hhid))
  #inc[is.element(inc$hhid,intersect(ohs[ohs$occupation==6,]$hhid,inc$hhid)),]
  #ds<-ds[!is.na(ds$occupation),]
  #r<-data.frame(occupation=res$occupation,mpay=res$mpay)
  #rds<-merge(r,ds)
  #x<-ddply(rds,.(mpay),summarise,totexp=median(total_expenditure))
  return(res);
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

write_variables_as_negative_sum<-function(vars){
  prefix="-"
  strout=""
  for ( i in vars){
    strout= paste(strout,prefix,i,sep='') 
  }
  return(strout)
}


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



run_regression_lsms<-function(ds,type,commodity_type,varsInfo){
  depvar <- varsInfo[["depvar"]]
  if (is.null(depvar)){
    stop("depvar must be provided")
  }
  if (type=="engel"){
    #    ds$p<-with(ds,log(visible_consumption)/log(total_expenditure))
    #    hist(ds$p,xlab = "ln(visible expenditure)/ln(total expenditure)",main = "Engel  / total expenditure")
    #plot(log(ds$total_expenditure),log(ds[,depvar]),xlab="ln(Total Expenditure)", 
    #    ylab="ln(Visible Expenditure)",main="Engel Curves for Visible Expenditure")
    # aggregate visible
    
    if (missing(depvar)){
      stop("must provide dependent variable")
      #depvar="visible_consumption";
    }
    
    print(paste("Using data in field:",depvar));
    
    t=.01
    while (quantile(ds[,depvar],t)<=0){
      t=t+.01
    }
    
    num_frames<-5
    stepsize<-(1-t)/num_frames
    thresholds= seq(t,1-stepsize,stepsize)
    
    tot<-length(thresholds)+1
    n<-as.integer(sqrt(tot))
    # n*n <= tot
    nrows=n
    ncols=n
    prod=nrows*ncols
    if (prod < tot){
      # add rows
      rows_inc=FALSE
      while (prod < tot) {
        if (rows_inc == FALSE){
          nrows=nrows+1
          rows_inc=TRUE
        } else {
          ncols=ncols+1
          rows_inc=FALSE
        }
        print (paste("nrows=",nrows,"ncols=",ncols))
        prod=nrows*ncols
      } # end while
    }
    
    ymax=20
    
    #commodity_type="Chosen"
    
    par(mfrow=c(nrows,ncols)) 
    plot(log(ds$total_expenditure),log(ds[,depvar]),xlab="log(Total Expenditure)", 
         ylim=c(0,ymax),
         ylab=paste("log(",commodity_type,"Expenditure)"),
         main=paste("log-log curve for",commodity_type,"consumption")
    )
    
    # calculate percentile
    for (t in thresholds){
      ds_new=ds
      threshold <- as.double(quantile(ds[,depvar],t)) # threshold calculated on the original dataset ds
      if (threshold<=0){
        stop("Lowest quantile must have non-zero visible consumption (please raise threshold)")
      }
      ds_new [ ds_new$visible_consumption<threshold,]$visible_consumption <-0
      print(paste("range of consumption for threshold=",threshold, " is:", 
                  toString(range(ds_new$visible_consumption))))
      
      plot(log(ds_new$total_expenditure),log(ds_new$visible_consumption),xlab="log(Total Expenditure)", 
           ylim=c(0,ymax),
           ylab=paste("log(",commodity_type,"Expenditure)"), 
           main=paste("log-log curve for",
                      commodity_type,
                      "consumption (bottom",toString(round(t*100,1)),"% excluded)"))
    }
    
    #threshold_level<-f(ps)
    #hvs<-ds[ds[,depvar]<threshold_level,]
    return(NULL)
  }
  if (type=="plot"){
    prev_nrows = dim(ds)[1]
    ds <-ds[!is.na(ds$occupation),];
    print(paste("Ignored:",prev_nrows-dim(ds)[1],"entries with null occupation codes"))
    plot(ds$occupation,ds$total_expenditure/1e+6,main="Total Expenditure vs Occupations",xlab="Occupation Codes",ylab="Total Expenditure (in millions)")
    mean_totexp = data.frame(occupation=NULL,mean_totexp=NULL,sd_income=NULL);
    for (occup in unique(ds$occupation)){
      totexp= ds[ds$occupation==occup,]$total_expenditure/1e+6;
      mean_totexp<-rbind(mean_totexp,data.frame(occupation=occup,
                                                mean_totexp=mean(totexp),
                                                sd_income = sd(totexp) 
      ))
    }
    return(mean_totexp)
  }
  if (type=="totexp"){
    res=lm(visible_consumption~total_expenditure,data=ds)
    plot(ds$total_expenditure,ds[,depvar],xlab = "total expenditure",ylab="visible expenditure")
    abline(res)
    return(res)
  }
  
  if (type=="simple"){
    
    varsList <- varsInfo[["vars_list"]]
    if (is.null(varsList)){
      stop("vars_list not provided")
    }
    
    ds$english <- as.integer(ds$litlang==2 | ds$litlang==3)
    
    vars_init <-c("total_expenditure","age","hsize","housingstatus","occupation",
                  "isrural","region","english","roomsnum","years_community",
                  "is_resident","toteducexpense","tothouserent")
    searchres <- significant_lmvars(ds=ds,depvar="visible_consumption",vars_init=varsList);
    sigvars = searchres[["vars"]]
    res = searchres[["result"]]
    print ("RELIGIOUS_EDUCATION,INDUSTRY_CODE,HOUSING_STATUS,LOCALITY_DUMMIES,SELF_REPORTED_HAPPINESS, VISIBLE_SERVICES IGNORED")
    
    print(summary(res))
    min_tval <- min(abs(summary(res)$coefficients[,"t value"]))
    print(paste("min(t-values)=",min_tval))
    resdf<-as.data.frame(summary(res)$coefficients)
    print(resdf[abs(resdf[,"t value"])==min_tval,])
    return(res)
  }
  if (type == "simple2"){
    
    if (missing(depvar)){
      stop("must provide dependent variable")
      #depvar="visible_consumption";
    }
    
    varsList <- varsInfo[["vars_list"]]
    if (is.null(varsList)){
      stop("vars_list not provided")
    }
    ds$dseducexpense = ds$toteducexpense+1e-10# adding a small quantity to ensure log transformation is not -Inf
    ds$lndseducexpense = log(ds$dseducexpense)
    ds$dshouserent =ds$tothouserent+1e-10 # adding a small quantity to ensure log transformation is not -Inf
    ds$lndshouserent = log(ds$dshouserent)
    ds$visible_consumption<-ds$visible_consumption+1e-10 # adding a small quantity to ensure log transformation is not -Inf
    ds$lnvis <- log(ds$visible_consumption) 
    
    ds$total_expenditure <- ds$total_expenditure+1e-10
    
    ds$lnpinc <- log(ds$total_expenditure)
    #ds$nonenglish <- as.integer(ds$litlang==1 | ds$litlang==4)
    ds$english <- as.integer(ds$litlang==2 | ds$litlang==3)
    print("PENDING region dummies")
    length_region_dummy=unique(ds$region)[-1]
    searchres <- significant_lmvars(ds=ds,depvar=depvar,vars_init=varsList);
    sigvars = searchres[["vars"]]
    res = searchres[["result"]]
    
    print(summary(res))
    
    min_tval <- min(abs(summary(res)$coefficients[,"t value"]))
    print(paste("min(t-values)=",min_tval))
    resdf<-as.data.frame(summary(res)$coefficients)
    print(resdf[abs(resdf[,"t value"])==min_tval,])
    return(res)
  }
  
  if (type=="2sls"){
    print ("RELIGIOUS_EDUCATION,INDUSTRY_CODE,SELF_REPORTED_HAPPINESS,AREA_TYPE IGNORED")
    if (missing(depvar)){
      stop("must provide dependent variable")
      #depvar="visible_consumption";
    }
    
    instrumentList <- varsInfo[["instrument_list"]]
    if (is.null(instrumentList)){
      stop("instrument_list not provided")
    }
    
    endogenousVars<- varsInfo[["endogenous_vars"]]
    if (is.null(endogenousVars)){
      stop("endogenous_vars not provided")
    }
    
    varsList <- varsInfo[["vars_list"]]
    if (is.null(varsList)){
      stop("vars_list not provided")
    }
    
    #TODO: move all secondary variables into a varsInfo functor 
  
    ds$lnpinc <- log(ds$total_expenditure)
    
    ds$cubic_highest_educ<-with(ds,highest_educ*highest_educ*highest_educ)
    ds$highest_educ<-ds$highest_educ+1e-10
    ds$ln_highest_educ<-log(ds$highest_educ)

    ds$cubic_age<-with(ds,age*age*age)
    
    ln_age<-log(ds$age)
    ln_age[ln_age==-Inf]<-0
    ds$ln_age<-ln_age
    ds$english <- as.integer(ds$litlang==2 | ds$litlang==3)
    
    ds$dseducexpense = ds$toteducexpense+1e-10# adding a small quantity to ensure log transformation is not -Inf
    ds$lndseducexpense = log(ds$dseducexpense)
    ds$dshouserent =  ds$tothouserent+1e-10 # adding a small quantity to ensure log transformation is not -Inf
    ds$lndshouserent = log(ds$dshouserent)
    ds$visible_consumption<-ds$visible_consumption+1e-10 # adding a small quantity to ensure log transformation is not -Inf
    ds$lnvis <- log(ds$visible_consumption)
    
    searchres <- significant_lmvars(ds=ds,depvar=depvar,vars_init=varsList);
    sigvars = searchres[["vars"]]
    ivreg_string <- paste(depvar,"~",write_variables_as_sum(sigvars), "| .",write_variables_as_negative_sum(endogenousVars),"+",write_variables_as_sum(instrumentList))
    print(paste("ivreg_string=",ivreg_string))
    res <- ivreg(ivreg_string,data=ds)
    print(summary(res,diagnostics=TRUE))
    return(res)
  }

  stop(paste("type:",type," not recognized"));
}

run_regression_cex<-function(ds,type){
  
  # ln(visible_consumption) ~ black_dummy + hispanic_dummy + ln(pInc) 
  #     + area_type + age + age*age + n_members + year_dummy
  if (class(ds$race)!="integer"){
    stop("race variable must be of integer type")
  }
  
  # Do NOT consider hhds with zero income heads
  n_all_income<-length(ds$hhid);
  
  #ds <- ds[ds$income>0,]
  #n_cur<-length(ds$hhid)
  #print (paste("Ignoring ",round(100*(1- n_cur/n_all_income),3),
  #             " % households(hhids having heads with zero income) at sample size=",n_cur))
  ds<-ds[ds$visible_consumption>0,]
  n_vis<-length(ds$hhid)
  print (paste("Ignoring altogether",round(100*(1- n_vis/n_all_income),3),
               " % households(hhids with zero visible income) at sample size=",n_vis))
  
  ds<-ds[ds$total_expenditure>0,]
  n_totexp<-length(ds$hhid)
  
  print (paste("Ignoring altogether",round(100*(1- n_totexp/n_all_income),3),
               " % households(hhids with zero total expenditure) at sample size=",n_totexp))
  
  ds$lnvis<-log(ds$visible_consumption)
  ds$black_dummy <-as.integer(ds$race==2)
  if (!is.logical(ds$hispanic)){
    stop("hispanic column must be a logical variable")
  }
  ds$hispanic_dummy<-as.integer(ds$hispanic) # use a translated non-overlapping value
  
  if (type=="no_controls"){
    res = lm(lnvis~black_dummy+hispanic_dummy,data=ds)
    # the dummies themselves black_dummy + hispanic_dummy show negative coefficients for lnvsi as dependent variable
    return(res)
  }
  if (type=="income_controls"){
    print("Pending addition of industry and occupation codes ")
    ds$incpsv <- as.integer(ds$income>0)
    ds$lninc <-sapply(ds$income,inc_control) # income control
    ds$cbinc <- ds$income*ds$income*ds$income # income control
    res=lm(lnvis~black_dummy+hispanic_dummy+ lninc+cbinc + incpsv ,data=ds)
    return(res)
  }
  if (type == "incpinc_controls"){
    print("No income controls in incpinc")
    ds$lnpinc <- log(ds$total_expenditure)
    
    res=lm(lnvis~black_dummy+hispanic_dummy+
             lnpinc,data=ds)
    return(res)
  }
  if (type == "v") {
    print("Pending addition of industry and occupation codes ")
    ds$incpsv <- as.integer(ds$income>0)
    ds$lninc <-sapply(ds$income,inc_control) # income control
    ds$cbinc <- ds$income*ds$income*ds$income # income control
    #ds$lsecd <-as.integer(ds$highest_education<12) 
    #ds$secd <- as.integer(ds$highest_education==12)
    #ds$college <-as.integer(ds$highest_education==13)
    #ds$degree <-as.integer(ds$highest_education>=14)
    ds$lnvis <-log(ds$visible_consumption)
    
    ds$lnpinc <- log(ds$total_expenditure)
    
    res= ivreg(lnvis~black_dummy+hispanic_dummy+ lnpinc |
                 . - lnpinc + incpsv+ lninc+ cbinc ,data=ds)
    return(res)
    
  }
  
  if (type == "vi") {
    print("Pending addition of industry and occupation codes ")
    ds$incpsv <- as.integer(ds$income>0)
    ds$lninc <-sapply(ds$income,inc_control) # income control
    ds$cbinc <- ds$income*ds$income*ds$income # income control
    #ds$lsecd <-as.integer(ds$highest_education<12) 
    #ds$secd <- as.integer(ds$highest_education==12)
    #ds$college <-as.integer(ds$highest_education==13)
    #ds$degree <-as.integer(ds$highest_education>=14)
    ds$lnvis <-log(ds$visible_consumption)
    
    ds$lnpinc <- log(ds$total_expenditure)
    
    res= ivreg(lnvis~black_dummy+hispanic_dummy+ lnpinc+year |
                 . - lnpinc + incpsv+ lninc+ cbinc ,data=ds)
    return(res)
    
    
  }
  
  if (type == "vii"){
    #stop("pending wealth/liquid assets control")
    ds$incpsv <- as.integer(ds$income>0)
    ds$lninc <-sapply(ds$income,inc_control) # income control
    ds$cbinc <- ds$income*ds$income*ds$income # income control
    
    ds$agesq <- ds$age*ds$age
    ds$lsecd <-as.integer(ds$highest_education<12) 
    ds$secd <- as.integer(ds$highest_education==12)
    ds$college <-as.integer(ds$highest_education==13)
    ds$degree <-as.integer(ds$highest_education>=14)
    
    ds$lnvis <-log(ds$visible_consumption)
    
    ds$lnpinc <- log(ds$total_expenditure)
    
    # lsecd,college,secd removed as they were not significant
    res= ivreg(lnvis~black_dummy+hispanic_dummy+ lnpinc+year+age+agesq+degree|#+lsecd+secd+degree+college |
                 . - lnpinc + incpsv+ lninc+ cbinc ,data=ds)
    
    
    return(res)
    
  }
  stop(paste("Unknown regression type:",type))
}
