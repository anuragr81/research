
inc_control<-function(inc){
  if (inc<=0){
    return(0)
  } else {
    return(log(inc));
  }
}

run_regression_lsms<-function(ds,type){
  if (type=="engel"){
    #    ds$p<-with(ds,log(visible_consumption)/log(total_expenditure))
    #    hist(ds$p,xlab = "ln(visible expenditure)/ln(total expenditure)",main = "Engel  / total expenditure")
    #plot(log(ds$total_expenditure),log(ds$visible_consumption),xlab="ln(Total Expenditure)", 
    #    ylab="ln(Visible Expenditure)",main="Engel Curves for Visible Expenditure")
    
    # electricity is spent by only top 20%
    # rice is spent by top 45%
    # donation to charity, soap bar, top 35%
    # personal items repair by top 4%
    t=.01
    while (quantile(ds$visible_consumption,t)<=0){
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
    
    commodity_type="Chosen"
    
    par(mfrow=c(nrows,ncols)) 
    plot(log(ds$total_expenditure),log(ds$visible_consumption),xlab="log(Total Expenditure)", 
         ylim=c(0,ymax),
         ylab=paste("log(",commodity_type,"Expenditure)"),
         main=paste("log-log curve for",commodity_type,"consumption")
    )
    
    # calculate percentile
    for (t in thresholds){
      ds_new=ds
      threshold <- as.double(quantile(ds$visible_consumption,t)) # threshold calculated on the original dataset ds
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
    #hvs<-ds[ds$visible_consumption<threshold_level,]
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
    plot(ds$total_expenditure,ds$visible_consumption,xlab = "total expenditure",ylab="visible expenditure")
    abline(res)
    return(res)
  }
  
  if (type=="simple"){
    # highest_educ, age, company-at-work, highest_eduation, years_in_community(=age when 99), total_expenditure, is_migrant, family_size  
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+highest_educ+age+years_community+is_resident+yearly_pay)# (yearly pay least significant)
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+highest_educ+age+years_community+is_resident)# (highest_educ least significant)
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+age+years_community+is_resident) # (is_resident least significant)
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+age+years_community) # age least significant
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+years_community) #  years_community least significant
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize)
    
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+years_community+age)# (highest_educ least significant)
    
    res=lm(data=ds,visible_consumption~total_expenditure+hsize+is_resident)
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize)
    
    # could is_resident be multicollinear?
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+age+years_community)
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+age+is_resident)
    
    #(religious_education, locality_dummies,self_reported_happiness,housing_expenditure,education,price_based_class,urban_rural)
    print ("RELIGIOUS_EDUCATION,INDUSTRY_CODE,HOUSING_STATUS,LOCALITY_DUMMIES,SELF_REPORTED_HAPPINESS, VISIBLE_SERVICES IGNORED")
    
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+years_community+is_resident)
    print(summary(res))
    return(res)
  }
  if (type == "simple2"){
    ds$lnvis <- log(ds$visible_consumption) 
    ds$lnvis[ds$lnvis==-Inf]<-0 # zeroing out -Inf from log
    ds$lnpinc <- log(ds$total_expenditure)
    #ds$nonenglish <- as.integer(ds$litlang==1 | ds$litlang==4)
    ds$english <- as.integer(ds$litlang==2 | ds$litlang==3)
    #res=lm(data=ds,lnvis~lnpinc+hsize+english+highest_educ)# both hsize and highest_educ are significant for all categories (except motorcycle repairs)
    print("PENDING region dummies")
    length_region_dummy=unique(ds$region)[-1]
    res=lm(data=ds,lnvis~lnpinc+hsize+english+occupation+isurbanp)
    print(summary(res))
    return(res)
  }
  if (type=="2sls_income"){
    print ("RELIGIOUS_EDUCATION,INDUSTRY_CODE,HOUSING_STATUS,LOCALITY_DUMMIES,SELF_REPORTED_HAPPINESS,AREA_TYPE, VISIBLE_SERVICES IGNORED")
    
    ds <-ds[ds$yearly_pay>0,]
    
    ds$lnvis <- log(ds$visible_consumption)
    ds$lnvis[ds$lnvis==-Inf]<-0 # zeroing out -Inf from log
    
    ds$lnpinc <- log(ds$total_expenditure)
    ds$lninc <- log(ds$yearly_pay)
    ds$incpsv <- as.integer(ds$yearly_pay>0)
    ds$cbinc <- ds$yearly_pay*ds$yearly_pay*ds$yearly_pay
    res<-NULL
    #res<- ivreg(data=ds,lnvis~lnpinc+age|
    #             . - lnpinc + incpsv+ lninc+ cbinc)
    
    res<- ivreg(data=ds,lnvis~lnpinc+highest_educ+years_community|
                  . - lnpinc + incpsv+ lninc+ cbinc)
    
    print(summary(res,diagnostics=TRUE))
    return(res)    
  }
  if (type=="2sls"){
    print ("RELIGIOUS_EDUCATION,INDUSTRY_CODE,HOUSING_STATUS,LOCALITY_DUMMIES,SELF_REPORTED_HAPPINESS,AREA_TYPE, VISIBLE_SERVICES IGNORED")
    ds$lnvis <- log(ds$visible_consumption) 
    ds$lnvis[ds$lnvis==-Inf]<-0 # zeroing out -Inf from log
    ds$lnpinc <- log(ds$total_expenditure)
    
    ds$cubic_highest_educ<-with(ds,highest_educ*highest_educ*highest_educ)
    ln_highest_educ<-log(ds$highest_educ)
    ln_highest_educ[ln_highest_educ==-Inf]<-0
    ds$ln_highest_educ<-ln_highest_educ
    
    ds$cubic_age<-with(ds,age*age*age)
    ln_age<-log(ds$age)
    ln_age[ln_age==-Inf]<-0
    ds$ln_age<-ln_age
    ds$english <- as.integer(ds$litlang==2 | ds$litlang==3)
    
    res<-NULL
    #res<- ivreg(data=ds,lnvis~lnpinc+age|
    #             . - lnpinc + incpsv+ lninc+ cbinc)
    
    if (FALSE){
      res<- ivreg(data=ds,lnvis~lnpinc+age|
                    . - lnpinc + highest_educ + ln_highest_educ+cubic_highest_educ)
      #               . - lnpinc + age + ln_age+cubic_age)
    }
    # highest_educ is near signficant (1.7) and instrumentation by age, age-cubics seems 
    # fine (subject to verification of diagnostics), endogeneity is not significant when
    # when adding years_community (instead of highest_educ)
    # in this analysis education is the second most important component the most (subject to verification of age as
    # valid instrument) housingstatus is also nearly significant
    if (FALSE){
      res<- ivreg(data=ds,lnvis~lnpinc+highest_educ|
                    . - lnpinc + age + ln_age+cubic_age)
    }
    
    if (TRUE){
      res<- ivreg(data=ds,lnvis~lnpinc+english+isurbanp|
                    . - lnpinc + age + ln_age+cubic_age+occupation+highest_educ)
      print("Pending better instrument than age");
    }
    print(summary(res,diagnostics=TRUE))
    return(res)    
  }
  #help(summary.ivreg)
  
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
