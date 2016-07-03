
inc_control<-function(inc){
  if (inc<=0){
    return(0)
  } else {
    return(log(inc));
  }
}

run_regression_lsms<-function(ds,type){
  
  if (type=="totexp"){
    res=lm(visible_consumption~total_expenditure,data=ds)
    plot(ds$total_expenditure,ds$visible_consumption,xlab = "total expenditure",ylab="visible expenditure")
    abline(res)
    return(res)
  }
  
  if (type=="simple"){
    # highest_educ, age, company-at-work, highest_eduation, years_in_community(=age when 99), total_expenditure, is_migrant, family_size  
    res=lm(data=ds,visible_consumption~total_expenditure+hsize+highest_educ+age+years_community+is_resident+yearly_pay)# (yearly pay least significant)
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+highest_educ+age+years_community+is_resident)# (highest_educ least significant)
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+age+years_community+is_resident) # (is_resident least significant)
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+age+years_community) # age least significant
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+years_community) #  years_community least significant
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize)
    
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+years_community+age)# (highest_educ least significant)
    
    
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize)
    
    # could is_resident be multicollinear?
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+age+years_community)
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+age+is_resident)
    
    #(religious_education, locality_dummies,self_reported_happiness,housing_expenditure,education,price_based_class,urban_rural)
    print ("RELIGIOUS_EDUCATION,INDUSTRY_CODE,HOUSING_STATUS,LOCALITY_DUMMIES,SELF_REPORTED_HAPPINESS,AREA_TYPE, VISIBLE_SERVICES IGNORED")
    
    #res=lm(data=ds,visible_consumption~total_expenditure+hsize+years_community+is_resident)
    print(summary(res))
    return(res)
    }
  if (type=="2sls"){
    print ("RELIGIOUS_EDUCATION,INDUSTRY_CODE,HOUSING_STATUS,LOCALITY_DUMMIES,SELF_REPORTED_HAPPINESS,AREA_TYPE, VISIBLE_SERVICES IGNORED")
    
    ds <-ds[ds$yearly_pay>0,]
    
    ds$lnvis <- log(ds$visible_consumption) 
    ds$lnpinc <- log(ds$total_expenditure)
    ds$lninc <- log(ds$yearly_pay)
    ds$incpsv <- as.integer(ds$yearly_pay>0)
    ds$cbinc <- ds$yearly_pay*ds$yearly_pay*ds$yearly_pay
    res<-NULL
    #res<- ivreg(data=ds,lnvis~lnpinc+age|
    #             . - lnpinc + incpsv+ lninc+ cbinc)

    res<- ivreg(data=ds,lnvis~lnpinc+highest_educ+years_community|
                 . - lnpinc + incpsv+ lninc+ cbinc)
    
    print(summary(res))
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
