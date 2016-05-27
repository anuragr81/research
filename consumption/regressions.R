
run_regression_cex<-function(ds,year,type){
  if (year==2004){
    # ln(visible_consumption) ~ black_dummy + hispanic_dummy + ln(pInc) 
    #     + area_type + age + age*age + n_members + year_dummy
    if (class(ds$race)!="integer"){
      stop("race variable must be of integer type")
    }
    
    # Do NOT consider hhds with zero income heads
    n_all_income<-length(ds$hhid);
    ds <- ds[ds$income>0,]
    n_cur<-length(ds$hhid)
    print (paste("Ignoring ",round(100*(1- n_cur/n_all_income),3),
                 " % households(hhids having heads with zero income) at sample size=",n_cur))
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
    ds$hispanic_dummy<-as.integer(length(as.character(ds$horref1))>0 & as.integer(ds$race)!=1 & as.integer(ds$race)!=2) # neither black nor white
    print("Regression using only 2004 data")
    
    if (type=="no_controls"){
      res = lm(lnvis~black_dummy+hispanic_dummy,data=ds)
      # the dummies themselves black_dummy + hispanic_dummy show negative coefficients for lnvsi as dependent variable
      return(res)
    }
    if (type=="income_controls"){
      ds$inc <-ds$income # income control
      #ds$incpsv <- as.integer(ds$inc>0) # income control - ignored because we only consider positive income households
      ds$lninc <-log(ds$inc)# income control
      ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
      # only lninc is significant
      res=lm(lnvis~black_dummy+hispanic_dummy+ lninc ,data=ds)
      #TODO: compare with ivreg (and perform the Hausman test)
      return(res)
    }
    if (type == "incpinc_controls"){
      ds$inc <-ds$income # income control
      ds$lninc <-log(ds$inc)# income control
      ds$lnpinc <- log(ds$total_expenditure)
      
      res=lm(lnvis~black_dummy+hispanic_dummy+
               lninc+lnpinc,data=ds)
      return(res)
    }
    if (type == "iv1") {
      ds$agesq <- ds$age*ds$age
      #ds$year_dummy <-year-1995
      
      ds$inc <-ds$total_income_of_household_head
      ds$incpsv <- as.integer(ds$inc>0) # income control
      ds$lninc <-log(ds$inc)# income control
      ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
      ds$lsecd <-as.integer(ds$education<8) 
      ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
      ds$degree <-as.integer(ds$education==13)
      
      ds$lnvis <-log(ds$visible_consumption)
      
      ds$lnpinc <- log(ds$total_expenditure)
      
      res= ivreg(lnvis~black_dummy+hispanic_dummy+ lninc+ lnpinc |
                   . - lnpinc + cbinc + lsecd + secd + degree ,data=ds)
      return(res)
      
    }
    
    if (type == "iv2") {
      ds$agesq <- ds$age*ds$age
      #ds$year_dummy <-year-1995
      
      ds$inc <-ds$total_income_of_household_head
      ds$incpsv <- as.integer(ds$inc>0) # income control
      ds$lninc <-log(ds$inc)# income control
      ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
      ds$lsecd <-as.integer(ds$education<8) 
      ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
      ds$degree <-as.integer(ds$education==13)
      
      ds$lnvis <-log(ds$visible_consumption)
      
      ds$lnpinc <- log(ds$total_expenditure)
      
      res= ivreg(lnvis~black_dummy+hispanic_dummy+ lnpinc  +lsecd |
                   . - lnpinc + cbinc+lninc +incpsv,data=ds)
      return(res)
      
    }
    
    if (type == "ivt1"){
      ds$agesq <- ds$age*ds$age
      #ds$year_dummy <-year-1995
      
      ds$inc <-ds$total_income_of_household_head
      ds$incpsv <- as.integer(ds$inc>0) # income control
      ds$lninc <-log(ds$inc)# income control
      ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
      ds$lsecd <-as.integer(ds$education<8) 
      ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
      ds$degree <-as.integer(ds$education==13)
      ds$year2005 <- as.integer(ds$year==2005)
      ds$lnvis <-log(ds$visible_consumption)
      
      ds$lnpinc <- log(ds$total_expenditure)
      #res=lm(lnvis~black_dummy+hispanic_dummy+ lninc+ lnpinc + year2005,data=ds)
      
      res= ivreg(lnvis~black_dummy+hispanic_dummy+ lninc+ lnpinc + year2005 |
                   . - lnpinc + cbinc + lsecd + secd + degree ,data=ds)
      return(res)
      
    }
    if (type == "ivt2"){
      ds$agesq <- ds$age*ds$age
      ds$year2005 <- as.integer(ds$year==2005)
      
      ds$inc <-ds$total_income_of_household_head
      ds$incpsv <- as.integer(ds$inc>0) # income control
      ds$lninc <-log(ds$inc)# income control
      ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
      ds$lsecd <-as.integer(ds$education<8) 
      ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
      ds$degree <-as.integer(ds$education==13)
      
      ds$lnvis <-log(ds$visible_consumption)
      
      ds$lnpinc <- log(ds$total_expenditure)
      
      res= ivreg(lnvis~black_dummy+hispanic_dummy+ lnpinc  +lsecd+year2005 |
                   . - lnpinc + cbinc+lninc +incpsv,data=ds)
      return(res)
      
    }
    if (type == "ivd1") {
      ds$agesq <- ds$age*ds$age
      #ds$year_dummy <-year-1995
      
      ds$inc <-ds$total_income_of_household_head
      ds$incpsv <- as.integer(ds$inc>0) # income control
      ds$lninc <-log(ds$inc)# income control
      ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
      ds$lsecd <-as.integer(ds$education<8) 
      ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
      ds$degree <-as.integer(ds$education==13)
      
      ds$lnvis <-log(ds$visible_consumption)
      
      ds$lnpinc <- log(ds$total_expenditure)
      
      res= ivreg(lnvis~black_dummy+hispanic_dummy+ lninc+ lnpinc + age+ n_members + area_type|
                   . - lnpinc + cbinc + lsecd + secd + degree ,data=ds)
      return(res)
      
    }
    if (type == "ivd2") {
      ds$agesq <- ds$age*ds$age
      #ds$year_dummy <-year-1995
      
      ds$inc <-ds$total_income_of_household_head
      ds$incpsv <- as.integer(ds$inc>0) # income control
      ds$lninc <-log(ds$inc)# income control
      ds$cbinc <- ds$inc*ds$inc*ds$inc # income control
      ds$lsecd <-as.integer(ds$education<8) 
      ds$secd <- as.integer(ds$education>=8 && ds$education <=12)
      ds$degree <-as.integer(ds$education==13)
      
      ds$lnvis <-log(ds$visible_consumption)
      
      ds$lnpinc <- log(ds$total_expenditure)
      
      res= ivreg(lnvis~black_dummy+hispanic_dummy+ lnpinc  +lsecd  + age + n_members + area_type |
                   . - lnpinc + cbinc+lninc +incpsv,data=ds)
      return(res)
      
    }
  }
  
  stop(paste("Year",year,"not supported"));
}
