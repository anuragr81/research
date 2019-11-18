

if (isClass("NigeriaNormaliser")){
  print ("Warning !!! previous definition of NigeriaNormaliser would be overwritten.")
}

## all exported functions are declared here
setClass("NigeriaNormaliser", representation(diary_columns_mapping="function", diary_info_columns_2010="function",
                                             get_lsms_weekrecall_info_columns="function",get_lsms_weekrecall_fields_mapping="function"
))


ngr_normaliser<-function() {
  
  diary_info_columns_2010<-function(){
    return(c("hhid","item","lwp_unit", "lwp","tlwp_unit", "gift_unit","gift","own_unit","own", "tlwp", "cost"))
  }
  
  diary_columns_mapping<-function(year){
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="s7bq1",name="is_consumed"))
      s= rbind(s,data.frame(iesname="item_cd",name="item"))
      s= rbind(s,data.frame(iesname="s7bq4",name="cost")) # this is populated only if there is consumption form items purchased in last 7 days
      s= rbind(s,data.frame(iesname="s7bq2b",name="lwp_unit"))
      s= rbind(s,data.frame(iesname="s7bq2a",name="lwp"))
      s= rbind(s,data.frame(iesname="s7bq3b",name="tlwp_unit"))
      s= rbind(s,data.frame(iesname="s7bq3a",name="tlwp"))
      
      s= rbind(s,data.frame(iesname="s7bq5b",name="own_unit"))
      s= rbind(s,data.frame(iesname="s7bq5a",name="own"))
      s= rbind(s,data.frame(iesname="s7bq6b",name="gift_unit"))
      s= rbind(s,data.frame(iesname="s7bq6a",name="gift"))
      
      return(s)
    }
    stop(paste("Year:",year,"not supported"))
  }
  
  get_lsms_weekrecall_info_columns <-function(year){
    if (year == 2010){
      return(c("hhid","region","district", "zone","item","ea","cost","is_urban"))
    }
    
    stop(paste("Year:",year,"not supported"))
  }
  
  get_lsms_weekrecall_fields_mapping <- function(year){
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="state",name="region"))
      s= rbind(s,data.frame(iesname="lga",name="district"))
      s= rbind(s,data.frame(iesname="sector",name="is_urban"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="item_cd",name="item"))
      s= rbind(s,data.frame(iesname="s8q2",name="cost"))
      return(s)
    }
    

    stop(paste("Year:",year,"not supported"))
  }
  

  return(new("NigeriaNormaliser",diary_columns_mapping=diary_columns_mapping, 
             diary_info_columns_2010=diary_info_columns_2010,get_lsms_weekrecall_info_columns=get_lsms_weekrecall_info_columns,
             get_lsms_weekrecall_fields_mapping=get_lsms_weekrecall_fields_mapping) )
  
}