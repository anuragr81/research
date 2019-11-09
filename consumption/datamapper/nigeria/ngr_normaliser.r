

if (isClass("NigeriaNormaliser")){
  print ("Warning !!! previous definition of NigeriaNormaliser would be overwritten.")
}

## all exported functions are declared here
setClass("NigeriaNormaliser", representation(diary_columns_mapping="function", diary_info_columns_2010="function"
))

ngr_normaliser<-function() {
  
  diary_info_columns_2010<-function(){
    return(c("hhid","item","lwp_unit", "lwp", "cost"))
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
      return(s)
    }
    stop(paste("Year:",year,"not supported"))
  }

  return(new("NigeriaNormaliser",diary_columns_mapping=diary_columns_mapping, 
             diary_info_columns_2010=diary_info_columns_2010) )
  
}