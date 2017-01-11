

if (isClass("USCEXNormalize")){
  print ("Warning !!! previous definition of USCEXNormalize would be overwritten.")
}

## all exported functions are declared here
setClass("USCEXNormalize", representation(diary_info_columns_us_cex_2004="function", 
                                          ohs_info_columns_us_cex_2004="function", 
                                          hh_us_cex_mapping_2004="function", 
                                          ohs_mapping_us_cex_2004="function", 
                                          visible_categories_us_cex_2004="function", 
                                          cex_combined_years_ds="function"))
                                       

us_cex_normalize<-function () {
  
  diary_info_columns_us_cex_2004<-function(){
    return(c("hhid","cost","ucc","alloc"));
  }
  
  
  ohs_info_columns_us_cex_2004<-function(){
    return(c("hhid","age","gender","educ","race","hsize","income","horref1","urban_rural","popsize","highest_education"))
  }
  
  
  hh_us_cex_mapping_2004<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="newid",name="hhid"))
    s= rbind(s,data.frame(iesname="cost",name="cost"))
    s= rbind(s,data.frame(iesname="alloc",name="alloc"))
    s= rbind(s,data.frame(iesname="ucc",name="ucc"))
    return(s)
    
  }
  
  ohs_mapping_us_cex_2004<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="newid",name="hhid"))
    s= rbind(s,data.frame(iesname="age_ref",name="age"))
    s= rbind(s,data.frame(iesname="sex_ref",name="gender"))
    s= rbind(s,data.frame(iesname="educ_ref",name="highest_education"))
    s= rbind(s,data.frame(iesname="ref_race",name="race"))
    s= rbind(s,data.frame(iesname="horref1",name="horref1"))
    s= rbind(s,data.frame(iesname="fam_size",name="hsize"))
    s= rbind(s,data.frame(iesname="fincaftm",name="income"))
    s= rbind(s,data.frame(iesname="popsize",name="popsize"))
    s= rbind(s,data.frame(iesname="bls_urbn",name="urban_rural"))
    return(s)
  }
  
  visible_categories_us_cex_2004<-function(){
    #return(c('miscpersonalcare', 'haircareproducts', 'nonelectrichairequipment', 'wigshairpieces',
    #         'oralhygieneproducts', 'shavingneeds', 'cosmetics', 'miscpersonalcare'))
    # personal care, clothing and apparel (including footwear),jewelry, cars
    #return(c("homerent"));
    #return(c('apples', 'bananas', 'oranges', 'freshfruits_other', 'fruites_citrus_non_orange', 'fruits_frozen'));
    return(c("jewelry"))
    stop("should NOT get here")
    return(c('miscpersonalcare', 'haircareproducts', 'nonelectrichairequipment', 'wigshairpieces',
             'oralhygieneproducts', 'shavingneeds', 'cosmetics', 'miscpersonalcare',
             'electricalpersonalcareequipment', 'femalepersonalcareservices', 
             'malepersonalcareservices', 'personalcareappliancesrentalrepair', 'menssuits', 
             'menssportjackets', 'mensformaljackets', 'mensunderwear', 'menshosiery',
             'menssleepwear', 'mensaccessories', 'menssweater', 'mensactivesportswear', 
             'mensshirts', 'menspants', 'mensshorts_exathletic', 'mensuniforms', 
             'boyscoatsjackets', 'boyssweaters', 'boysshirts', 'boysunderwear',
             'boyssleepwear', 'boyshosiery', 'boysaccessories', 
             'boyssuitssportcoats', 'boyspants', 'boysshortsexcathletic', 'boysuniformsactivesportswear',
             'womenscoats', 'womensdresses', 'womenssportcoats', 
             'womenssweaters', 'womensshirts', 'womensskirts', 'womenspants',
             'womensshorts_exathletic', 'womensactivesportswear', 'womenssleepwear', 
             'womensundergarments', 'womenshosiery', 'womenssuits', 'womensaccessories',
             'womensuniforms', 'girlscoatsjackets', 'girlsdressessuits', 'girlssportcoats',
             'girlsskirtspants', 'girlsshortsexathletic', 'girlsactivesportswear', 
             'girlsundergarments', 'girlshosiery', 'girlsaccessories', 'girlsuniforms',
             'mensfootwear', 'boysfootwear', 'girlsfootwear', 'womensfootwear', 'infantscoats',
             'infantsdresses', 'infantsundergarments', 'infantssleepingwear', 'infantsaccessories', 
             'sewingmaterial_clothes', 'watches', 'jewelry', 'shoerepair', 'apparelcleaning_coinoperated',
             'clothes_repair', 'clothing_rental', 'watchjewelryrepair', 'apparell_notcoinoperated', 
             'newcars', 'newtrucks', 'newmotorcycles', 'carlease', 'trucklease', 'usedcars', 
             'usedtrucks', 'usedmotorcycles', 'usedaircraft'))
  }
  
  cex_combined_years_ds<-function(years)
  {
    if (!is.vector(years)){
      stop("years must be a vector")
    }
    resds <-NULL
    for (year in years){
      ds=combined_data_set("us_cex",year,FALSE)
      resds = rbind(resds,ds,stringsAsFactors=FALSE)
    }
    return(resds)
  }
 
  
  return(new("USCEXNormalize",diary_info_columns_us_cex_2004=diary_info_columns_us_cex_2004, 
                                                       ohs_info_columns_us_cex_2004=ohs_info_columns_us_cex_2004, 
                                                       hh_us_cex_mapping_2004=hh_us_cex_mapping_2004, 
                                                       ohs_mapping_us_cex_2004=ohs_mapping_us_cex_2004, 
                                                       visible_categories_us_cex_2004=visible_categories_us_cex_2004, 
                                                       cex_combined_years_ds=cex_combined_years_ds))
             
}