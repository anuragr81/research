library(foreign)
require(plyr)


if (isClass("LSMSLoader")){
  print ("Warning !!! previous definition of LSMSLoader would be overwritten.")
}

## all exported functions are declared here
setClass("LSMSLoader", representation(combined_data_set="function"))

lsms_loader<-function() {
  
  read_tnz <- function(filename,convert_factors) {
    if (!is.logical(convert_factors) || !is.atomic(convert_factors)){
      stop("convert_factords must be ")
    }
    dat1 = read.dta(filename,convert.factors = convert_factors);
    dat2 = as.data.frame(dat1,stringsAsFactors=FALSE);
    dat3 = dat2[as.numeric(dat2$y2_hhid)>0,] # only take data with hhid>0
    return(dat3);
  }
  
  load_diary_file <-function(dirprefix,year){
    if (year == 2010){
      # combine sections ( k , l, m )
      kdat <- read_tnz(paste(dirprefix,"./lsms/TZNPS2HH3DTA/HH_SEC_K1.dta",sep=""),FALSE)
      #*    Reading weekly Diary data in Section K data and retrieving item as well as the quantity as well as cost 
      
      k <- fu()@get_translated_frame(dat=kdat,
                                names=diary_info_columns_lsms_2010(),
                                m=hh_mapping_lsms_2010())
      k$hhid <-as.character(k$hhid)
      #*    Ignored items where there is no associated cost
      k <- k[as.numeric(k$cost)>0 & !is.na(k$cost),]
      k$item<-k$item+10000 # adding 10,000 only to avoid overlaps with sections (l,m)
      factor <- 52
      
      #*    Multiplied weekly diary data by 52 (to look at annual data)
      # quantities are normalized to annual values
      k$cost <- k$cost*factor
      k$lwp <- k$lwp *factor
      k$own <-k$own*factor
      k$gift <-k$gift*factor
      
      #*    gift quantities are ignored (total quantity ignored is to be presented)
      #*    weekly recall items are also multiplied by 52
      
      ldat <- read_tnz(paste(dirprefix,"./lsms/TZNPS2HH2DTA/HH_SEC_L.dta",sep=""),FALSE)
      l <- fu()@get_translated_frame(dat=ldat,
                                names=get_lsms_secl_info_columns_2010(),
                                m=get_lsms_secl_fields_mapping_2010())
      l$hhid <-as.character(l$hhid)
      l <- l[!is.na(l$cost) & l$cost>0 & !is.na(l$hhid),]
      weekly_recall_items <-c(101,102,103)
      
      # l is weekly and  monthly data
      
      l <- multiplyLsmsQuantities(dat = l , 
                                  quantity_field_name="cost", 
                                  item_field_name="item", 
                                  factor=52,
                                  items_list = weekly_recall_items)
      
      monthly_recall_items <- c("201", "202", "203", "204", "205", "206", "207", "208", "209",
                                "210", "211", "212", "213", "214", "215", "216", "217", "218", "219",
                                "220", "221", "222", "223", "224")  
      #*    Monthly recall items are multiplied by 12
      l <- multiplyLsmsQuantities(dat = l , 
                                  quantity_field_name="cost", 
                                  item_field_name="item", 
                                  factor=12,
                                  items_list = monthly_recall_items)
      
      # m is yearly data
      mdat <-read_tnz( paste(dirprefix,'./lsms/TZNPS2HH2DTA/HH_SEC_M.dta',sep=""),FALSE)
      m <- fu()@get_translated_frame(dat=mdat,
                                names=get_lsms_secm_info_columns(2010),
                                m=get_lsms_secm_fields_mapping(2010))
      m$hhid <-as.character(m$hhid)
      m<- m[!is.na(m$hhid) & !is.na(m$cost) & m$cost>0,]
      # nothing to be multiplied for yearly-recall (since we're looking at annual consumption)
      
      #yearly_recall_items <- c("301", "302", "303", "304", "305", "306", "307", "308", "309", 
      #                         "310", "311", "312", "313", "314", "315", "316", "317", "318", "319")
      
      # Either outer-join or an rbind must be used
      #*    zero-cost items are ignored for all these 
      ml <-merge(m,l,all=TRUE)
      #return(ml)
      mlk <-merge(ml,k,all=TRUE)
      return(mlk)
      #*    merging all the 4 categories results in the expenditure file
    }
    stop(paste("Year:",year, " not supported"))
  }
  
  combined_data_set<-function(dataset,year,selected_category,isTranslated,isDebug, set_depvar){
    
    ############ PHASE 0 ########################
    if (missing(set_depvar)){
      set_depvar = TRUE 
    }
    if (missing(selected_category)){
      print("setting selected_category to the default value")
      selected_category= visible_categories(dataset=dataset,year=year)
    }
    
    #*  (( Loading diary file
    hhdat <- load_diary_file(dataset,year) # must provide total and visible expenditure (must be already translated)
    
    #* Loading the person/family data fie
    ohsdat <-load_ohs_file(dataset,year) # (using fmld) must provide age (age_ref), gender(sex_ref), 
    # highest_educ(educ_ref), ishead(no_earnr,earncomp - all reference person data),
    # race(ref_race),family size (fam_size),
    # area_type (popsize,bls_urbn)
    #* Loading income file
    incomedat <-load_income_file(dataset,year) # must provide total income (fincaftm)
    
    if (is.null(hhdat)){
      stop("Could not load diary hhdata")
    }
    
    print("Ensuring duplicates do NOT exist in the diary hhdata")
    hhdat = unique(hhdat)
    
    ############ PHASE 1 - Translation ########################
    # info_columns must contain all hhdata-fields referred to in merging/aggregation phase (one per file)
    # translated frame makes the data available in a universal dictionary (age, gender etc.)
    hh<-hhdat
    ohs <-ohsdat
    income<-incomedat
    
    print("Loaded translated frame(s)")
    ############ PHASE 2 - Aggregation and Merge ########################
    # merge criteria is defined for every dependent variable
    
    ignored_hhids <- get_ignored_hhids(dataset,hhdat,ohsdat,incomedat);
    if (!missing(isDebug) && isDebug==TRUE){
      print(paste("Ids to be ignored(",length(ignored_hhids),"):{",toString(ignored_hhids),"}"))
    }
    if (!is.null(ignored_hhids)){  
      if (!is.null(hhdat)){
        n1<-length(unique(hh$hhid))
        hh<-hh[!is.element(as.character(hh$hhid),as.character(ignored_hhids)),]
        
        n2<-length(unique(hh$hhid))
        print(paste("ignored",n1-n2,"/",n1," hhids"))
      }
      if (!is.null(ohsdat)){
        
        n1<-length(unique(ohs$hhid))
        ohs<-ohs[!is.element(as.character(ohs$hhid),as.character(ignored_hhids)),]
        
        n2<-length(unique(ohs$hhid))
        print(paste("ignored",n1-n2,"/",n1," hhids"))
      }
      if (!is.null(incomedat)){
        n1<-length(unique(income$hhid))
        income<-income[!is.element(as.character(income$hhid),as.character(ignored_hhids)),]
        n2<-length(unique(income$hhid))
        print(paste("ignored",n1-n2,"/",n1," hhids"))
      }
    }
    dstruct<-merge_hh_ohs_income_data(dataset=dataset,hh=hh,ohs=ohs,income=income,year=year,selected_category=selected_category,set_depvar=set_depvar);
    return(dstruct);
    #* ))
  }
  
  return(new("LSMSLoader",combined_data_set=combined_data_set))
}