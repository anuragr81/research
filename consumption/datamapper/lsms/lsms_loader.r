library(foreign)
require(plyr)


if (isClass("LSMSLoader")){
  print ("Warning !!! previous definition of LSMSLoader would be overwritten.")
}

## all exported functions are declared here
setClass("LSMSLoader", representation(combined_data_set="function",load_diary_file="function",
                                      analyse_cj="function",load_ohs_file="function"))


lsms_loader<-function(fu,ln) {
  
  read_tnz <- function(filename,convert_factors) {
    if (!is.logical(convert_factors) || !is.atomic(convert_factors)){
      stop("convert_factords must be ")
    }
    print(paste("Reading file:",filename))
    dat1 = read.dta(filename,convert.factors = convert_factors);
    dat2 = as.data.frame(dat1,stringsAsFactors=FALSE);
    dat3 = dat2[as.numeric(dat2$y2_hhid)>0,] # only take data with hhid>0
    return(dat3);
  }
  
  get_expensiveregion_codes<-function(){
    return (c(7,12,19,53)) 
  }
  
  load_diary_file <-function(dirprefix,year){
    if (year == 2010){
      # combine sections ( k , l, m )
      kdat <- read_tnz(paste(dirprefix,"./lsms/TZNPS2HH3DTA/HH_SEC_K1.dta",sep=""),FALSE)
      #*    Reading weekly Diary data in Section K data and retrieving item as well as the quantity as well as cost 
      
      k <- fu()@get_translated_frame(dat=kdat,
                                     names=ln()@diary_info_columns_lsms_2010(),
                                     m=ln()@hh_mapping_lsms_2010())
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
                                     names=ln()@get_lsms_secl_info_columns_2010(),
                                     m=ln()@get_lsms_secl_fields_mapping_2010())
      l$hhid <-as.character(l$hhid)
      l <- l[!is.na(l$cost) & l$cost>0 & !is.na(l$hhid),]
      weekly_recall_items <-c(101,102,103)
      
      # l is weekly and  monthly data
      
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=52,
                                       items_list = weekly_recall_items)
      
      monthly_recall_items <- c("201", "202", "203", "204", "205", "206", "207", "208", "209",
                                "210", "211", "212", "213", "214", "215", "216", "217", "218", "219",
                                "220", "221", "222", "223", "224")  
      #*    Monthly recall items are multiplied by 12
      l <- ln()@multiplyLsmsQuantities(dat = l , 
                                       quantity_field_name="cost", 
                                       item_field_name="item", 
                                       factor=12,
                                       items_list = monthly_recall_items)
      
      # m is yearly data
      mdat <-read_tnz( paste(dirprefix,'./lsms/TZNPS2HH2DTA/HH_SEC_M.dta',sep=""),FALSE)
      m <- fu()@get_translated_frame(dat=mdat,
                                     names=ln()@get_lsms_secm_info_columns(2010),
                                     m=ln()@get_lsms_secm_fields_mapping(2010))
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
  
  
  load_ohs_file <-function(year,dirprefix){
    
    if (year == 2010){
      
      #* Read section c_cb file
      cbdat<-read.dta(paste(dirprefix,'./lsms/TZNPS2COMDTA/COMSEC_CB.dta',sep=""),convert.factors = FALSE)
      
      cb <- fu()@get_translated_frame(dat=cbdat,
                                      names=ln()@ohs_seccb_columns_lsms_2010(),
                                      m=ln()@ohs_seccb_mapping_lsms_2010())
      #* chose facilitycode l and collected accessibility 1 and 2(<10) (in the centre or less than 10 km away)
      l<-(cb[is.element(tolower(as.character(cb$facilitycode)),c("l")),])
      #* extract those with 1
      l$accessiblemarket<-as.integer(l$accessibility==1)
      #* extract those with 2 (and assign them the same status as 1's)
      l$accessiblemarket<-l$accessiblemarket+as.integer(l$accessibility==2 & l$distance<10)
      l=l[!is.na(l$accessiblemarket),]
      #* chose accessible market value using (if both in the centre and closer then ambiguous)
      l_i=ddply(l,.(region,district,ward),summarize,accessiblemarket=max(accessiblemarket))
      l = merge(l,l_i)
      fu()@removeall_cols_except(l,c("region","district","ward","accessiblemarket","travelcost"))
      #l[,setdiff(names(l),)]<-NULL
      
      #l = data.frame(region=l$region,district=l$district,ward=l$ward,ea=l$ea,accessiblemarket=l$accessiblemarket)
      ##
      #* Also considered urban/rural based on population density 
      u <-read.csv(paste(dirprefix,'./lsms/district_code.csv',sep=""))
      u = data.frame(region=u$region,district=u$district,isurbanp=u$is_urban);
      
      adat<-read_tnz(paste(dirprefix,'./lsms/TZNPS2HH1DTA/HH_SEC_A.dta',sep=""),FALSE)
      
      a <- fu()@get_translated_frame(dat=adat,
                                     names=ln()@ohs_seca_columns_lsms_2010(),
                                     m=ln()@ohs_seca_mapping_lsms_2010())
      a<-merge(a,u)
      a<-merge(a,l)
      a$expensiveregion<-as.integer(is.element(a$region,get_expensiveregion_codes()))
      popDensity <- read.csv(paste(dirprefix,"./lsms/tnzpopdensity.csv",sep=""))
      a<-merge(a,popDensity)
      
      #*    Read section B
      bdat<-read_tnz(paste(dirprefix,'./lsms/TZNPS2HH1DTA/HH_SEC_B.dta',sep=""),FALSE)
      b <- fu()@get_translated_frame(dat=bdat,
                                     names=ln()@ohs_info_columns_lsms_2010(),
                                     m=ln()@ohs_mapping_lsms_2010())
      
      
      b$hhid<-as.character(b$hhid)
      #* inferring occupation rank with occupation_mapping
      b<-merge(b,occupation_mapping())
      
      cdat<-read_tnz(paste(dirprefix,'./lsms/TZNPS2HH1DTA/HH_SEC_C.dta',sep=""),FALSE)
      #*    Read section C
      c <- fu()@get_translated_frame(dat=cdat,
                                     names=ln()@get_ohs_secc_columns_lsms_2010(),
                                     m=ln()@get_ohs_secc_fields_mapping_lsms_2010())
      c$hhid<-as.character(c$hhid)
      ab <- merge(a,b)
      ohs<-merge(ab,c)
      ohs$age <-2010-ohs$YOB
      #*    calculated age by subtracting YOB from 2010 (survey year)
      #*    read section J for housing data (rent, number of primary/secondary rooms)
      
      jdat <- read.dta(paste(dirprefix,'./lsms/TZNPS2HH1DTA/HH_SEC_J1.dta',sep=""),convert.factors=FALSE)
      j <- fu()@get_translated_frame(dat=jdat,
                                     names=ln()@get_lsms_secj_info_columns_2010(),
                                     m=ln()@get_lsms_secj_fields_mapping_2010())
      j$hhid <-as.character(j$hhid)
      j$roomsnum_secondary[is.na(j$roomsnum_secondary)]<-0
      j$houserent[is.na(j$houserent)]<-0
      j$roomsnum<-j$roomsnum_primary+j$roomsnum_secondary
      ohsj<-merge(ohs,j,all=TRUE)
      return(ohsj)
      
    }
    stop(paste("Year:",year," not supported"))
    
  }
  
  visible_categories<-function(year){
    #return(c("213","214","219","224","301","313","314"));
    #return(c("10101","10102"))#rice
    #return(c("218")) #bar soap 
    #return(c("224")) # radio/watch personal items repairs
    #return(c("211")) # toothbrush
    #return(c("10801","10802","10803","10804","10805","10806","10807","10808","10809","10810")) # meat
    #return(c("10701","10702","10703","10704"))
    #return(c("101")) # cigarettes/tobacco
    #return(c("11106","11107","11108")) # alcohol
    #return(c("218")) # donations
    #return(c("202"))# electricity
    #return(food_categories_lsms_2010());
    # return(c("313","314"))
    #return(c("313"))
    # 219 - Motor vehicle service, repair, or parts
    # 214 - Other personal products (shampoo, razor blades, cosmetics, hair products, etc.)
    # 301 - Carpets, rugs
    # 313 - Bride price
    # 314 - Funeral costs
    # 224 - repairs to personal items
    return (-1)
  }
  
  
  get_ignored_hhids<-function(hh,ohs,income){
    #* ignored 5 households with really high expenditure on marriage (more than reported annual income)
    ignoredhhids_adhoc<- c("0701006104006701","0702006012004001","0701021174002601","0702001125000103")
    #* ignored households with zero income (ensuring that not more than 2.5% number of households are ignored)
    ignoredhhids_zero_income <- unique(income[as.integer(income$yearly_pay)==0,]$hhid)
    ignored_threshold<-.025
    if( length(ignoredhhids_zero_income)/length(unique(income$hhid))>ignored_threshold){
      stop (paste("More than",ignored_threshold*100, "% hhids with zero income"))
    }
    print(paste("Ignored ",length(ignoredhhids_zero_income),"/",length(unique(income$hhid)),"(=",
                length(ignoredhhids_zero_income)/length(unique(income$hhid)),") households with zero income" ))
    ignored<-union(ignoredhhids_zero_income,ignoredhhids_adhoc)
    return(ignored)
  }
  
  
  occupation_mapping<-function(){
    
    occupation<-"AGRICULTURE/LIVESTOCK.......1
    FISHING.............2
    MINING..............3
    TOURISM.............4
    EMPLOYED:
    GOVERMENT...........5
    PARASTATAL..........6
    PRIVATE SECTOR......7
    NGO/RELIGIOUS.......8
    EMPLOYED(NOT
    AGRICULTURE):
    WITH EMPLOYEES......9
    WITHOUT EMPLOYEES..10
    UNPAID FAMILY
    WORK...............11
    PAID FAMILY WORK...12
    JOB SEEKERS........13
    STUDENT............14
    DISABLED...........15
    NO JOB.............16
    TOO YOUNG .........17"
    
    res<-"
    occupation        mpay      sdpay    n
    1           1  1187965.72  7055046.0 1141 (4)
    4           5  2906734.21  4783352.2  148 (8)
    6           7  1614594.39  5137868.2  385 (6)
    8           9 11107091.89 32746264.8   37 (10)
    9          10  4872580.65 13506098.7  309 (9)
    10         11  1820317.04  6415290.0   90 (7)
    11         12   456890.91  1220102.0   66 (3)
    12         13   450215.87   899453.6   21 (2)
    13         14    64989.34   214007.3   96 (1)
    15         16  1217024.62  2625432.6   65 (5)
    "
    occupations<-"STUDENT,14
    JOB_SEEKERS,13
    PAID_FAMILY_WORK,12
    AGRICULTURE_LIVESTOCK,1
    UNEMPLOYED,16
    PRIVATE_SECTOR,7
    GOVERMENT,5
    NON_AGR_WO_EMPLOYEES,10
    NON_AGR_W_EMPLOYEES,9"
    #(0m(12,13,14,17),1m(1,7,8,11,16),...2m(5,15),..,4m(6,10),...,9m(2),..,11m(9),)
    # based on the following, rank (or class) does not have a strong predictive power
    #12,13,14,17 <-poor/unqualified
    #1,7,8,11,16 <- middle
    #5,15,6,10<- upper
    # Following mappings to try:
    
    occupations<-c(14,13,12,1,16,7,5,10,9)
    # the following doesn't work well for the mean pay
    # rank doesn't have a predictive power for total expenditure
    r=NULL;
    r=rbind(r,data.frame(occupation=14,occupation_rank=0))
    r=rbind(r,data.frame(occupation=13,occupation_rank=0))
    r=rbind(r,data.frame(occupation=12,occupation_rank=0))
    r=rbind(r,data.frame(occupation=16,occupation_rank=0))
    r=rbind(r,data.frame(occupation=11,occupation_rank=0))
    r=rbind(r,data.frame(occupation=1,occupation_rank=0))
    r=rbind(r,data.frame(occupation=17,occupation_rank=1))
    r=rbind(r,data.frame(occupation=2,occupation_rank=1))
    r=rbind(r,data.frame(occupation=3,occupation_rank=1))
    r=rbind(r,data.frame(occupation=4,occupation_rank=1))
    r=rbind(r,data.frame(occupation=7,occupation_rank=2))
    r=rbind(r,data.frame(occupation=9,occupation_rank=2))
    r=rbind(r,data.frame(occupation=10,occupation_rank=2))
    r=rbind(r,data.frame(occupation=15,occupation_rank=2))
    
    r=rbind(r,data.frame(occupation=8,occupation_rank=3))
    r=rbind(r,data.frame(occupation=5,occupation_rank=3))
    r=rbind(r,data.frame(occupation=6,occupation_rank=3))
    
    return(r)
    
    #2(fishing)~9
    #3(mining)~5
    #6(parastatal)~10
    #8(religious)~16
    #15(disabled)~5
    #17(too young)~14
    # based on median, we have 1,14 as lowest (<0.1) (student or farmer - poor)
    #                          11,12,13,14,16 <.2 (family work or student or jobseeker - poor) 
    
    #                          .11 < (11) < .12 (unpaid family work also with farmer - poor)
    #                          .15 < (16) < .16 (no job - poor)
    #                          .17 < (13) < .18 (job seeker - poor)
    #                          .28 < (2,4) < .29 ( tourism, fishing - worker)  
    #                                          (3) ( mining - worker) forced
    #                          .4< (7,9) < .5 (private company - worker)
    #                          .41 < (7) <.42 
    #                          .47 < (9) < .48 ( company - worker )
    #                          .59 <(10) < .6  ( business worker)
    #                          .71 < (15) < .72 (ommitted or worker) 
    #                          1.0 < (8) < 1.01 (religious - business worker)
    #                           1.59 < (5) < 1.6 (govt - business worker)
    #                           2.37 <(6) < 2.38 (parastatal - business worker)
  }
  
  
  analyse_cj<-function(dirprefix,sl){
    cjdat<-read.dta(paste(dirprefix,'/./lsms/TZNPS2COMDTA/COMSEC_CJ.dta',sep=''),convert.factors = FALSE) 
    
    cj <- fu()@get_translated_frame(dat=cjdat, names=ohs_seccj_columns_lsms_2010(), m=ohs_seccj_mapping_lsms_2010())
    cj$factor<-as.integer(cj$lwp_unit==1)+as.integer(cj$lwp_unit==2)/1000.0+as.integer(cj$lwp_unit==3)+as.integer(cj$lwp_unit==4)/1000.0+as.integer(cj$lwp_unit==5)
    cj$lwp <-cj$lwp*cj$factor
    cj$price <cj$price/cj$lwp
    
    if (missing(sl)){
      sl<-sort(unique(cj$item));
    }
    
    print (paste("sl=",sl))
    for (i in sl) {
      print(i);
      cjt<-cj[cj$item==i,]; 
      cjt<-cjt[!is.na(cjt$price) & cjt$price>0,];
      if(dim(cjt)[1]>0)
      {
        plot(cjt$r,cjt$price,xlab="region",ylab=paste("price for item=",i));
        View(cjt); 
        print(paste("Enter threshold for item=",i)); 
        m<-as.numeric(readline());
        if (m <= 0 || is.na(m)){
          stop ("Done")
        }
        print (paste("Using",m,"as threshold")); 
        x<-cjt[cjt$price<=max(cjt$price) & cjt$price > m,]; 
        print(paste(unique(x$item),unique(x$region),sep=","));
      } # end if
    } # end for
  }
  
  
  load_income_file<-function (year,dirprefix){
    
    #* read section E
    idat1 <-read_tnz(paste(dirprefix,'./lsms/./TZNPS2HH1DTA/HH_SEC_E1.dta',sep=""),FALSE)
    idat2 <-read_tnz(paste(dirprefix,'./lsms/./TZNPS2HH1DTA/HH_SEC_E2.dta',sep=""),FALSE)
    i1 <- fu()@get_translated_frame(dat=idat1,
                                    names=ln()@get_lsms_sece1_columns_2010(),
                                    m=ln()@get_lsms_sece_fields_mapping_2010())
    #TODO: add the conversion into get_translated_frame functionality
    i1$hhid<-as.character(i1$hhid)
    i2 <- fu()@get_translated_frame(dat=idat1,
                                    names=ln()@get_lsms_sece2_columns_2010(),
                                    m=ln()@get_lsms_sece_fields_mapping_2010())
    i2$hhid<as.character(i2$hhid)
    #TODO: add the conversion into get_translated_frame functionality
    
    ti <- ln()@infer_lsms_sece_total_income(i1,i2);
    #* inferred section e data
    #* ))
    # idat2 has only got self-employment details
    
    return(ti)
  }
  
  
  merge_hh_ohs_income_data<-function(hh,ohs,income,year,selected_category,set_depvar){
    if (year == 2010) {
      if (!is.integer(ohs$household_status)|| !(is.integer(ohs$highest_educ))){
        stop("factors must be converted an integer")
      }
      print ("Calculating visible expenditures")
      print(paste("Total number of households to search for visible consumption=",length(unique(hh$hhid))))
      vis<-fu()@filter_categories_data(hh=hh,selected_category = selected_category, item_field = "item", set_depvar=set_depvar)
      print(paste("Number of households with visible expenditure = ",length(unique(vis$hhid))))
      print("Calculating total expenditures")
      totexp<-ddply(hh,.(hhid),summarize,total_expenditure=sum(cost))
      print(paste("Number of households with total expenditure data minus housing = ",length(unique(totexp$hhid))))
      
      # obtain map (hhid->housing) with ddply
      tothouserent<-ddply(ohs,.(hhid),summarize,tothouserent=sum(houserent))
      # obtain map (hhid->educexpen) with ddply 
      print(paste("Number of households with houserent data = ",length(unique(tothouserent$hhid))))
      
      print ("Appending educexpense and houserent to total expenditure");
      #* setting houses with education exenses= NA as zero
      ohs$educexpense[is.na(ohs$educexpense)]<-0
      toteducexpense<-ddply(ohs,.(hhid),summarize,toteducexpense=sum(educexpense))
      print(paste("Number of households with educexpense data = ",length(unique(toteducexpense$hhid))))
      
      totexp<-merge(totexp,toteducexpense)
      print(paste("Number of households after merging total_expenditure and total_educexpense = ",length(unique(totexp$hhid))))
      
      totexp<-merge(totexp,tothouserent)
      print(paste("Number of households after merging total_expenditure with houserent = ",length(unique(totexp$hhid))))
      
      totexp$total_expenditure=totexp$total_expenditure+totexp$tothouserent+totexp$toteducexpense
      #totexp$tothouserent<-NULL
      #totexp$toteducexpense<-NULL
      
      #* calculating educational expense and total houserent
      print(paste("Number of households with total expenditure data = ",length(unique(totexp$hhid))))
      #* finding personids of the house-heads and their education-level, age, years in community, 
      #* language, occupation and 
      #* other household characteristics
      
      heads<-ohs[as.integer(ohs$household_status)==1,]
      print(paste("Number of houesehold heads = ",length(unique(heads$hhid))))
      
      heads<-data.frame(hhid=heads$hhid,
                        highest_educ=heads$highest_educ,
                        age=heads$age,
                        region=heads$region,
                        expensiveregion=heads$expensiveregion,
                        popdensity =heads$popdensity,
                        district=heads$district,
                        ward=heads$ward,
                        ea=heads$ea,
                        personid=heads$personid,
                        litlang=heads$litlang,
                        isrural=heads$isrural,
                        isurbanp=heads$isurbanp,
                        accessiblemarket=heads$accessiblemarket,
                        travelcost=heads$travelcost,
                        schoolowner=heads$schoolowner,
                        occupation = heads$occupation,
                        occupation_rank = heads$occupation_rank,
                        years_community=heads$years_community,
                        housingstatus=heads$housingstatus,
                        roomsnum=heads$roomsnum,
                        stringsAsFactors=FALSE);
      print(
        paste("Total number of households with head_highest_education=NA : ",
              dim(heads[is.na(heads$highest_educ),])[1]
        )
      );
      #heads$highest_educ[is.na(heads$highest_educ)]<-1
      print("Setting members with years_community=99 as their age");
      heads$is_resident<-as.integer(as.integer(heads$years_community)==99)
      heads$years_community<-heads$years_community+heads$is_resident*(heads$age-99);
      
      
      
      print ("Calculating hsize")
      hhid_personid_consu<-data.frame(hhid=ohs$hhid,personid=ohs$personid,age=ohs$age,stringsAsFactors=FALSE);
      szNonIgnoredAgeWithNAs<- dim(hhid_personid_consu)[1]
      szIgnoredAgeWithNAs<-dim(hhid_personid_consu[is.na(hhid_personid_consu$age),])[1]
      hhid_personid_consu<-hhid_personid_consu[!is.na(hhid_personid_consu$age),]
      print (paste("Ignored ", szIgnoredAgeWithNAs ," (out of ",szNonIgnoredAgeWithNAs ,") ohs entries with no age (cursz=",
                   dim(hhid_personid_consu)[1],")"))
      
      #calculating the consumption_factor
      
      hhid_personid_consu$consumption_factor<-as.integer(hhid_personid_consu$age<=5)*.2+as.integer(hhid_personid_consu$age>5 & hhid_personid_consu$age<=10)*.3+as.integer(hhid_personid_consu$age>10 & hhid_personid_consu$age<=15)*.4+as.integer(hhid_personid_consu$age>15 & hhid_personid_consu$age<=45)+as.integer(hhid_personid_consu$age>45 & hhid_personid_consu$age<=65)*.7+as.integer(hhid_personid_consu$age>65)*.6
      
      hhid_personid<- ddply(hhid_personid_consu,.(hhid),summarize,hsize=length(personid), consu=sum(consumption_factor));
      print(paste("Number of households with hsize data = ",length(unique(hhid_personid$hhid))))
      
      print("Merging visual expenditure")
      ds <-merge(totexp,vis);
      print(paste("Number of households after merging total expenditure with visible expenditure= ",length(unique(ds$hhid))))
      print(paste("Merging hsize",dim(ds)[1]))
      
      ds <-merge(ds,hhid_personid);
      print(paste("Number of households after merging resultant with hsize data= ",length(unique(ds$hhid))))
      
      ds<-merge(ds,heads)
      print(paste("Number of households after merging resultant with household head data = ",length(unique(ds$hhid))))
      
      #print(paste("Merging income",dim(ds)[1]))
      #ds<-merge(ds,income)
      print(paste("personid range:",toString(unique(ds$personid))))
      ds$personid<-NULL
      return(ds)
    }
    stop(paste("merge not available for year:",year))
  }
  
  combined_data_set<-function(year,dirprefix,selected_category,isDebug, set_depvar){
    
    ############ PHASE 0 ########################
    if (missing(set_depvar)){
      set_depvar = TRUE 
    }
    if (missing(selected_category)){
      print("setting selected_category to the default value")
      selected_category= visible_categories(year=year)
    } else {
      print(paste("using selected_category:",toString(selected_category)));
    }
    
    #*  (( Loading diary file
    hhdat <- load_diary_file(dirprefix=dirprefix,year=year) # must provide total and visible expenditure (must be already translated)
    
    #* Loading the person/family data fie
    ohsdat <-load_ohs_file(dirprefix=dirprefix,year=year) # (using fmld) must provide age (age_ref), gender(sex_ref), 
    # highest_educ(educ_ref), ishead(no_earnr,earncomp - all reference person data),
    # race(ref_race),family size (fam_size),
    # area_type (popsize,bls_urbn)
    #* Loading income file
    incomedat <-load_income_file(dirprefix=dirprefix,year=year) # must provide total income (fincaftm)
    
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
    
    ignored_hhids <- get_ignored_hhids(hhdat,ohsdat,incomedat);
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
    dstruct<-merge_hh_ohs_income_data(hh=hh,ohs=ohs,income=income,year=year,selected_category=selected_category,set_depvar=set_depvar);
    return(dstruct);
    #* ))
  }
  

  return(new("LSMSLoader",combined_data_set=combined_data_set,load_diary_file=load_diary_file, 
             analyse_cj=analyse_cj,load_ohs_file=load_ohs_file))
  
}