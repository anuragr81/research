

if (isClass("USCEXNormalize")){
  print ("Warning !!! previous definition of USCEXNormalize would be overwritten.")
}

## all exported functions are declared here
setClass("USCEXNormalize", representation(diary_info_columns_us_cex_2004="function", 
                                          ohs_info_columns_us_cex_2004="function", 
                                          hh_us_cex_mapping_2004="function", 
                                          ohs_mapping_us_cex_2004="function", 
                                          visible_categories_us_cex_2004="function", 
                                          cex_combined_years_ds="function",
                                          ucc_codes_2009="function",
                                          cex_groups="function"))


us_cex_normalize<-function () {
  
  diary_info_columns_us_cex_2004<-function(){
    return(c("hhid","cost","ucc","alloc"));
  }
  
  
  ohs_info_columns_us_cex_2004<-function(){
    return(c("hhid","age","gender","educ","race","hsize","income","horref1","urban_rural","popsize","highest_educ","housing_type"))
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
    s= rbind(s,data.frame(iesname="educ_ref",name="highest_educ"))
    s= rbind(s,data.frame(iesname="ref_race",name="race"))
    s= rbind(s,data.frame(iesname="horref1",name="horref1"))
    s= rbind(s,data.frame(iesname="fam_size",name="hsize"))
    s= rbind(s,data.frame(iesname="fincaftm",name="income"))
    s= rbind(s,data.frame(iesname="popsize",name="popsize"))
    s= rbind(s,data.frame(iesname="bls_urbn",name="urban_rural"))
    s= rbind(s,data.frame(iesname="descrip",name="housing_type"))
    
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
  
  
  ucc_codes_2009<-function() { 
    r=data.frame(longname=NULL,shortname=NULL,ucc=NULL)
    
    r=rbind(r,data.frame(longname='STOCKS; BONDS; MUTUAL FUNDS',shortname='stocksbonds',ucc='1000'))
    r=rbind(r,data.frame(longname='PRECIOUS METALS',shortname='preciousmetals',ucc='1100'))
    r=rbind(r,data.frame(longname='MISCELLANEOUS INVESTMENTS',shortname='miscinvestments',ucc='1200'))
    r=rbind(r,data.frame(longname='EMPLOY. COUNSELING & FEES',shortname='employmentcounseling',ucc='1400'))
    r=rbind(r,data.frame(longname='SAVINGS ACCOUNT DEPOSIT',shortname='savingsdeposit',ucc='2000'))
    r=rbind(r,data.frame(longname='INSUR. OTH THAN HEALTH/VEHICLE',shortname='insurance',ucc='2100'))
    r=rbind(r,data.frame(longname='RETIREMENT PLANS',shortname='retirementplans',ucc='2200'))
    r=rbind(r,data.frame(longname='CONTRIBUTIONS',shortname='contributions',ucc='4000'))
    r=rbind(r,data.frame(longname='CASH GIFTS',shortname='cashgifts',ucc='4100'))
    r=rbind(r,data.frame(longname='GIFTS NOT SPECIFIED',shortname='gifts_unpecified',ucc='4190'))
    r=rbind(r,data.frame(longname='ALIMONY AND CHILD SUPPORT',shortname='alimonychildsupport' ,ucc='5000'))
    r=rbind(r,data.frame(longname='MORTGAGE PAYMENT',shortname='mortgage',ucc='9000'))
    r=rbind(r,data.frame(longname='PROPERTY ASSESSMENT',shortname='propertyassessment',ucc='9900'))
    r=rbind(r,data.frame(longname='FLOUR',shortname='flour',ucc='10110'))
    r=rbind(r,data.frame(longname='PREPARED FLOUR MIXES',shortname='flour_prepared',ucc='10120'))
    r=rbind(r,data.frame(longname='CEREAL',shortname='cereal',ucc='10210'))
    r=rbind(r,data.frame(longname='RICE',shortname='rice',ucc='10310'))
    r=rbind(r,data.frame(longname='PASTA CORNMEAL OTH CEREAL PRODS',shortname='pasta_cornmeal',ucc='10320'))
    r=rbind(r,data.frame(longname='WHITE BREAD',shortname='whitebread',ucc='20110'))
    r=rbind(r,data.frame(longname='BREAD OTHER THAN WHITE',shortname='breadnonwhite',ucc='20210'))
    r=rbind(r,data.frame(longname='FRESH BISCUITS; ROLLS; MUFFINS',shortname='biscuitsrollsmuffins',ucc='20310'))
    r=rbind(r,data.frame(longname='CAKES AND CUPCAKES',shortname='cakes_nonfrozen',ucc='20410'))
    r=rbind(r,data.frame(longname='COOKIES',shortname='cookies',ucc='20510'))
    r=rbind(r,data.frame(longname='CRACKERS',shortname='crackers',ucc='20610'))
    r=rbind(r,data.frame(longname='BREAD AND CRACKER PRODUCTS',shortname='breadcracker',ucc='20620'))
    r=rbind(r,data.frame(longname='DOUGHNUTS;SWEETROLLS;COFFECAKE',shortname='bakery_excake',ucc='20710'))
    r=rbind(r,data.frame(longname='FROZEN & REFRIG. BAKERY PROD.',shortname='bakery_frozen',ucc='20810'))
    r=rbind(r,data.frame(longname='FRESH PIES; TARTS; TURNOVERS',shortname='piestarts',ucc='20820'))
    r=rbind(r,data.frame(longname='GROUND BEEF EXCLUDE CANNED',shortname='groundbeef_noncanned',ucc='30110'))
    r=rbind(r,data.frame(longname='CHUCK ROAST',shortname='chuckroast_noncanned',ucc='30210'))
    r=rbind(r,data.frame(longname='ROUND ROAST',shortname='roundroast_noncanned',ucc='30310'))
    r=rbind(r,data.frame(longname='OTHER ROAST',shortname='otherbeefroast_noncanned',ucc='30410'))
    r=rbind(r,data.frame(longname='ROUND STEAK',shortname='roundsteak_noncanned',ucc='30510'))
    r=rbind(r,data.frame(longname='SIRLOIN STEAK',shortname='sirloinsteak_noncanned',ucc='30610'))
    r=rbind(r,data.frame(longname='OTHER STEAK',shortname='steak_other_noncanned',ucc='30710'))
    r=rbind(r,data.frame(longname='OTHER BEEF (EXCLUDE CANNED)',shortname='beef_other_noncanned',ucc='30810'))
    r=rbind(r,data.frame(longname='BACON',shortname='bacon',ucc='40110'))
    r=rbind(r,data.frame(longname='PORK CHOPS',shortname='porkchops',ucc='40210'))
    r=rbind(r,data.frame(longname='HAM (EXCLUDE CANNED)',shortname='ham_noncanned',ucc='40310'))
    r=rbind(r,data.frame(longname='OTHER PORK',shortname='otherpork_noncanned',ucc='40410'))
    r=rbind(r,data.frame(longname='PORK SAUSAGE',shortname='porksausage_noncanned',ucc='40510'))
    r=rbind(r,data.frame(longname='CANNED HAM',shortname='ham_canned',ucc='40610'))
    r=rbind(r,data.frame(longname='FRANKFURTERS',shortname='frankfurters_noncanned',ucc='50110'))
    r=rbind(r,data.frame(longname='BOLOGNA; LIVERWURST; SALAMI',shortname='salami_noncanned',ucc='50210'))
    r=rbind(r,data.frame(longname='OTHER LUNCHMEAT',shortname='otherlunchmeat',ucc='50310'))
    r=rbind(r,data.frame(longname='LAMB AND ORGAN MEATS',shortname='lamb_noncanned',ucc='50410'))
    r=rbind(r,data.frame(longname='MUTTON; GOAT; GAME',shortname='mutton_goat_game',ucc='50900'))
    r=rbind(r,data.frame(longname='FRESH & FROZEN WHOLE CHICKEN',shortname='chicken',ucc='60110'))
    r=rbind(r,data.frame(longname='FRESH OR FROZEN CHICKEN PARTS',shortname='chicken_parts',ucc='60210'))
    r=rbind(r,data.frame(longname='OTHER POULTRY',shortname='poultry_other',ucc='60310'))
    r=rbind(r,data.frame(longname='CANNED FISH AND SEAFOOD',shortname='seafood_canned',ucc='70110'))
    r=rbind(r,data.frame(longname='FRESH FISH & SHELLFISH',shortname='fish_fresh',ucc='70230'))
    r=rbind(r,data.frame(longname='FROZEN FISH & SHELLFISH',shortname='fish_frozen',ucc='70240'))
    r=rbind(r,data.frame(longname='EGGS',shortname='eggs',ucc='80110'))
    r=rbind(r,data.frame(longname='FRESH MILK ALL TYPES',shortname='milk_fresh',ucc='90110'))
    r=rbind(r,data.frame(longname='CREAM',shortname='cream',ucc='90210'))
    r=rbind(r,data.frame(longname='BUTTER',shortname='butter',ucc='100110'))
    r=rbind(r,data.frame(longname='CHEESE',shortname='cheese',ucc='100210'))
    r=rbind(r,data.frame(longname='ICE CREAM AND RELATED PRODUCTS',shortname='icecream',ucc='100410'))
    r=rbind(r,data.frame(longname='OTHER DAIRY PRODUCTS',shortname='dairy_other',ucc='100510'))
    r=rbind(r,data.frame(longname='APPLES',shortname='apples',ucc='110110'))
    r=rbind(r,data.frame(longname='BANANAS',shortname='bananas',ucc='110210'))
    r=rbind(r,data.frame(longname='ORANGES',shortname='oranges',ucc='110310'))
    r=rbind(r,data.frame(longname='OTHER FRESH FRUITS',shortname='freshfruits_other',ucc='110410'))
    r=rbind(r,data.frame(longname='CITRUS FRUITS EXCL. ORANGES',shortname='fruites_citrus_non_orange',ucc='110510'))
    r=rbind(r,data.frame(longname='POTATOES',shortname='potatoes',ucc='120110'))
    r=rbind(r,data.frame(longname='LETTUCE',shortname='lettuce',ucc='120210'))
    r=rbind(r,data.frame(longname='TOMATOES',shortname='tomatoes',ucc='120310'))
    r=rbind(r,data.frame(longname='OTHER FRESH VEGETABLES',shortname='freshvegetables_other',ucc='120410'))
    r=rbind(r,data.frame(longname='FROZEN ORANGE JUICE',shortname='orangejuice_frozen',ucc='130110'))
    r=rbind(r,data.frame(longname='FROZEN FRUITS',shortname='fruits_frozen',ucc='130121'))
    r=rbind(r,data.frame(longname='FROZEN FRUIT JUICES',shortname='fruitjuices_frozen',ucc='130122'))
    r=rbind(r,data.frame(longname='FRESH FRUIT JUICE',shortname='fruitjuices_fresh',ucc='130211'))
    r=rbind(r,data.frame(longname='CANNED/BOTTLE FRUIT JUICE',shortname='fruitjuices_bottled',ucc='130212'))
    r=rbind(r,data.frame(longname='CANNED FRUITS',shortname='fruits_canned',ucc='130310'))
    r=rbind(r,data.frame(longname='DRIED FRUITS',shortname='driedfruits',ucc='130320'))
    r=rbind(r,data.frame(longname='FROZEN VEGETABLES',shortname='vegetables_frozen',ucc='140110'))
    r=rbind(r,data.frame(longname='CANNED BEANS',shortname='beans_canned',ucc='140210'))
    r=rbind(r,data.frame(longname='CANNED CORN',shortname='corn_canned',ucc='140220'))
    r=rbind(r,data.frame(longname='CANNED VEGETABLES MISC',shortname='miscvegetables_canned',ucc='140230'))
    r=rbind(r,data.frame(longname='OTHER PROCESSED VEGETABLES',shortname='vegetables_otherprocessed',ucc='140310'))
    r=rbind(r,data.frame(longname='OTHER PEAS',shortname='peas_dried',ucc='140320'))
    r=rbind(r,data.frame(longname='OTHER BEANS',shortname='beans_dried',ucc='140330'))
    r=rbind(r,data.frame(longname='OTHER VEGETABLES MISC',shortname='carrots_onions_greens_cabbage',ucc='140340'))
    r=rbind(r,data.frame(longname='FROZEN VEGETABLE JUICES',shortname='vegetablejuices_frozen',ucc='140410'))
    r=rbind(r,data.frame(longname='FRESH & CANNED VEGETABLE JUICES',shortname='vegetablejuices',ucc='140420'))
    r=rbind(r,data.frame(longname='CANDY AND CHEWING GUM',shortname='candy',ucc='150110'))
    r=rbind(r,data.frame(longname='SUGAR',shortname='sugar',ucc='150211'))
    r=rbind(r,data.frame(longname='ARTIFICIAL SWEETENERS',shortname='artificialsweeteners',ucc='150212'))
    r=rbind(r,data.frame(longname='OTHER SWEETS',shortname='jams_jellies',ucc='150310'))
    r=rbind(r,data.frame(longname='MARGARINE',shortname='margarine',ucc='160110'))
    r=rbind(r,data.frame(longname='FATS & OILS',shortname='fatsoils',ucc='160211'))
    r=rbind(r,data.frame(longname='SALAD DRESSINGS',shortname='saladdressings',ucc='160212'))
    r=rbind(r,data.frame(longname='NON-DIARY CREAM SUBSTITUTES',shortname='nondairycreamsubstitutes',ucc='160310'))
    r=rbind(r,data.frame(longname='PEANUT BUTTER',shortname='peanutbutter',ucc='160320'))
    r=rbind(r,data.frame(longname='COLA DRINKS',shortname='coladrinks',ucc='170110'))
    r=rbind(r,data.frame(longname='OTHER CARBONATED DRINKS',shortname='carbonateddrinks_other',ucc='170210'))
    r=rbind(r,data.frame(longname='ROASTED COFFEE',shortname='coffee_roasted',ucc='170310'))
    r=rbind(r,data.frame(longname='INSTANT/FREEZE DRIED COFFEE',shortname='coffee_instant',ucc='170410'))
    r=rbind(r,data.frame(longname='NONCARB FRUT FLAV/LEMADE NONFROZ',shortname='noncarbonatedfruitflavoreddrinks',ucc='170510'))
    r=rbind(r,data.frame(longname='TEA',shortname='tea',ucc='170520'))
    r=rbind(r,data.frame(longname='OTHER NONCARB. BEVERAGES/ICE',shortname='drinks_other_noncarbonated',ucc='170530'))
    r=rbind(r,data.frame(longname='OTHER NONCARB. BEVERAGES/ICE',shortname='drinks_other_noncarbonated',ucc='170531'))
    r=rbind(r,data.frame(longname='BOTTLED WATER',shortname='bottled_water',ucc='170532'))
    r=rbind(r,data.frame(longname='SPORTS DRINKS',shortname='drinks_sports',ucc='170533'))
    r=rbind(r,data.frame(longname='SOUP',shortname='soup',ucc='180110'))
    r=rbind(r,data.frame(longname='FROZEN MEALS',shortname='frozenmeals',ucc='180210'))
    r=rbind(r,data.frame(longname='FROZ/PREP. FOOD OTH THAN MEALS',shortname='frozenpreparedfood_other',ucc='180220'))
    r=rbind(r,data.frame(longname='POTATO CHIPS AND OTHER SNACKS',shortname='potatochipssnacks',ucc='180310'))
    r=rbind(r,data.frame(longname='NUTS',shortname='nuts',ucc='180320'))
    r=rbind(r,data.frame(longname='SALT/OTHER SEASONINGS & SPICES',shortname='saltseasoningsspices',ucc='180410'))
    r=rbind(r,data.frame(longname='OLIVES; PICKLES; RELISHES',shortname='olivespickles',ucc='180420'))
    r=rbind(r,data.frame(longname='SAUCES AND GRAVIES',shortname='sauces',ucc='180510'))
    r=rbind(r,data.frame(longname='OTHER CONDIMENTS',shortname='condiments_other',ucc='180520'))
    r=rbind(r,data.frame(longname='PREPARED SALADS',shortname='preparedsalads',ucc='180611'))
    r=rbind(r,data.frame(longname='PREPARED DESSERTS',shortname='prepareddesserts',ucc='180612'))
    r=rbind(r,data.frame(longname='BABY FOOD',shortname='babyfood',ucc='180620'))
    r=rbind(r,data.frame(longname='MISC. PREPARED FOODS',shortname='miscpreparedfoods',ucc='180710'))
    r=rbind(r,data.frame(longname='VITAMIN SUPPLEMENT',shortname='vitaminsupplements',ucc='180720'))
    r=rbind(r,data.frame(longname='LUNCH AT FAST FOOD',shortname='lunch_atfastfood',ucc='190111'))
    r=rbind(r,data.frame(longname='LUNCH AT FULL SERVICE',shortname='lunch_atfullservice',ucc='190112'))
    r=rbind(r,data.frame(longname='LUNCH AT VENDING MACHINE',shortname='lunch_atvendingmachine',ucc='190113'))
    r=rbind(r,data.frame(longname='LUNCH AT EMPLOYER',shortname='lunch_atemployer',ucc='190114'))
    r=rbind(r,data.frame(longname='LUNCH AT BOARD',shortname='lunch_atboard',ucc='190115'))
    r=rbind(r,data.frame(longname='LUNCH AT CATERED AFFAIRS',shortname='lunch_atcatered',ucc='190116'))
    r=rbind(r,data.frame(longname='DINNER AT FAST FOOD',shortname='dinner_atfastfood',ucc='190211'))
    r=rbind(r,data.frame(longname='DINNER AT FULL SERVICE',shortname='dinner_atfullservice',ucc='190212'))
    r=rbind(r,data.frame(longname='DINNER AT VENDING MACHINE',shortname='dinner_atvendingmachine',ucc='190213'))
    r=rbind(r,data.frame(longname='DINNER AT EMPLOYER',shortname='dinner_atemployer',ucc='190214'))
    r=rbind(r,data.frame(longname='DINNER AT BOARD',shortname='dinner_atboard',ucc='190215'))
    r=rbind(r,data.frame(longname='DINNER AT CATERED AFFAIRS',shortname='dinner_atcatered',ucc='190216'))
    r=rbind(r,data.frame(longname='SNACKS AT FAST FOOD',shortname='snacks_atfastfood',ucc='190311'))
    r=rbind(r,data.frame(longname='SNACKS AT FULL SERVICE',shortname='snacks_atfullservice',ucc='190312'))
    r=rbind(r,data.frame(longname='SNACKS AT VEND MACHINE',shortname='snacks_atvendmachine',ucc='190313'))
    r=rbind(r,data.frame(longname='SNACKS AT EMPLOYER',shortname='snacks_atemployer',ucc='190314'))
    r=rbind(r,data.frame(longname='SNACKS AT BOARD',shortname='snacks_atboard',ucc='190315'))
    r=rbind(r,data.frame(longname='SNACKS AT CATERED AFFAIRS',shortname='snacks_atcatered',ucc='190316'))
    r=rbind(r,data.frame(longname='BREAKFAST AT FAST FOOD',shortname='breakfast_atfastfood',ucc='190321'))
    r=rbind(r,data.frame(longname='BREAKFAST AT FULL SERVICE',shortname='breakfast_atfullservice',ucc='190322'))
    r=rbind(r,data.frame(longname='BREAKFAST AT VENDING MACHINE',shortname='breakfast_atvendingmachine',ucc='190323'))
    r=rbind(r,data.frame(longname='BREAKFAST AT EMPLOYER',shortname='breakfast_atemployer',ucc='190324'))
    r=rbind(r,data.frame(longname='BREAKFAST AT BOARD',shortname='breakfast_atboard',ucc='190325'))
    r=rbind(r,data.frame(longname='BREAKFAST AT CATERED AFFAIRS',shortname='breakfast_atcatered',ucc='190326'))
    r=rbind(r,data.frame(longname='BOARD AT FAST FOOD',shortname='board_atfastfood',ucc='190911'))
    r=rbind(r,data.frame(longname='BOARD AT FULL SERVICE',shortname='board_atfullservice',ucc='190912'))
    r=rbind(r,data.frame(longname='BOARD AT VENDING MACHINE',shortname='board_atvendingmachine',ucc='190913'))
    r=rbind(r,data.frame(longname='BOARD AT EMPLOYER',shortname='board_atemployer',ucc='190914'))
    r=rbind(r,data.frame(longname='BOARD AT BOARD',shortname='board',ucc='190915'))
    r=rbind(r,data.frame(longname='BOARD AT CATERED AFFAIRS',shortname='board_atcatered',ucc='190916'))
    r=rbind(r,data.frame(longname='CATERED AFF AT FAST FOOD',shortname='catered_atfastfood',ucc='190921'))
    r=rbind(r,data.frame(longname='CATERED AFF AT FULL SERVICE',shortname='catered_atfullservice',ucc='190922'))
    r=rbind(r,data.frame(longname='CATERED AFF AT VEND MACHINE',shortname='catered_atvendingmachine',ucc='190923'))
    r=rbind(r,data.frame(longname='CATERED AFF AT EMPLOYER',shortname='catered_atemployer',ucc='190924'))
    r=rbind(r,data.frame(longname='CATERED AFF AT BOARD',shortname='catered_atboard',ucc='190925'))
    r=rbind(r,data.frame(longname='CATERED AFF AT CATERED AFF',shortname='catered',ucc='190926'))
    r=rbind(r,data.frame(longname='BEER AND ALC AT HOME',shortname='beer_athome',ucc='200111'))
    r=rbind(r,data.frame(longname='NON ALCOHOLIC BEER',shortname='nonalcoholicbeer',ucc='200112'))
    r=rbind(r,data.frame(longname='WHISKEY AT HOME',shortname='whiskey_athome',ucc='200210'))
    r=rbind(r,data.frame(longname='WINE AT HOME',shortname='wine_athome',ucc='200310'))
    r=rbind(r,data.frame(longname='OTHER ALCOHOLIC BEV. AT HOME',shortname='alcohol_other_athome',ucc='200410'))
    r=rbind(r,data.frame(longname='BEER AT FAST FOOD',shortname='beer_atfastfood',ucc='200511'))
    r=rbind(r,data.frame(longname='BEER AT FULL SERVICE',shortname='beer_atfullservice',ucc='200512'))
    r=rbind(r,data.frame(longname='BEER AT VENDING MACHINE',shortname='beer_atvendingmachine',ucc='200513'))
    r=rbind(r,data.frame(longname='BEER AT EMPLOYER',shortname='beer_atemployer',ucc='200514'))
    r=rbind(r,data.frame(longname='BEER AT BOARD',shortname='beer_atboard',ucc='200515'))
    r=rbind(r,data.frame(longname='BEER AT CATERED AFFAIRS',shortname='beer_atcatered',ucc='200516'))
    r=rbind(r,data.frame(longname='WINE AT FAST FOOD',shortname='wine_atfastfood',ucc='200521'))
    r=rbind(r,data.frame(longname='WINE AT FULL SERVICE',shortname='wine_atfullservice',ucc='200522'))
    r=rbind(r,data.frame(longname='WINE AT VENDING MACHINE',shortname='wine_atvendingmachine',ucc='200523'))
    r=rbind(r,data.frame(longname='WINE AT EMPLOYER',shortname='wine_atemployer',ucc='200524'))
    r=rbind(r,data.frame(longname='WINE AT BOARD',shortname='wine_atboard',ucc='200525'))
    r=rbind(r,data.frame(longname='WINE AT CATERED AFFAIRS',shortname='wine_atcatered',ucc='200526'))
    r=rbind(r,data.frame(longname='ALC. BEV EXC BEER/WINE FAST FD',shortname='alcoholex_atfastfood',ucc='200531'))
    r=rbind(r,data.frame(longname='ALC. BEV EXC B/W FULL SERV',shortname='alcoholexfullservice',ucc='200532'))
    r=rbind(r,data.frame(longname='ALC. BEV B/W VEND MACH',shortname='alcoholexvendingmachine',ucc='200533'))
    r=rbind(r,data.frame(longname='ALC BEV EXC B/W AT EMP',shortname='alcoholex_atemployer',ucc='200534'))
    r=rbind(r,data.frame(longname='ALC BEV EXC B/W AT BOARD',shortname='alcoholex_atboard',ucc='200535'))
    r=rbind(r,data.frame(longname='OTH ALC. BEV AWAY FROM HOME',shortname='alcoholex_caterer',ucc='200536'))
    r=rbind(r,data.frame(longname='RENT OF DWELLING',shortname='homerent',ucc='210110'))
    r=rbind(r,data.frame(longname='LODGING AWAY FROM HOME',shortname='lodgingawayfromhome',ucc='210210'))
    r=rbind(r,data.frame(longname='HOUSING FOR SOMEONE AT SCHOOL',shortname='housing_schoolkid',ucc='210310'))
    r=rbind(r,data.frame(longname='GROUND OR LAND RENT',shortname='landrent',ucc='210900'))
    r=rbind(r,data.frame(longname='CAPITAL IMPROVEMENTS - N/SPEC.',shortname='capitalimprovements_notspecified',ucc='220000'))
    r=rbind(r,data.frame(longname='FIRE/EXTENDED COVERAGE INSUR',shortname='fireinsurance',ucc='220110'))
    r=rbind(r,data.frame(longname='HOMEOWNERS   INSURANCE',shortname='homeownerinsurance',ucc='220120'))
    r=rbind(r,data.frame(longname='PROPERTY TAXES',shortname='propertytaxes',ucc='220210'))
    r=rbind(r,data.frame(longname='PURCHASE OF PROPERTY',shortname='purchaseofproperty',ucc='220400'))
    r=rbind(r,data.frame(longname='CAPITAL IMPROVEMENTS - COMMOD',shortname='capitalimprovements_commodities',ucc='220510'))
    r=rbind(r,data.frame(longname='CAPITAL IMPROVEMENTS - SERVICE',shortname='capitalimprovements_services',ucc='220610'))
    r=rbind(r,data.frame(longname='PARKING-OWNED DWELLING',shortname='parking_owneddwelling',ucc='220900'))
    r=rbind(r,data.frame(longname='REPAIR/MAINT/IMPROV. N/SPEC.',shortname='repair_maintenance_improvements',ucc='230000'))
    r=rbind(r,data.frame(longname='MAINTENANCE OF PROPERTY',shortname='propertymaintenance',ucc='230110'))
    r=rbind(r,data.frame(longname='INSTALLED HARD SURFACE FLOORIN',shortname='installedhardsurfaceflooring',ucc='230120'))
    r=rbind(r,data.frame(longname='INSTALLED WALL-TO-WALL CARPET',shortname='installedwalltowallcarpet',ucc='230130'))
    r=rbind(r,data.frame(longname='REPAIR-DISPL/DISHR/RANG HD',shortname='repairdisposal_dishwasher_rangehood',ucc='230140'))
    r=rbind(r,data.frame(longname='MAINTENANCE FEES',shortname='maintenancefeesassociation',ucc='230900'))
    r=rbind(r,data.frame(longname='PAINT; WALLPAPER AND SUPPLIES',shortname='paint_wallpapersupplies',ucc='240110'))
    r=rbind(r,data.frame(longname='TOOLS/EQUIP. FOR PAINTG;PAPERG',shortname='paintingequipment',ucc='240120'))
    r=rbind(r,data.frame(longname='LUBER;PANLING;TILE;AWNING;GLAS',shortname='paneling',ucc='240210'))
    r=rbind(r,data.frame(longname='BLACKTOP AND MASONRY MATERIALS',shortname='masonrymaterials',ucc='240220'))
    r=rbind(r,data.frame(longname='PLUMBING SUPPLIES AND EQUIP.',shortname='plumbingfixtures',ucc='240310'))
    r=rbind(r,data.frame(longname='ELEC HEATG/A.C. SUPP. EQUIP',shortname='electricheatingairconditioning',ucc='240320'))
    r=rbind(r,data.frame(longname='SOFT SURFACE FLOOR COVERING',shortname='softsurfacefloorcovering',ucc='240900'))
    r=rbind(r,data.frame(longname='FUEL OIL',shortname='fueloil',ucc='250110'))
    r=rbind(r,data.frame(longname='BOTTLED OR TANK GAS',shortname='bottledortankgas',ucc='250210'))
    r=rbind(r,data.frame(longname='COAL',shortname='coal',ucc='250220'))
    r=rbind(r,data.frame(longname='MISC. FUELS',shortname='miscfuels',ucc='250900'))
    r=rbind(r,data.frame(longname='ELECTRICITY',shortname='electricity',ucc='260110'))
    r=rbind(r,data.frame(longname='UTILITY - NATURAL GAS',shortname='utility_naturalgas',ucc='260210'))
    r=rbind(r,data.frame(longname='TELEPHONE SERVICE NOT SPEC.',shortname='telephoneservice',ucc='270000'))
    r=rbind(r,data.frame(longname='WATER AND SEWERAGE MAINTENANCE',shortname='waterseweragemaintenance',ucc='270210'))
    r=rbind(r,data.frame(longname='CABLE/SATELLITE/COM ANTENNA SERV',shortname='cable_satellite',ucc='270310'))
    r=rbind(r,data.frame(longname='SATELLITE RADIO SERVICE',shortname='radio_nonvehicle',ucc='270311'))
    r=rbind(r,data.frame(longname='GARBAGE/TRASH COLLECTION',shortname='trashcollection',ucc='270410'))
    r=rbind(r,data.frame(longname='SEPTIC TANK CLEANING',shortname='septictankcleaning',ucc='270900'))
    r=rbind(r,data.frame(longname='STEAM HEAT',shortname='steamheat',ucc='270905'))
    r=rbind(r,data.frame(longname='BATHROOM LINENS',shortname='bathroomlinens',ucc='280110'))
    r=rbind(r,data.frame(longname='BEDROOM LINENS',shortname='bedroomlinens',ucc='280120'))
    r=rbind(r,data.frame(longname='KITCHEN AND DINING ROOM LINENS',shortname='kitchendiningroomlinens',ucc='280130'))
    r=rbind(r,data.frame(longname='CURTAINS AND DRAPES',shortname='curtainsdrapes_exshower',ucc='280210'))
    r=rbind(r,data.frame(longname='SLIPCOVERS/DECORATIVE PILLOWS',shortname='coverscushions',ucc='280220'))
    r=rbind(r,data.frame(longname='SEWING MATERIALS',shortname='slipcovers_curtains_sewingmaterial',ucc='280230'))
    r=rbind(r,data.frame(longname='OTHER LINENS',shortname='linens_other',ucc='280900'))
    r=rbind(r,data.frame(longname='MATTRESS AND SPRINGS',shortname='mattress',ucc='290110'))
    r=rbind(r,data.frame(longname='OTHER BEDROOM FURNITURE',shortname='bedroomfurniture_other',ucc='290120'))
    r=rbind(r,data.frame(longname='SOFAS',shortname='sofas',ucc='290210'))
    r=rbind(r,data.frame(longname='LIVING ROOM CHAIRS',shortname='livingroomchairs',ucc='290310'))
    r=rbind(r,data.frame(longname='LIVING ROOM TABLES',shortname='livingroomtables',ucc='290320'))
    r=rbind(r,data.frame(longname='KITCHEN/DINING ROOM FURNITURE',shortname='kitchendiningroomfurniture',ucc='290410'))
    r=rbind(r,data.frame(longname='INFANTS FURNITURE',shortname='infantsfurniture',ucc='290420'))
    r=rbind(r,data.frame(longname='OUTDOOR FURNITURE',shortname='patiofurniture',ucc='290430'))
    r=rbind(r,data.frame(longname='WALL UNITS; CABINETS; OCCAS FURN',shortname='shelvesfurniture',ucc='290440'))
    r=rbind(r,data.frame(longname='REFRIGERATOR; HOME FREEZER',shortname='refrigerator',ucc='300110'))
    r=rbind(r,data.frame(longname='WASHERS',shortname='washers',ucc='300210'))
    r=rbind(r,data.frame(longname='DRYERS',shortname='dryers',ucc='300220'))
    r=rbind(r,data.frame(longname='STOVES; OVENS',shortname='stoves_ovens',ucc='300310'))
    r=rbind(r,data.frame(longname='MICROWAVE OVENS',shortname='microwave',ucc='300320'))
    r=rbind(r,data.frame(longname='PORTABLE DISHWASHERS',shortname='dishwasher_portable',ucc='300330'))
    r=rbind(r,data.frame(longname='WINDOW AIR CONDITIONERS',shortname='windowairconditioners',ucc='300410'))
    r=rbind(r,data.frame(longname='MISC. HOUSEHOLD APPLIANCES',shortname='mischouseholdappliances',ucc='300900'))
    r=rbind(r,data.frame(longname='BLACK AND WHITE TV',shortname='tv_bw',ucc='310110'))
    r=rbind(r,data.frame(longname='COLOR TV - CONSOLE',shortname='colortv',ucc='310120'))
    r=rbind(r,data.frame(longname='COLOR TV - PORTABLE/TABLE MOD',shortname='colortv_portable',ucc='310130'))
    r=rbind(r,data.frame(longname='TELEVISIONS',shortname='TV',ucc='310140'))
    r=rbind(r,data.frame(longname='VCRS/VIDEO DISC PLAYERS',shortname='videoplayers',ucc='310210'))
    r=rbind(r,data.frame(longname='VIDEO CASSETTES/TAPES/DISCS',shortname='disktapemedia',ucc='310220'))
    r=rbind(r,data.frame(longname='VIDEO/COMP GAME HARDWARE/SOFTWARE',shortname='consolesupplies',ucc='310230'))
    r=rbind(r,data.frame(longname='STREAMING VIDEO FILES',shortname='internet_video',ucc='310241'))
    r=rbind(r,data.frame(longname='DOWNLOADING VIDEO FILES',shortname='internet_video',ucc='310242'))
    r=rbind(r,data.frame(longname='RADIOS',shortname='radio_nonvehicle',ucc='310311'))
    r=rbind(r,data.frame(longname='PHONOGRAPHS',shortname='recordplayer',ucc='310312'))
    r=rbind(r,data.frame(longname='TAPE RECORDERS AND PLAYERS',shortname='taperecorder',ucc='310313'))
    r=rbind(r,data.frame(longname='DIGITAL AUDIO PLAYERS',shortname='audio_player',ucc='310314'))
    r=rbind(r,data.frame(longname='DIGITAL MEDIA PLAYERS & RECORDERS',shortname='audio_player',ucc='310315'))
    r=rbind(r,data.frame(longname='COMPONENTS/COMPONENT SYSTEMS',shortname='soundcomponents',ucc='310320'))
    r=rbind(r,data.frame(longname='MISC SOUND EQUIPMENT',shortname='miscsoundequipment',ucc='310331'))
    r=rbind(r,data.frame(longname='SOUND EQUIP ACCESSORIES',shortname='soundequipmentaccessories',ucc='310332'))
    r=rbind(r,data.frame(longname='SATELLITE DISHES',shortname='satellitedishes',ucc='310334'))
    r=rbind(r,data.frame(longname='MISC VIDEO EQUIPMENT',shortname='video_equipment',ucc='310335'))
    r=rbind(r,data.frame(longname='RECORDS;CDS;AUDIO TAPES',shortname='records_tapes',ucc='310340'))
    r=rbind(r,data.frame(longname='STREAMING AUDIO FILES',shortname='internet_audio',ucc='310351'))
    r=rbind(r,data.frame(longname='DOWNLOADING AUDIO FILES',shortname='internet_audio',ucc='310352'))
    r=rbind(r,data.frame(longname='ACCESS. FOR ELECTRONIC EQUIP.',shortname='electronicequipment',ucc='310900'))
    r=rbind(r,data.frame(longname='FLOOR COVERINGS (NON-PERM.)',shortname='roomsizerugs',ucc='320110'))
    r=rbind(r,data.frame(longname='WINDOW COVERINGS',shortname='venetianblinds',ucc='320120'))
    r=rbind(r,data.frame(longname='INFANTS EQUIPMENT',shortname='infantsequipment',ucc='320130'))
    r=rbind(r,data.frame(longname='LAUNDRY AND CLEANING EQUIP.',shortname='laundrycleaningequipment',ucc='320140'))
    r=rbind(r,data.frame(longname='BARBEQUE GRILLS AND OUTDOOR EQUIP',shortname='outdoorequipment',ucc='320150'))
    r=rbind(r,data.frame(longname='CLOCKS',shortname='clocks',ucc='320210'))
    r=rbind(r,data.frame(longname='LAMPS AND LIGHTING FIXTURES',shortname='lampslightingfixtures',ucc='320220'))
    r=rbind(r,data.frame(longname='OTH HOUSEHOLD DECORATIVE ITEMS',shortname='householddecorativeitems_other',ucc='320231'))
    r=rbind(r,data.frame(longname='TELEPHONES AND ACCESSORIES',shortname='telephone',ucc='320232'))
    r=rbind(r,data.frame(longname='CLOCKS AND OTHER HH DECOR ITEMS',shortname='clocks',ucc='320233'))
    r=rbind(r,data.frame(longname='PLASTIC DINNERWARE',shortname='dinnerware_plastic',ucc='320310'))
    r=rbind(r,data.frame(longname='CHINA AND OTHER DINNERWARE',shortname='dinnerware_china',ucc='320320'))
    r=rbind(r,data.frame(longname='FLATWARE',shortname='tableware',ucc='320330'))
    r=rbind(r,data.frame(longname='GLASSWARE',shortname='glassware',ucc='320340'))
    r=rbind(r,data.frame(longname='SILVER SERVING PIECES',shortname='servingpieces_silver',ucc='320350'))
    r=rbind(r,data.frame(longname='OTHER SERVING PIECES',shortname='servingpieces_nonsilver',ucc='320360'))
    r=rbind(r,data.frame(longname='NONELECTRIC COOKWARE',shortname='cookware_nonelectric',ucc='320370'))
    r=rbind(r,data.frame(longname='TABLEWARE/NON-ELEC. KITWARE',shortname='tableware_nonelectrickitchenware',ucc='320380'))
    r=rbind(r,data.frame(longname='LAWN AND GARDEN EQUIPMENT',shortname='lawnmowingequipment',ucc='320410'))
    r=rbind(r,data.frame(longname='POWER TOOLS',shortname='powertools',ucc='320420'))
    r=rbind(r,data.frame(longname='OTHER HARDWARE',shortname='otherhardware',ucc='320430'))
    r=rbind(r,data.frame(longname='ELECTRIC FLOOR CLEANING EQUIP',shortname='electricfloorcleaningequipment',ucc='320511'))
    r=rbind(r,data.frame(longname='SEWING MACHINES',shortname='sewingmachines',ucc='320512'))
    r=rbind(r,data.frame(longname='SMALL ELECTRIC KITCHEN APPLIANCES',shortname='electricalkitchenappliances_small',ucc='320521'))
    r=rbind(r,data.frame(longname='PORTABLE HEATING/COOLING EQUIP',shortname='portableheatingcoolingequipment',ucc='320522'))
    r=rbind(r,data.frame(longname='MISC. SUPPLIES AND EQUIPMENT',shortname='miscsequipmentsupplies',ucc='320610'))
    r=rbind(r,data.frame(longname='PERM HARD SURFACE FLR COVERING',shortname='permanenthardsurfacefloorcovering',ucc='320620'))
    r=rbind(r,data.frame(longname='LANDSCAPING ITEMS',shortname='landscapingitems',ucc='320630'))
    r=rbind(r,data.frame(longname='OFFICE FURNITURE HOME USE',shortname='officefurnitureforhome',ucc='320901'))
    r=rbind(r,data.frame(longname='HAND TOOLS',shortname='tools_nonpowered',ucc='320902'))
    r=rbind(r,data.frame(longname='INDOOR PLANTS; FRESH FLOWERS',shortname='pottedplants',ucc='320903'))
    r=rbind(r,data.frame(longname='CLOSET AND STORAGE ITEMS',shortname='homestorage',ucc='320904'))
    r=rbind(r,data.frame(longname='MISC. HOUSEHOLD EQUIP/PARTS',shortname='mischouseholdequipment',ucc='320905'))
    r=rbind(r,data.frame(longname='ELECTRONIC TESTING EQUIP.',shortname='electronictestingequipment',ucc='320906'))
    r=rbind(r,data.frame(longname='SOAPS AND DETERGENTS',shortname='soapsdetergents_exhandsoaps',ucc='330110'))
    r=rbind(r,data.frame(longname='OTHER LAUNDRY /CLEANING PRODS.',shortname='otherlaundrycleaningproducts',ucc='330210'))
    r=rbind(r,data.frame(longname='PAPER TOWELS/NAPKINS/TOILET TI',shortname='papertowels',ucc='330310'))
    r=rbind(r,data.frame(longname='STATIONERY; GIFTWRAP; ETC.',shortname='stationery',ucc='330410'))
    r=rbind(r,data.frame(longname='MISC HOUSEHOLD PRODUCTS',shortname='mischouseholdproducts',ucc='330510'))
    r=rbind(r,data.frame(longname='LAWN AND GARDEN SUPPLIES',shortname='gardensupplies',ucc='330610'))
    r=rbind(r,data.frame(longname='POSTAGE',shortname='postage',ucc='340110'))
    r=rbind(r,data.frame(longname='DELIVERY SERVICES',shortname='deliveryservices',ucc='340120'))
    r=rbind(r,data.frame(longname='BABYSITTING',shortname='homecareforchildren',ucc='340210'))
    r=rbind(r,data.frame(longname='DOMESTIC SERVICE',shortname='housekeepingservice',ucc='340310'))
    r=rbind(r,data.frame(longname='GARDENING/LAWN CARE SERVICE',shortname='gardeningservices',ucc='340410'))
    r=rbind(r,data.frame(longname='MOVING; STORAGE;FREIGHT',shortname='movingstorage',ucc='340510'))
    r=rbind(r,data.frame(longname='HSHLD LNDRY;DRYCLN NOT COIN-OP',shortname='nonclothinghouseholdlaundry',ucc='340520'))
    r=rbind(r,data.frame(longname='COIN-OP HSHLD LNDRY; DRY CLN',shortname='nonclothinghouseholdlaundry_coinoperated',ucc='340530'))
    r=rbind(r,data.frame(longname='REPAIR OF TV/RADIO/SOUND EQUIP',shortname='tvradioexcarfittedrepair',ucc='340610'))
    r=rbind(r,data.frame(longname='REPAIR OF HOUSEHOLD APPLIANCES',shortname='householdappliancesrepair',ucc='340620'))
    r=rbind(r,data.frame(longname='REUPHOLSTERY OF FURNITURE',shortname='furniturerepair',ucc='340630'))
    r=rbind(r,data.frame(longname='RENTAL/REPAIR-TOOLS;LAWN/GARDEN',shortname='gardeningequipmentrepair',ucc='340901'))
    r=rbind(r,data.frame(longname='MISC. HOME SERVICES',shortname='mischomeservices',ucc='340903'))
    r=rbind(r,data.frame(longname='RENTAL OF FURNITURE',shortname='furniturerental',ucc='340904'))
    r=rbind(r,data.frame(longname='CARE OF INVALIDS; ELDERLY; ETC',shortname='careforinvalids',ucc='340906'))
    r=rbind(r,data.frame(longname='RENTAL OF HOUSEHOLD EQUIPMENT',shortname='householdequipmentrental',ucc='340907'))
    r=rbind(r,data.frame(longname='RNTL OFF EQUIP NON-BUS USE',shortname='officeequipmentforhomerental',ucc='340908'))
    r=rbind(r,data.frame(longname='RENTAL OF TV/RADIO SOUND EQUIP',shortname='tvradiosoundrental',ucc='340909'))
    r=rbind(r,data.frame(longname='REPAIR OF MISC HSHLD EQ/FSHGS',shortname='miscshouseholdequipmentrepair',ucc='340913'))
    r=rbind(r,data.frame(longname='RENTERS INSURANCE',shortname='tenantsinsurance',ucc='350110'))
    r=rbind(r,data.frame(longname='MENS SUITS',shortname='menssuits',ucc='360110'))
    r=rbind(r,data.frame(longname='MENS SPORTCOATS/TAILORED JACKETS',shortname='menssportjackets',ucc='360120'))
    r=rbind(r,data.frame(longname='MENS COATS AND JACKETS',shortname='mensformaljackets',ucc='360210'))
    r=rbind(r,data.frame(longname='MENS UNDERWEAR',shortname='mensunderwear',ucc='360311'))
    r=rbind(r,data.frame(longname='MENS HOSIERY',shortname='menshosiery',ucc='360312'))
    r=rbind(r,data.frame(longname='MENS NIGHTWEAR/LOUNGEWEAR',shortname='menssleepwear',ucc='360320'))
    r=rbind(r,data.frame(longname='MENS ACCESSORIES',shortname='mensaccessories',ucc='360330'))
    r=rbind(r,data.frame(longname='MENS SWEATERS AND VESTS',shortname='menssweater',ucc='360340'))
    r=rbind(r,data.frame(longname='MENS SWIMSUITS/WARM-UP/SKI SUITS',shortname='mensactivesportswear',ucc='360350'))
    r=rbind(r,data.frame(longname='MENS SHIRTS',shortname='mensshirts',ucc='360410'))
    r=rbind(r,data.frame(longname='MENS PANTS',shortname='menspants',ucc='360511'))
    r=rbind(r,data.frame(longname='MENS SHORTS/SHORTS SETS',shortname='mensshorts_exathletic',ucc='360512'))
    r=rbind(r,data.frame(longname='MENS PANTS AND SHORTS',shortname='clothes_men',ucc='360513'))
    r=rbind(r,data.frame(longname='MENS UNIFORMS',shortname='mensuniforms',ucc='360901'))
    r=rbind(r,data.frame(longname='BOYS COATS AND JACKETS',shortname='boyscoatsjackets',ucc='370110'))
    r=rbind(r,data.frame(longname='BOYS SWEATERS',shortname='boyssweaters',ucc='370120'))
    r=rbind(r,data.frame(longname='BOYS SHIRTS',shortname='boysshirts',ucc='370130'))
    r=rbind(r,data.frame(longname='BOYS UNDERWEAR',shortname='boysunderwear',ucc='370211'))
    r=rbind(r,data.frame(longname='BOYS NIGHTWEAR',shortname='boyssleepwear',ucc='370212'))
    r=rbind(r,data.frame(longname='BOYS HOSIERY',shortname='boyshosiery',ucc='370213'))
    r=rbind(r,data.frame(longname='BOYS ACCESSORIES',shortname='boysaccessories',ucc='370220'))
    r=rbind(r,data.frame(longname='BOYS SUITS; SPORTCOATS;VESTS',shortname='boyssuitssportcoats',ucc='370311'))
    r=rbind(r,data.frame(longname='BOYS PANTS',shortname='boyspants',ucc='370312'))
    r=rbind(r,data.frame(longname='BOYS SHORTS; SHORTS SETS',shortname='boysshortsexcathletic',ucc='370313'))
    r=rbind(r,data.frame(longname='BOYS PANTS AND SHORTS',shortname='clothes_boys',ucc='370314'))
    r=rbind(r,data.frame(longname='BOYS UNIFORMS/ACTIVE SPORTSWE',shortname='boysuniformsactivesportswear',ucc='370901'))
    r=rbind(r,data.frame(longname='BOYS SWIMSUITS/WARM-UP/SKI SUITS',shortname='clothes_boys',ucc='370904'))
    r=rbind(r,data.frame(longname='WOMENS COATS AND JACKETS',shortname='womenscoats',ucc='380110'))
    r=rbind(r,data.frame(longname='WOMENS DRESSES',shortname='womensdresses',ucc='380210'))
    r=rbind(r,data.frame(longname='WOMENS SPORTCOATS; TAIL. JKTS',shortname='womenssportcoats',ucc='380311'))
    r=rbind(r,data.frame(longname='WOMENS VESTS AND SWEATERS',shortname='womenssweaters',ucc='380312'))
    r=rbind(r,data.frame(longname='WOMENS SHIRTS; TOPS;BLOUSES',shortname='womensshirts',ucc='380313'))
    r=rbind(r,data.frame(longname='WOMENS SKIRTS',shortname='womensskirts',ucc='380320'))
    r=rbind(r,data.frame(longname='WOMENS PANTS',shortname='womenspants',ucc='380331'))
    r=rbind(r,data.frame(longname='WOMENS SHORTS;SHORTS SETS',shortname='womensshorts_exathletic',ucc='380332'))
    r=rbind(r,data.frame(longname='WOMENS PANTS AND SHORTS',shortname='clothes_women',ucc='380333'))
    r=rbind(r,data.frame(longname='WOMENS SWIMSUITS/WARM-UP/SKI SUIT',shortname='womensactivesportswear',ucc='380340'))
    r=rbind(r,data.frame(longname='WOMENS SLEEPWEAR',shortname='womenssleepwear',ucc='380410'))
    r=rbind(r,data.frame(longname='WOMENS UNDERGARMENTS',shortname='womensundergarments',ucc='380420'))
    r=rbind(r,data.frame(longname='WOMENS HOSIERY',shortname='womenshosiery',ucc='380430'))
    r=rbind(r,data.frame(longname='WOMENS SUITS',shortname='womenssuits',ucc='380510'))
    r=rbind(r,data.frame(longname='WOMENS ACCESSORIES',shortname='womensaccessories',ucc='380901'))
    r=rbind(r,data.frame(longname='WOMENS UNIFORMS',shortname='womensuniforms',ucc='380902'))
    r=rbind(r,data.frame(longname='GIRLS COATS AND JACKETS',shortname='girlscoatsjackets',ucc='390110'))
    r=rbind(r,data.frame(longname='GIRLS DRESSES; SUITS',shortname='girlsdressessuits',ucc='390120'))
    r=rbind(r,data.frame(longname='GIRLS SHIRTS/BLOUSES/SWEATERS',shortname='girlssportcoats',ucc='390210'))
    r=rbind(r,data.frame(longname='GIRLS SKIRTS AND PANTS',shortname='girlsskirtspants',ucc='390221'))
    r=rbind(r,data.frame(longname='GIRLS SHORTS; SHORTS SETS',shortname='girlsshortsexathletic',ucc='390222'))
    r=rbind(r,data.frame(longname='GIRLS SKIRTS; PANTS; AND SHORTS',shortname='clothes_girls',ucc='390223'))
    r=rbind(r,data.frame(longname='GIRLS SWIMSUITS/WARM-UP/SKI SUITS',shortname='girlsactivesportswear',ucc='390230'))
    r=rbind(r,data.frame(longname='GIRLS UNDERWEAR AND SLEEPWEAR',shortname='girlsundergarments',ucc='390310'))
    r=rbind(r,data.frame(longname='GIRLS HOSIERY',shortname='girlshosiery',ucc='390321'))
    r=rbind(r,data.frame(longname='GIRLS ACCESSORIES',shortname='girlsaccessories',ucc='390322'))
    r=rbind(r,data.frame(longname='GIRLS UNIFORMS',shortname='girlsuniforms',ucc='390901'))
    r=rbind(r,data.frame(longname='MENS FOOTWEAR',shortname='mensfootwear',ucc='400110'))
    r=rbind(r,data.frame(longname='BOYS FOOTWEAR',shortname='boysfootwear',ucc='400210'))
    r=rbind(r,data.frame(longname='GIRLS FOOTWEAR',shortname='girlsfootwear',ucc='400220'))
    r=rbind(r,data.frame(longname='WOMENS FOOTWEAR',shortname='womensfootwear',ucc='400310'))
    r=rbind(r,data.frame(longname='INFANT COAT/JACKET/SNOWSUIT',shortname='infantscoats',ucc='410110'))
    r=rbind(r,data.frame(longname='INFANT DRESSES/OUTERWEAR',shortname='infantsdresses',ucc='410120'))
    r=rbind(r,data.frame(longname='INFANT UNDERGARMENTS',shortname='infantsundergarments',ucc='410130'))
    r=rbind(r,data.frame(longname='INFANT NIGHTWEAR/LOUNGEWEAR',shortname='infantssleepingwear',ucc='410140'))
    r=rbind(r,data.frame(longname='INFANTS   ACCESSORIES',shortname='infantsaccessories',ucc='410901'))
    r=rbind(r,data.frame(longname='MATERIAL FOR MAKING CLOTHES',shortname='sewingmaterial_clothes',ucc='420110'))
    r=rbind(r,data.frame(longname='SEWING NOTIONS; PATTERNS',shortname='sewingpatterns',ucc='420120'))
    r=rbind(r,data.frame(longname='WATCHES',shortname='watches',ucc='430110'))
    r=rbind(r,data.frame(longname='JEWELRY',shortname='jewelry',ucc='430120'))
    r=rbind(r,data.frame(longname='LUGGAGE',shortname='travelitems',ucc='430130'))
    r=rbind(r,data.frame(longname='SHOE REPAIR; OTH SHOE SERVICE',shortname='shoerepair',ucc='440110'))
    r=rbind(r,data.frame(longname='COIN-OP APPAREL LDRY/DRY CLNG',shortname='apparelcleaning_coinoperated',ucc='440120'))
    r=rbind(r,data.frame(longname='ALTER/REPAIR OF APPAREL; ACCESS',shortname='clothes_repair',ucc='440130'))
    r=rbind(r,data.frame(longname='CLOTHING RENTAL',shortname='clothing_rental',ucc='440140'))
    r=rbind(r,data.frame(longname='WATCH AND JEWELRY REPAIR',shortname='watchjewelryrepair',ucc='440150'))
    r=rbind(r,data.frame(longname='APPAREL LNDRY/DRY CLNG N/COIN-OP',shortname='apparell_notcoinoperated',ucc='440210'))
    r=rbind(r,data.frame(longname='CLOTHING STORAGE OUTSIDE THE HOME',shortname='clothingstorage',ucc='440900'))
    r=rbind(r,data.frame(longname='NEW CARS',shortname='newcars',ucc='450110'))
    r=rbind(r,data.frame(longname='NEW TRUCKS',shortname='newtrucks',ucc='450210'))
    r=rbind(r,data.frame(longname='NEW MOTORCYCLES',shortname='newmotorcycles',ucc='450220'))
    r=rbind(r,data.frame(longname='CAR LEASE PAYMENTS',shortname='carlease',ucc='450310'))
    r=rbind(r,data.frame(longname='TRUCK LEASE PAYMENTS',shortname='trucklease',ucc='450410'))
    r=rbind(r,data.frame(longname='AIRCRAFT',shortname='aircraft',ucc='450900'))
    r=rbind(r,data.frame(longname='USED CARS',shortname='usedcars',ucc='460110'))
    r=rbind(r,data.frame(longname='USED TRUCKS',shortname='usedtrucks',ucc='460901'))
    r=rbind(r,data.frame(longname='USED MOTORCYCLES',shortname='usedmotorcycles',ucc='460902'))
    r=rbind(r,data.frame(longname='USED AIRCRAFT',shortname='usedaircraft',ucc='460903'))
    r=rbind(r,data.frame(longname='GASOLINE',shortname='gasoline',ucc='470111'))
    r=rbind(r,data.frame(longname='DIESEL FUEL',shortname='dieselfuel',ucc='470112'))
    r=rbind(r,data.frame(longname='GASAHOL',shortname='gasohol',ucc='470114'))
    r=rbind(r,data.frame(longname='MOTOROIL',shortname='motoroil',ucc='470211'))
    r=rbind(r,data.frame(longname='COOLANT/ADDITIVES/BRK/TRNS FLD',shortname='coolant',ucc='470220'))
    r=rbind(r,data.frame(longname='TIRES PURCHASED/REPLACED/INSTALL',shortname='tires',ucc='480110'))
    r=rbind(r,data.frame(longname='VEHICLE PRODUCTS & SERVICES',shortname='vehicleproducts',ucc='480212'))
    r=rbind(r,data.frame(longname='PARTS/EQUIP/ACCESSORIES',shortname='carequipment',ucc='480213'))
    r=rbind(r,data.frame(longname='VEHICLE AUDIO EQUIPMENT',shortname='vehicleaudioequipment_excludinglabor',ucc='480214'))
    r=rbind(r,data.frame(longname='MISC. AUTO REPAIR/SERVICING',shortname='miscsautorepair',ucc='490000'))
    r=rbind(r,data.frame(longname='BODY WORK AND PAINTING',shortname='carbodyworkholstery',ucc='490110'))
    r=rbind(r,data.frame(longname='CLUTCH; TRANSMISSION REPAIR',shortname='transmissionrepair',ucc='490211'))
    r=rbind(r,data.frame(longname='DRIVE SHAFT AND REAR-END REPAIR',shortname='driveshaftrepair',ucc='490212'))
    r=rbind(r,data.frame(longname='BRAKE WORK (DIARY Q961 NEW)',shortname='brakework',ucc='490220'))
    r=rbind(r,data.frame(longname='REPAIR TO STEERING OR FRONT END',shortname='steeringrepair',ucc='490231'))
    r=rbind(r,data.frame(longname='REPAIR TO ENGINE COOLING SYSTEM',shortname='coolingrepair',ucc='490232'))
    r=rbind(r,data.frame(longname='MOTOR TUNE-UP',shortname='motortuneup',ucc='490311'))
    r=rbind(r,data.frame(longname='LUBE; OIL CHANGE AND OIL FILTERS',shortname='oilchange',ucc='490312'))
    r=rbind(r,data.frame(longname='FRNT END ALIGN; WHEEL BAL/ROTAT',shortname='caralignment',ucc='490313'))
    r=rbind(r,data.frame(longname='SHOCK ABSORBER REPLACEMENT',shortname='shockabsorberreplacement',ucc='490314'))
    r=rbind(r,data.frame(longname='BRAKE ADJUSTMENT',shortname='brakeadjustment',ucc='490315'))
    r=rbind(r,data.frame(longname='GAS TANK REPAIR;REPLACEMENT',shortname='gastankrepairreplacement',ucc='490316'))
    r=rbind(r,data.frame(longname='EXHAUST SYSTEM REPAIR',shortname='exhaustsystemrepair',ucc='490411'))
    r=rbind(r,data.frame(longname='ELECTRICAL SYSTEM REPAIR',shortname='electricalsystemrepair',ucc='490412'))
    r=rbind(r,data.frame(longname='MOTOR REPAIR/REPLACEMENT',shortname='motorrepair',ucc='490413'))
    r=rbind(r,data.frame(longname='VEHICLE INSURANCE',shortname='vehicleinsurance',ucc='500110'))
    r=rbind(r,data.frame(longname='VEHICLE REGISTRATION STATE/LOCAL',shortname='vehicleregistration_statelocal',ucc='520110'))
    r=rbind(r,data.frame(longname='VEHICLE REGISTRATION STATE',shortname='vehicleregistration_state',ucc='520111'))
    r=rbind(r,data.frame(longname='VEHICLE REGISTRATION LOCAL',shortname='vehicleregistration_local',ucc='520112'))
    r=rbind(r,data.frame(longname='DRIVERS LICENSE',shortname='driverslicense',ucc='520310'))
    r=rbind(r,data.frame(longname='VEHICLE INSPECTION',shortname='vehicleinspection',ucc='520410'))
    r=rbind(r,data.frame(longname='AUTO RENTAL',shortname='autorental_extrips',ucc='520511'))
    r=rbind(r,data.frame(longname='TRUCK RENTAL',shortname='truckrental_extrips',ucc='520521'))
    r=rbind(r,data.frame(longname='PRKNG FEE IN HME CITY EXCL RSDNC',shortname='parkingfeesy',ucc='520531'))
    r=rbind(r,data.frame(longname='TOLLS OR ELECTRONIC TOLL PASSES',shortname='tolls',ucc='520541'))
    r=rbind(r,data.frame(longname='TOWING CHARGES',shortname='towingcharges',ucc='520550'))
    r=rbind(r,data.frame(longname='GLOBAL POSITIONING SERVICES',shortname='GPS',ucc='520560'))
    r=rbind(r,data.frame(longname='DOCKING/LANDING FEES',shortname='boatsplansdockinglandingfees',ucc='520901'))
    r=rbind(r,data.frame(longname='MOTORCYCLE RENTAL',shortname='motorcyclerentals',ucc='520902'))
    r=rbind(r,data.frame(longname='NA',shortname='aircraftrentals',ucc='520903'))
    r=rbind(r,data.frame(longname='RENTAL NON-CAMPER TRAILER',shortname='noncampertypetrailerrentals',ucc='520904'))
    r=rbind(r,data.frame(longname='AIRLINE FARES',shortname='airlinefares',ucc='530110'))
    r=rbind(r,data.frame(longname='INTERCITY BUS FARES',shortname='intercitybusfares',ucc='530210'))
    r=rbind(r,data.frame(longname='INTRACITY MASS TRANSIT FARES',shortname='intracitymasstransitfares',ucc='530311'))
    r=rbind(r,data.frame(longname='TAXI FARES AND LIMOUSINE SERVICE',shortname='taxifares',ucc='530412'))
    r=rbind(r,data.frame(longname='INTERCITY TRAIN FARES',shortname='intercitytrainfares',ucc='530510'))
    r=rbind(r,data.frame(longname='SHIP FARES',shortname='shipfares',ucc='530901'))
    r=rbind(r,data.frame(longname='SCHOOL BUS',shortname='privateschoolbus',ucc='530902'))
    r=rbind(r,data.frame(longname='CAR/VAN POOL & NON-MOTOR TRANS',shortname='carpoolnonmotortransportation',ucc='530903'))
    r=rbind(r,data.frame(longname='PRESCRIPTION DRUGS',shortname='prescriptiondrugs',ucc='540000'))
    r=rbind(r,data.frame(longname='EYEGLASSES AND CONTACT LENSES',shortname='eyeglassesorcontactlenses',ucc='550110'))
    r=rbind(r,data.frame(longname='OVER-THE-COUNTER DRUGS',shortname='overthecounterdrugs',ucc='550210'))
    r=rbind(r,data.frame(longname='TOPICALS AND DRESSINGS',shortname='cottonballs',ucc='550310'))
    r=rbind(r,data.frame(longname='MEDICAL EQUIP. FOR GENERAL USE',shortname='medicalequipment',ucc='550320'))
    r=rbind(r,data.frame(longname='SUPPORTIVE/CONVAL MED. EQUIP.',shortname='supportivemedicalequipment',ucc='550330'))
    r=rbind(r,data.frame(longname='HEARING AIDS',shortname='hearingaids',ucc='550340'))
    r=rbind(r,data.frame(longname='NONPRESCRIP VITAMINS',shortname='nonprescriptionvitamins',ucc='550410'))
    r=rbind(r,data.frame(longname='RECREATIONAL DRUGS',shortname='recreationaldrugs',ucc='550900'))
    r=rbind(r,data.frame(longname='PHYSICIANS SERVICES',shortname='physiciansservices',ucc='560110'))
    r=rbind(r,data.frame(longname='DENTAL SERVICES',shortname='dentalservices',ucc='560210'))
    r=rbind(r,data.frame(longname='EYECARE SERVICES',shortname='eyeexams',ucc='560310'))
    r=rbind(r,data.frame(longname='LAB TESTS; X-RAYS',shortname='labtests',ucc='560330'))
    r=rbind(r,data.frame(longname='SERV BY PROS OTH THAN PHYSICIANS',shortname='nonphysicalmedicalservices',ucc='560400'))
    r=rbind(r,data.frame(longname='HOSPITAL CARE NOT SPECIFIED',shortname='hospitalcare_notspecified',ucc='570000'))
    r=rbind(r,data.frame(longname='CARE IN CONVL OR NURSING HOME',shortname='carenursinghome',ucc='570220'))
    r=rbind(r,data.frame(longname='OTHER MEDICAL CARE SERVICE',shortname='medicalcare_other',ucc='570230'))
    r=rbind(r,data.frame(longname='RENTAL OF MEDICAL/SURGICAL EQUIP',shortname='medicalequipmentrentals',ucc='570901'))
    r=rbind(r,data.frame(longname='REPAIR OF MEDICAL EQUIPMENT',shortname='medicalequipmentrepair',ucc='570902'))
    r=rbind(r,data.frame(longname='RENTAL OF SUPORTIVE/CONVAL EQUIP',shortname='supportiveequipmentrepair',ucc='570903'))
    r=rbind(r,data.frame(longname='HEALTH INSURANCE NOT SPEC.',shortname='healthinsurance_nonspecified',ucc='580000'))
    r=rbind(r,data.frame(longname='COMMERCIAL HEALTH INSURANCE',shortname='healthinsurance_commercial',ucc='580110'))
    r=rbind(r,data.frame(longname='BLUECROSS/BLUE SHIELD',shortname='bluecross',ucc='580210'))
    r=rbind(r,data.frame(longname='HEALTH MAINTENANCE PLANS',shortname='healthmaintenanceplans',ucc='580310'))
    r=rbind(r,data.frame(longname='MEDICARE PAYMENTS',shortname='medicarepayments',ucc='580901'))
    r=rbind(r,data.frame(longname='NEWSPAPERS',shortname='newspapers',ucc='590110'))
    r=rbind(r,data.frame(longname='MAGAZINES',shortname='magazinesperiodicals',ucc='590210'))
    r=rbind(r,data.frame(longname='BOOKS THRU BOOK CLUBS',shortname='books_bookclubs',ucc='590220'))
    r=rbind(r,data.frame(longname='BOOKS NOT THRU BOOK CLUBS',shortname='books_exbookclubs',ucc='590230'))
    r=rbind(r,data.frame(longname='NEWSLETTERS',shortname='newsletters',ucc='590900'))
    r=rbind(r,data.frame(longname='OUTBOARD MOTOR',shortname='outboardmotor',ucc='600110'))
    r=rbind(r,data.frame(longname='UNPOWERED BOATS; TRAILERS',shortname='unpoweredboatstrailers',ucc='600120'))
    r=rbind(r,data.frame(longname='POWERED SPORTS VEHICLES',shortname='poweredsportsvehicles',ucc='600130'))
    r=rbind(r,data.frame(longname='GENERAL SPORT/EXCERCISE EQUIP',shortname='recsportstables',ucc='600210'))
    r=rbind(r,data.frame(longname='BICYCLES',shortname='bicycles',ucc='600310'))
    r=rbind(r,data.frame(longname='CAMPING EQUIPMENT',shortname='campingequipment',ucc='600410'))
    r=rbind(r,data.frame(longname='HUNTING; FISHING EQUIPMENT',shortname='huntingfishingequipment',ucc='600420'))
    r=rbind(r,data.frame(longname='WINTER SPORT EQUIPMENT',shortname='wintersportsequipment',ucc='600430'))
    r=rbind(r,data.frame(longname='WATER SPORT EQUIPMENT',shortname='miscsportsequipment',ucc='600900'))
    r=rbind(r,data.frame(longname='GLOBAL POSITIONING SYSTEM DEVICES',shortname='GPS',ucc='600903'))
    r=rbind(r,data.frame(longname='TOYS GAMES ARTS CRAFTS TRICYCLES',shortname='batterypoweredriders',ucc='610110'))
    r=rbind(r,data.frame(longname='PLAYGROUND EQUIPMENT',shortname='playgroundequipment',ucc='610120'))
    r=rbind(r,data.frame(longname='MUSIC INSTRUMENTS/ACCESSORIES',shortname='musicalinstruments',ucc='610130'))
    r=rbind(r,data.frame(longname='STAMP AND COIN COLLECTING',shortname='numismatism',ucc='610140'))
    r=rbind(r,data.frame(longname='FILM',shortname='film',ucc='610210'))
    r=rbind(r,data.frame(longname='OTHER PHOTOGRAPHIC SUPPLIES',shortname='photographicsupplies_exfilm',ucc='610220'))
    r=rbind(r,data.frame(longname='PHOTOGRAPHIC EQUIPMENT',shortname='photographicequipment',ucc='610230'))
    r=rbind(r,data.frame(longname='PET FOOD',shortname='petfood',ucc='610310'))
    r=rbind(r,data.frame(longname='PET-PURCHASE/SUPPLIES/MEDICINE',shortname='petsupplies_nonfood',ucc='610320'))
    r=rbind(r,data.frame(longname='FIREWORKS',shortname='fireworks',ucc='610901'))
    r=rbind(r,data.frame(longname='SOUVENIRS',shortname='souvenirs',ucc='610902'))
    r=rbind(r,data.frame(longname='VISUAL GOODS',shortname='visualgoods',ucc='610903'))
    r=rbind(r,data.frame(longname='SOCIAL/RECRE/CIVIC CLUB MEMBRSHP',shortname='clubmembershipfees',ucc='620111'))
    r=rbind(r,data.frame(longname='CREDIT CARD MEMBERSHIPS',shortname='creditcardmembershipsfees',ucc='620112'))
    r=rbind(r,data.frame(longname='AUTOMOBILE SERVICE CLUBS',shortname='automobileserviceclubsfees',ucc='620113'))
    r=rbind(r,data.frame(longname='FEES FOR PARTICIPANT SPORTS',shortname='sportsclubmembershipfees',ucc='620121'))
    r=rbind(r,data.frame(longname='MOVIE; THEATER; OPERA; BALLET',shortname='entertainmentvenuemembershipfees',ucc='620211'))
    r=rbind(r,data.frame(longname='ADMISSION TO SPORTING EVENTS',shortname='sportingeventsmembershipfees',ucc='620221'))
    r=rbind(r,data.frame(longname='FEES FOR RECREATIONAL LESSONS',shortname='recreationallessonsfees',ucc='620310'))
    r=rbind(r,data.frame(longname='PHOTOGRAPHER FEES',shortname='photographerfees',ucc='620320'))
    r=rbind(r,data.frame(longname='FILM PROCESSING',shortname='filmprocessing',ucc='620330'))
    r=rbind(r,data.frame(longname='PET SERVICES',shortname='petservices',ucc='620410'))
    r=rbind(r,data.frame(longname='VET SERVICES',shortname='veterinarian',ucc='620420'))
    r=rbind(r,data.frame(longname='ADMISSIONS MISC',shortname='miscadmissionsfees',ucc='620510'))
    r=rbind(r,data.frame(longname='MISC. ENTERTAINMENT SERVICES',shortname='miscentertainmentservices',ucc='620610'))
    r=rbind(r,data.frame(longname='CAMP FEES',shortname='campfees',ucc='620710'))
    r=rbind(r,data.frame(longname='REN/REP SP/PHOT/EQUP/PASSP',shortname='sportphotopgraphicmusicequipmentrentalrepairs',ucc='620810'))
    r=rbind(r,data.frame(longname='RNTL VIDEO CASS/TAPES/DISCS/FILMS',shortname='videotape_rentals',ucc='620912'))
    r=rbind(r,data.frame(longname='PINBALL/ELECTRONIC VIDEO GAMES',shortname='videogames_coinoperated',ucc='620913'))
    r=rbind(r,data.frame(longname='SPORT VEH RENTAL',shortname='sportvehiclerental',ucc='620915'))
    r=rbind(r,data.frame(longname='MISCELLANEOUS FEES',shortname='lotterieslosses',ucc='620925'))
    r=rbind(r,data.frame(longname='LOTTERIES AND PARIMUTUEL LOSSES',shortname='miscfees',ucc='620926'))
    r=rbind(r,data.frame(longname='ONLINE ENTERTAINMENT AND GAMES',shortname='games_online',ucc='620930'))
    r=rbind(r,data.frame(longname='CIGARETTES',shortname='cigarettes',ucc='630110'))
    r=rbind(r,data.frame(longname='OTHER TOBACCO PRODUCTS',shortname='tobacco_other',ucc='630210'))
    r=rbind(r,data.frame(longname='SMOKING ACCESSORIES',shortname='smokingaccessories',ucc='630220'))
    r=rbind(r,data.frame(longname='MARIJUANA',shortname='marijuana',ucc='630900'))
    r=rbind(r,data.frame(longname='HAIR CARE PRODUCTS',shortname='haircareproducts',ucc='640110'))
    r=rbind(r,data.frame(longname='NON-ELEC ARTICLES FOR THE HAIR',shortname='nonelectrichairequipment',ucc='640120'))
    r=rbind(r,data.frame(longname='WIGS AND HAIRPIECES',shortname='wigshairpieces',ucc='640130'))
    r=rbind(r,data.frame(longname='ORAL HYGIENE PRODUCTS;ARTICLES',shortname='oralhygieneproducts',ucc='640210'))
    r=rbind(r,data.frame(longname='SHAVING NEEDS',shortname='shavingneeds',ucc='640220'))
    r=rbind(r,data.frame(longname='COSMETICS; PERFUME; BATH PREP',shortname='cosmetics',ucc='640310'))
    r=rbind(r,data.frame(longname='DEOD;FEM HYG; MISC. PERS. CARE',shortname='miscpersonalcare',ucc='640410'))
    r=rbind(r,data.frame(longname='ELECTRIC PERSONAL CARE APPL.',shortname='electricalpersonalcareequipment',ucc='640420'))
    r=rbind(r,data.frame(longname='PERS. CARE SERV FOR FEMALES',shortname='femalepersonalcareservices',ucc='650110'))
    r=rbind(r,data.frame(longname='PERS. CARE SERV FOR MALES',shortname='malepersonalcareservices',ucc='650210'))
    r=rbind(r,data.frame(longname='REPAIR OF PERS. CARE APP.',shortname='personalcareappliancesrentalrepair',ucc='650900'))
    r=rbind(r,data.frame(longname='SCHOOL SUPPL.; ETC. - UNSPEC.',shortname='schoolsupplies',ucc='660000'))
    r=rbind(r,data.frame(longname='SCHOOL BK/SUPL/EQUIP FOR COLLEGE',shortname='schoolbooks_college',ucc='660110'))
    r=rbind(r,data.frame(longname='SCHOOL BK/SUPL/EQUIP FOR ELEM/HS',shortname='schoolbooks_highschoolelementary',ucc='660210'))
    r=rbind(r,data.frame(longname='ENCYL. OTH SETS OF REFRNCE BKS',shortname='referencebooks',ucc='660310'))
    r=rbind(r,data.frame(longname='SCH BK/SUP/EQ-DAY CARE;NURS;OTH',shortname='schoolbooksnurseryschool',ucc='660900'))
    r=rbind(r,data.frame(longname='COLLEGE TUITION',shortname='tuition_college',ucc='670110'))
    r=rbind(r,data.frame(longname='ELEM./H.S. TUITION',shortname='tuition_highschoolelementary',ucc='670210'))
    r=rbind(r,data.frame(longname='DAY CARE/NURS/PRSCH EXP INCL TUIT',shortname='schoolsexpense_other',ucc='670310'))
    r=rbind(r,data.frame(longname='VOC/TECH SCHOOL TUITION',shortname='school_tuition',ucc='670410'))
    r=rbind(r,data.frame(longname='OTHER SCHOOL TUITION',shortname='tuition_otherschool',ucc='670901'))
    r=rbind(r,data.frame(longname='OTH SCH EXPENSES INCLUD RENTALS',shortname='booksequipmentrental',ucc='670902'))
    r=rbind(r,data.frame(longname='TEST PREP/TUTORING SERVICES',shortname='school_prep',ucc='670903'))
    r=rbind(r,data.frame(longname='LEGAL FEES',shortname='legalfees',ucc='680110'))
    r=rbind(r,data.frame(longname='FUNERAL EXPENSE',shortname='funeralexpenses',ucc='680140'))
    r=rbind(r,data.frame(longname='SAFE DEPOSIT BOX RENTAL',shortname='safedepositboxrental',ucc='680210'))
    r=rbind(r,data.frame(longname='CHECK ACCTS / OTH BANK SERV CHGS',shortname='bankingservicesfees',ucc='680220'))
    r=rbind(r,data.frame(longname='CEMETERY LOTS;VAULTS;MAINT FEES',shortname='cemeterylotsmaintenance',ucc='680901'))
    r=rbind(r,data.frame(longname='ACCOUNTING FEES',shortname='accountingfees',ucc='680902'))
    r=rbind(r,data.frame(longname='MISC. PERS. SERVICES',shortname='miscpersonalservices',ucc='680903'))
    r=rbind(r,data.frame(longname='DATING SERVICES',shortname='dating_services',ucc='680904'))
    r=rbind(r,data.frame(longname='COMPUTER; COMP HRDWR NON BUS USE',shortname='computersfornonbusinessuse',ucc='690110'))
    r=rbind(r,data.frame(longname='COMPUTER INFORMATION SERVICES',shortname='computerinformationservices',ucc='690114'))
    r=rbind(r,data.frame(longname='PERSONAL DIGITAL ASSISTANTS',shortname='digital_assistant',ucc='690115'))
    r=rbind(r,data.frame(longname='INTERNET SERVICES AWAY FROM HOME',shortname='internet_outside',ucc='690116'))
    r=rbind(r,data.frame(longname='PORTABLE MEMORY',shortname='computer_portablememory',ucc='690117'))
    r=rbind(r,data.frame(longname='TELEPHONE ANSWERING DEVICES',shortname='telephoneansweringdevices',ucc='690210'))
    r=rbind(r,data.frame(longname='CALCULATORS',shortname='calculators',ucc='690220'))
    r=rbind(r,data.frame(longname='TYPWRITS/OTH OFF MACH NON-BUS USE',shortname='typewritersnonbusiness',ucc='690230'))
    r=rbind(r,data.frame(longname='MEALS AS PAY',shortname='income_meals',ucc='800700'))
    r=rbind(r,data.frame(longname='RENT AS PAY',shortname='income_rent',ucc='800710'))
    r=rbind(r,data.frame(longname='DEDUCTIONS FOR GOVT RETIREMENT',shortname='retirement_deductions',ucc='800910'))
    r=rbind(r,data.frame(longname='DEDUCTIONS FOR RR RETIREMENT',shortname='retirement_deductions',ucc='800920'))
    r=rbind(r,data.frame(longname='DEDUCTIONS FOR PRIVATE PENSIONS',shortname='retirement_deductions',ucc='800931'))
    r=rbind(r,data.frame(longname='HOME OWNERSHIP EXPENSE',shortname='homeownershipexpensenotspecified',ucc='999000'))
    r=rbind(r,data.frame(longname='TAXES NOT SPECIFIED',shortname='taxesnotspecified',ucc='999900'))
    
    r$ucc <- as.integer(as.character(r$ucc))
    return(r)
  }
  cex_groups <-function(year){
    if (year == 2009){
      
      x<-NULL
      
      x<-rbind(x,data.frame(category='food', group='low', shortname='cooking_oil'))
      x<-rbind(x,data.frame(category='food', group='high', shortname='butter_margarine'))
      x<-rbind(x,data.frame(category='food', group='high', shortname='rice_husked'))
      
      #finance
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='stocksbonds'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='preciousmetals'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='miscinvestments'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='stocksbonds'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='savingsdeposit'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='retirementplans'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='mortgage'))
      
      
      #expenditure
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='employmentcounseling'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='employmentcounseling'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='employmentcounseling'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='insurance'))
      
      x<-rbind(x,data.frame(category='food', group='low', shortname='flour'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='flour_prepared'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='cereal'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='rice'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='pasta_cornmeal'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='whitebread'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='breadnonwhite'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='biscuitsrollsmuffins'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='cakes_nonfrozen'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='cookies'))
      
      x<-rbind(x,data.frame(category='food', group='low', shortname='crackers'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='breadcracker'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='bakery_excake')) #?
      x<-rbind(x,data.frame(category='food', group='low', shortname='bakery_frozen'))
      x<-rbind(x,data.frame(category='food', group='low', shortname='piestarts')) #?
      
      x<-rbind(x,data.frame(category='food', group='low', shortname='groundbeef_noncanned')) #?
      
      x<-rbind(x,data.frame(category='food', group='low', shortname='chuckroast_noncanned')) #?
      x<-rbind(x,data.frame(category='food', group='low', shortname='roundroast_noncanned')) #?
      x<-rbind(x,data.frame(category='food', group='low', shortname='otherbeefroast_noncanned')) #?
      x<-rbind(x,data.frame(category='food', group='low', shortname='roundsteak_noncanned'))#?
      x<-rbind(x,data.frame(category='food', group='high', shortname='sirloinsteak_noncanned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='steak_other_noncanned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='beef_other_noncanned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='bacon')) #?
      x<-rbind(x,data.frame(category='food', group='low', shortname='porkchops'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='ham_noncanned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='otherpork_noncanned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='porksausage_noncanned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='ham_canned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='frankfurters_noncanned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='salami_noncanned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='otherlunchmeat'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='lamb_noncanned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='mutton_goat_game'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='chicken'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='chicken_parts'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='poultry_other'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='seafood_canned'))#?
      x<-rbind(x,data.frame(category='food', group='high', shortname='fish_fresh'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='fish_frozen'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='eggs'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='milk_fresh'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='cream'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='butter'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='cheese'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='icecream'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='dairy_other'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='apples'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='bananas'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='oranges'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='freshfruits_other'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='fruites_citrus_non_orange'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='potatoes'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='lettuce'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='tomatoes'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='freshvegetables_other'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='orangejuice_frozen'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='fruits_frozen'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='fruitjuices_frozen'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='fruitjuices_fresh'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='fruitjuices_bottled'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='fruits_canned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='driedfruits'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='vegetables_frozen'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='beans_canned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='corn_canned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='miscvegetables_canned'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='vegetables_otherprocessed'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='peas_dried'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='beans_dried'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='carrots_onions_greens_cabbage'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='vegetablejuices_frozen'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='vegetablejuices'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='candy'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='sugar'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='artificialsweeteners'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='jams_jellies'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='margarine'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='fatsoils'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='saladdressings'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='nondairycreamsubstitutes'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='peanutbutter'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='coladrinks'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='carbonateddrinks_other'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='coffee_roasted'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='coffee_instant'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='noncarbonatedfruitflavoreddrinks'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='tea'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='drinks_other_noncarbonated'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='soup'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='frozenmeals'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='frozenpreparedfood_other'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='potatochipssnacks'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='nuts'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='saltseasoningsspices'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='olivespickles'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='sauces'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='condiments_other'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='preparedsalads'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='prepareddesserts'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='babyfood'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='miscpreparedfoods'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='vitaminsupplements'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='lunch_atfastfood'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='lunch_atfullservice'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='lunch_atvendingmachine'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='lunch_atemployer'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='lunch_atboard'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='lunch_atcatered'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='dinner_atfastfood'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='dinner_atfullservice'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='dinner_atvendingmachine'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='dinner_atemployer'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='dinner_atboard'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='dinner_atcatered'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='snacks_atfastfood'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='snacks_atfullservice'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='snacks_atvendmachine'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='snacks_atemployer'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='snacks_atboard'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='snacks_atcatered'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='breakfast_atfastfood'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='breakfast_atfullservice'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='breakfast_atvendingmachine'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='breakfast_atemployer'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='breakfast_atboard'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='breakfast_atcatered'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='board_atfastfood'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='board_atfullservice'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='boarda_tvendingmachine'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='board_atemployer'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='board'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='board_atcatered'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='catered_atfastfood'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='catered_atfullservice'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='catered_atvendingmachine'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='catered_atemployer'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='catered_atboard'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='catered'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='beer_athome'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='nonalcoholicbeer'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='whiskey_athome'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='wine_athome'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='alcohol_other_athome'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='beer_atfastfood'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='beer_atfullservice'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='beer_atvendingmachine'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='beer_atemployer'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='beer_atboard'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='beer_atcatered'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='wine_atfastfood'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='wine_atfullservice'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='wine_atvendingmachine'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='wine_atemployer'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='wine_atboard'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='wine_atcatered'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='alcoholex_atfastfood'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='alcoholexfullservice'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='alcoholexvendingmachine'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='alcoholex_atemployer'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='alcoholex_atboard'))#?
      x<-rbind(x,data.frame(category='food', group='low', shortname='alcoholex_caterer'))#?
      
      x<-rbind(x,data.frame(category='energy', group='high', shortname='bottledortankgas'))
      x<-rbind(x,data.frame(category='energy', group='low', shortname='coal'))
      x<-rbind(x,data.frame(category='energy', group='high', shortname='miscfuels'))#?
      x<-rbind(x,data.frame(category='energy', group='low', shortname='electricity'))#?
      x<-rbind(x,data.frame(category='energy', group='high', shortname='utility_naturalgas'))
      
      #x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='homerent'))
      #x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='lodgingawayfromhome'))
      #x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='housing_schoolkid'))
      #x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='landrent'))
      
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='capitalimprovements_notspecified'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='purchaseofproperty'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='capitalimprovements_commodities'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='capitalimprovements_services'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='installedhardsurfaceflooring'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='repair_maintenance_improvements'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='propertymaintenance'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='installedwalltowallcarpet'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='repairdisposal_dishwasher_rangehood'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='paint_wallpapersupplies'))
      
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='paintingequipment'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='paneling'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='masonrymaterials'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='plumbingfixtures'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='electricheatingairconditioning'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='softsurfacefloorcovering'))
      
      
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='telephoneservice'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='waterseweragemaintenance'))
      
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='steamheat'))
      
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='bathroomlinens'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='bedroomlinens'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='kitchendiningroomlinens'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='curtainsdrapes_exshower'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='coverscushions'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='slipcovers_curtains_sewingmaterial'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='linens_other'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='mattress'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='bedroomfurniture_other'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='sofas'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='livingroomchairs'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='livingroomtables'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='kitchendiningroomfurniture'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='infantsfurniture')) #?
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='patiofurniture'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='shelvesfurniture'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='refrigerator'))#?
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='washers'))#?
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='dryers'))#?
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='stoves_ovens'))#?
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='microwave'))#?
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='dishwasher_portable'))#?
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='windowairconditioners'))#?
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='mischouseholdappliances'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='tv_bw'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='colortv'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='colortv_portable'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='videoplayers'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='electronicequipment'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='roomsizerugs'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='venetianblinds'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='infantsequipment'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='laundrycleaningequipment'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='lampslightingfixtures'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='householddecorativeitems_other'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='dinnerware_plastic'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='dinnerware_china'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='tableware'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='glassware'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='servingpieces_silver'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='servingpieces_nonsilver'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='cookware_nonelectric'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='tableware_nonelectrickitchenware'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='lawnmowingequipment'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='powertools'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='otherhardware'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='electricfloorcleaningequipment'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='sewingmachines'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='electricalkitchenappliances_small'))
      
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='portableheatingcoolingequipment'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='miscsequipmentsupplies'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='permanenthardsurfacefloorcovering'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='landscapingitems'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='officefurnitureforhome'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='tools_nonpowered'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='pottedplants'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='homestorage'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='mischouseholdequipment'))
      x<-rbind(x,data.frame(category='housing', group='asset', shortname='electronictestingequipment'))
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='otherlaundrycleaningproducts'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='mischouseholdproducts'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='gardensupplies'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='postage'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='deliveryservices'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='homecareforchildren'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='housekeepingservice'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='gardeningservices'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='movingstorage'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='nonclothinghouseholdlaundry'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='nonclothinghouseholdlaundry_coinoperated'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='mischomeservices'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='furniturerental'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='householdequipmentrental'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='officeequipmentforhomerental'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='tvradiosoundrental'))#?
      x<-rbind(x,data.frame(category='housing', group='expenditure', shortname='tenantsinsurance'))#?
      
      
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='cable_satellite'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='disktapemedia'))
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='consolesupplies'))
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='radio_nonvehicle'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='recordplayer'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='taperecorder'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='soundcomponents'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='miscsoundequipment'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='soundequipmentaccessories'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='satellitedishes'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='records_tapes'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='outdoorequipment'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='clocks'))
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='telephone'))
      
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='soapsdetergents_exhandsoaps'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='papertowels'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='stationery'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='menssuits'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='menssportjackets'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='mensformaljackets'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='mensunderwear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='menshosiery'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='menssleepwear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='mensaccessories'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='menssweater'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='mensactivesportswear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='mensshirts'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='menspants'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='mensshorts_exathletic'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='mensuniforms'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boyscoatsjackets'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boyssweaters'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boysshirts'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boysunderwear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boyssleepwear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boyshosiery'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boysaccessories'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boyssuitssportcoats'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boyspants'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boysshortsexcathletic'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boysuniformsactivesportswear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womenscoats'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womensdresses'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womenssportcoats'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womenssweaters'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womensshirts'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womensskirts'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womenspants'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womensshorts_exathletic'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womensactivesportswear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womenssleepwear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womensundergarments'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womenshosiery'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womenssuits'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womensaccessories'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womensuniforms'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlscoatsjackets'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlsdressessuits'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlssportcoats'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlsskirtspants'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlsshortsexathletic'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlsactivesportswear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlsundergarments'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlshosiery'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlsaccessories'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlsuniforms'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='mensfootwear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='boysfootwear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='girlsfootwear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='womensfootwear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='infantscoats'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='infantsdresses'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='infantsundergarments'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='infantssleepingwear'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='infantsaccessories'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='sewingmaterial_clothes'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='sewingpatterns'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='watches'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='jewelry'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='travelitems'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='shoerepair'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='apparelcleaning_coinoperated'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='clothes_repair'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='clothing_rental'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='watchjewelryrepair'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='apparell_notcoinoperated'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='clothingstorage'))#?      
      
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='newspapers'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='magazinesperiodicals'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='books_bookclubs'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='books_exbookclubs'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='newsletters'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='outboardmotor'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='unpoweredboatstrailers'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='poweredsportsvehicles'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='recsportstables'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='bicycles'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='campingequipment'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='huntingfishingequipment'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='wintersportsequipment'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='miscsportsequipment'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='batterypoweredriders'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='playgroundequipment'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='musicalinstruments'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='film'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='photographicsupplies_exfilm')) # items tied to assets should not be considered as expenditure 
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='photographicequipment'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='petfood'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='petsupplies_nonfood'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='fireworks'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='souvenirs'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='visualgoods'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='clubmembershipfees'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='creditcardmembershipsfees'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='automobileserviceclubsfees'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='sportsclubmembershipfees'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='entertainmentvenuemembershipfees'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='sportingeventsmembershipfees'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='recreationallessonsfees'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='photographerfees'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='filmprocessing'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='petservices'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='veterinarian'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='miscentertainmentservices'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='campfees'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='sportphotopgraphicmusicequipmentrentalrepairs'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='videotape_rentals'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='videogames_coinoperated'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='sportvehiclerental'))#?
      
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='haircareproducts'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='nonelectrichairequipment'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='wigshairpieces'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='oralhygieneproducts'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='shavingneeds'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='cosmetics'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='miscpersonalcare'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='electricalpersonalcareequipment'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='femalepersonalcareservices'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='malepersonalcareservices'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='personalcareappliancesrentalrepair'))#?
      
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='funeralexpenses'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='cemeterylotsmaintenance'))#?
      x<-rbind(x,data.frame(category='personal_products', group='asset', shortname='computersfornonbusinessuse'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='computerinformationservices'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='telephoneansweringdevices'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='calculators'))#?
      x<-rbind(x,data.frame(category='personal_products', group='expenditure', shortname='typewritersnonbusiness'))#?
      
      
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='fueloil'))
      x<-rbind(x,data.frame(category='transport', group='asset', shortname='newcars'))#?
      x<-rbind(x,data.frame(category='transport', group='asset', shortname='newtrucks'))#?
      x<-rbind(x,data.frame(category='transport', group='asset', shortname='newmotorcycles'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='carlease'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='trucklease'))#?
      x<-rbind(x,data.frame(category='transport', group='asset', shortname='usedcars'))#?
      x<-rbind(x,data.frame(category='transport', group='asset', shortname='usedtrucks'))#?
      x<-rbind(x,data.frame(category='transport', group='asset', shortname='usedmotorcycles'))#?
      x<-rbind(x,data.frame(category='transport', group='asset', shortname='usedaircraft'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='gasoline'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='dieselfuel'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='gasohol'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='motoroil'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='coolant'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='tires'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='vehicleproducts'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='carequipment'))#?
      
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='vehicleaudioequipment_excludinglabor'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='miscsautorepair'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='carbodyworkholstery'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='transmissionrepair'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='driveshaftrepair'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='brakework'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='steeringrepair'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='coolingrepair'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='motortuneup'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='oilchange'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='caralignment'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='shockabsorberreplacement'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='brakeadjustment'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='gastankrepairreplacement'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='exhaustsystemrepair'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='electricalsystemrepair'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='motorrepair'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='vehicleinsurance'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='vehicleregistration_state'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='vehicleregistration_local'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='driverslicense'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='vehicleinspection'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='autorental_extrips'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='truckrental_extrips'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='parkingfeesy'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='tolls'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='towingcharges'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='boatsplansdockinglandingfees'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='motorcyclerentals'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='aircraftrentals'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='noncampertypetrailerrentals'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='airlinefares'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='intercitybusfares'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='intracitymasstransitfares'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='taxifares'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='intercitytrainfares'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='shipfares'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='privateschoolbus'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='carpoolnonmotortransportation'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='prescriptiondrugs'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='eyeglassesorcontactlenses'))#?
      x<-rbind(x,data.frame(category='transport', group='expenditure', shortname='overthecounterdrugs'))#?
      
      
      
      #fireinsurance - ignored
      #homeownerinsurance - ignored
      #propertytaxes - ignored
      #parking_owneddwelling - ignored
      #maintenancefeesassociation - ignored
      #septictankcleaning - ignored
      #trashcollection - ignored (could be added in the rent)
      #contributions
      #cashgifts
      #gifts_unpecified
      #alimonychildsupport
      #propertyassessment
      
      #householdappliancesrepair - ignored
      #tvradioexcarfittedrepair - ignored
      #furniturerepair
      #gardeningequipmentrepair
      #careforinvalids
      #miscshouseholdequipmentrepair
      
      #cottonballs
      #medicalequipment
      #supportivemedicalequipment
      #hearingaids
      #nonprescriptionvitamins
      #recreationaldrugs
      #physiciansservices
      #dentalservices
      #eyeexams
      #labtests
      #nonphysicalmedicalservices
      ##hospitalcare_notspecified
      #carenursinghome
      #medicalcare_other
      #medicalequipmentrentals
      #medicalequipmentrepair
      ##supportiveequipmentrepair
      #healthinsurance_nonspecified
      #healthinsurance_commercial
      #bluecross
      #healthmaintenanceplans
      #medicarepayments
      #miscfees
      #miscadmissionsfees
      #lotterieslosses
      #cigarettes
      #tobacco_other
      #smokingaccessories
      #marijuana
      
      #schoolsupplies
      #schoolbooks_college
      #schoolbooks_highschoolelementary
      #referencebooks
      #schoolbooksnurseryschool
      #tuition_college
      #tuition_highschoolelementary
      #schoolsexpense_other
      #tuition_otherschool
      #booksequipmentrental
      #legalfees
      #bankingservicesfees
      #safedepositboxrental
      #accountingfees
      #miscpersonalservices
      #homeownershipexpensenotspecified
      #taxesnotspecified
      
      return(x)
    }
    
    stop(paste("No groups declaration for year:",year))
  }
  
  return(new("USCEXNormalize",diary_info_columns_us_cex_2004=diary_info_columns_us_cex_2004, 
             ohs_info_columns_us_cex_2004=ohs_info_columns_us_cex_2004, 
             hh_us_cex_mapping_2004=hh_us_cex_mapping_2004, 
             ohs_mapping_us_cex_2004=ohs_mapping_us_cex_2004, 
             visible_categories_us_cex_2004=visible_categories_us_cex_2004, 
             cex_combined_years_ds=cex_combined_years_ds,
             cex_groups=cex_groups,
             ucc_codes_2009=ucc_codes_2009))
  
}