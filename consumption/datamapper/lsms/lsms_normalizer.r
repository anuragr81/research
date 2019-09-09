
if (isClass("LSMSNormalizer")){
  print ("Warning !!! previous definition of LSMSNormalizer would be overwritten.")
}

## all exported functions are declared here
setClass("LSMSNormalizer", representation(hh_mapping_lsms_2008= "function",
                                          get_lsms_secl_info_columns_2008="function",
                                          get_lsms_secl_fields_mapping_2008="function",
                                          ohs_seca_mapping_lsms_2008="function",
                                          ohs_info_columns_lsms_2008="function",
                                          ohs_mapping_lsms_2008="function",
                                          get_ohs_secc_columns_lsms_2008="function",
                                          get_ohs_secc_fields_mapping_lsms_2008="function",
                                          get_lsms_secj_info_columns_2008="function",
                                          get_lsms_secj_fields_mapping_2008="function",
                                          food_categories_lsms_2010="function",
                                          items_codes_2008="function",
                                          items_codes_2010="function",
                                          lsms_groups_pricebased_2010_2012="function",
                                          lsms_groups_qualitybased_2010_2012="function",
                                          lsms_groups_sparsenessbased_2010_2012="function",
                                          assets_order_2010_2012="function",
                                          items_codes_2012="function",
                                          items_codes_2014="function",
                                          asset_types_2010_2012="function",
                                          inheritable_assets_2010_2012="function",
                                          get_piece_measures="function",
                                          ignored_bad_units="function",
                                          get_group_qconv_factor="function",
                                          multiplyLsmsQuantities="function",
                                          get_lsms_secj_info_columns_2010="function", 
                                          get_lsms_secj_fields_mapping_2010="function", 
                                          get_lsms_secm_info_columns="function",
                                          get_region_popdensity_map="function",
                                          read_tnz="function",
                                          get_lsms_secm_fields_mapping="function",
                                          get_lsms_secl_info_columns_2010="function",
                                          get_lsms_secl_fields_mapping_2010="function",
                                          get_ohs_secc_columns_lsms_2010="function",
                                          get_ohs_secc_fields_mapping_lsms_2010="function",
                                          ohs_seccb_columns_lsms="function",
                                          ohs_seccb_mapping_lsms="function",
                                          ohs_seccj_columns_lsms_2010="function",
                                          ohs_seccj_mapping_lsms_2010="function",
                                          ohs_seccj_columns_lsms_2012="function",
                                          ohs_seccj_mapping_lsms_2012="function",
                                          ohs_seccj_columns_lsms_2008="function",
                                          ohs_seccj_mapping_lsms_2008="function",
                                          ohs_seccf_mapping_lsms_2014="function",
                                          ohs_seccf_columns_lsms_2014="function",
                                          items_market_price_codes_2014="function",
                                          ohs_seca_columns_lsms="function",
                                          ohs_seca_mapping_lsms_2010="function",
                                          area_code="function",
                                          analyse_cj="function",
                                          occupation_mapping="function",
                                          ohs_info_columns_lsms2010="function",
                                          get_ohs_info_columns="function",
                                          get_diary_info_columns="function",
                                          ohs_info_columns_lsms_2010="function",
                                          diary_info_columns_lsms_2010="function",
                                          diary_info_columns_lsms_2008="function",
                                          hh_mapping_lsms_2010="function",
                                          ohs_mapping_lsms_2010="function",
                                          get_lsms_sece1_columns_2010="function",
                                          get_lsms_sece2_columns_2010="function",
                                          get_lsms_sece_fields_mapping_2010="function",
                                          diary_info_columns_lsms_2012="function",
                                          diary_info_columns_lsms_2014="function",
                                          ohs_mapping_lsms_2012="function",
                                          ohs_mapping_lsms_2014="function",
                                          hh_mapping_lsms_2012="function",
                                          hh_mapping_lsms_2014="function",
                                          get_lsms_seck_info_columns_2012="function", 
                                          get_lsms_seck_info_columns_2014="function",
                                          get_lsms_seck_fields_mapping_2012="function",
                                          get_lsms_seck_fields_mapping_2014="function",
                                          ohs_seca_mapping_lsms_2012="function",
                                          ohs_seca_mapping_lsms_2014="function",
                                          ohs_info_columns_lsms_2012="function",
                                          ohs_info_columns_lsms_2014="function",
                                          get_ohs_secc_columns_lsms_2012="function",
                                          get_ohs_secc_columns_lsms_2014="function",
                                          get_ohs_secc_fields_mapping_lsms_2012="function",
                                          get_ohs_secc_fields_mapping_lsms_2014="function",
                                          get_lsms_secj_info_columns_2012="function",
                                          get_lsms_secj_info_columns_2014="function",
                                          get_lsms_secj_fields_mapping_2012="function",
                                          get_lsms_secj_fields_mapping_2014="function",
                                          get_diary_secn_columns_lsms_2008= "function",
                                          get_diary_secn_columns_lsms_2010="function",
                                          get_diary_secn_columns_lsms_2012="function",
                                          get_diary_secn_fields_mapping_lsms_2008="function",
                                          get_diary_secn_fields_mapping_lsms_2010="function",
                                          get_diary_secn_fields_mapping_lsms_2012="function",
                                          computeYearValues="function",
                                          decode_clusterid="function",
                                          computeLsmsSelfemployedValues="function",
                                          infer_lsms_sece_total_income="function"))

lsms_normalizer<-function() {
  
  #PP.........1 ADULT......2
  #PRIMARY  SECONDARY
  #D1........11 F1........21
  #D2........12 F2........22
  #D3........13 F3........23
  #D4........14 F4........24
  #D5........15 'O' +COURSE .25
  #D6........16 F5........31,
  #D7........17 F6........32
  #D8........18 'A'+COURSE .33
  #OSC.......19 DIPLOMA...34
  #MS+COURSE.20
  #UNIVERSITY
  #U1........41 U2........42
  #U3........43 U4........44
  #U5&+......45
  
  food_categories_lsms_2010<-function(){
    return(c("10101", "10102", "10103", "10104", "10105", "10106", "10107", "10108", "10109", "10110", 
             "10111", "10112", "10201", "10202", "10203", "10204", "10205", "10206", "10207", "10301", 
             "10302", "10303", "10401", "10501", "10502", "10503", "10504", "10601", "10602", "10603", 
             "10701", "10702", "10703", "10704", "10801", "10802", "10803", "10804", "10805", "10806", 
             "10807", "10808", "10809", "10810", "10901", "10902", "10903", "11001", "11002", "11003", 
             "11004", "11101", "11102", "11103", "11104", "11105", "11106", "11107", "11108"))
  }
  
  get_piece_measures<-function (year){
    if (year == 2010){
      r=data.frame(item=NULL,lwp_unit=NULL,piecefactor=NULL,piece_unit=NULL)
      r=rbind(r,data.frame(item=c(10701),lwp_unit=c(5),piecefactor=c(.12),piece_unit=c(1)))
      r=rbind(r,data.frame(item=c(10302),lwp_unit=c(5),piecefactor=c(.035),piece_unit=c(1)))
      r=rbind(r,data.frame(item=c(10703),lwp_unit=c(5),piecefactor=c(.18),piece_unit=c(1)))
      r=rbind(r,data.frame(item=c(10804),lwp_unit=c(5),piecefactor=c(1.0),piece_unit=c(1)))
      r=rbind(r,data.frame(item=c(10805),lwp_unit=c(5),piecefactor=c(.7),piece_unit=c(1))) # estimate
      r=rbind(r,data.frame(item=c(10807),lwp_unit=c(5),piecefactor=c(.08),piece_unit=c(1)))
      r=rbind(r,data.frame(item=c(11104),lwp_unit=c(5),piecefactor=c(.35),piece_unit=c(3)))
      
      return(r)
    }
    stop(paste("get_piece_measures: Year", year, "not supported"))
  }
  
  ignored_bad_units <- function(year,datConsum){
    if (year==2010){
      # For 2010 - ignore:
      # items with lwp_unit == NA 
      hhidData <- subset( datConsum, !is.element (hhid,unique(subset(datConsum, (is.na(lwp_unit) ) & item > 10000 )$hhid)) )
      # coconut in l/ml
      hhidData <- subset(hhidData,!is.element(hhid,unique ( subset(hhidData,  is.element(lwp_unit,c(3,4)) & item==10502)$hhid ) ) ) 
      # fish_seafood (10808) in l/mls
      hhidData <- subset(hhidData,!is.element(hhid,unique ( subset(hhidData,  is.element(lwp_unit,c(3,4)) & item==10808)$hhid ) ) )
      
      # canned_milk (10903) in l/ml
      hhidData <- subset(hhidData,!is.element(hhid,unique ( subset(hhidData,  is.element(lwp_unit,c(3,4)) & item==10903)$hhid ) ) )
      # salt in l/ml
      hhidData <- subset(hhidData,!is.element(hhid,unique ( subset(hhidData,  is.element(lwp_unit,c(3,4)) & item==11003)$hhid ) ) )
      # potatoes in l/ml
      
      hhidData <- subset(hhidData,!is.element(hhid,unique ( subset(hhidData,  is.element(lwp_unit,c(3,4)) & item==10205)$hhid ) ) )
      
      hhidData <- subset(hhidData,!is.element(hhid,unique ( subset(hhidData, item==11105)$hhid ) ) )
      
      origSize = length(unique(datConsum$hhid))
      finalSize = length(unique(hhidData$hhid))
      print(paste("ignored_bad_units - Ignored",origSize-finalSize, "(",1-finalSize/origSize,"%) households because of bad units"))
      return(setdiff(unique(datConsum$hhid),unique(hhidData$hhid)))
    }
    stop(paste("ignore_bad_units - year:",year," not supported"))
  }
  
  get_group_qconv_factor<-function (year){
    if (year == 2010){
      r=data.frame(item=NULL,lwp_unit=NULL,group=NULL,group_unit=NULL, unif_factor=NULL)
      r=rbind(r,data.frame(item=c(11101),lwp_unit=c(1), group=c("beverages"),group_unit =c(3) , unif_factor=c(130./15.))) # litres of tea in a kilogram
      r=rbind(r,data.frame(item=c(11102),lwp_unit=c(1), group=c("beverages"),group_unit=c(3), unif_factor=c(130./15.))) # litres of coffee in a kilogram
      r=rbind(r,data.frame(item=c(11103),lwp_unit=c(1), group=c("beverages"),group_unit=c(3) , unif_factor=c(130./15.))) # litres of miscdrinkpowder in a kilogram
      r=rbind(r,data.frame(item=c(11104),lwp_unit=c(1), group=c("beverages"),group_unit=c(3) , unif_factor=c(1.))) # litres of water in a kilogram
      r=rbind(r,data.frame(item=c(10303),lwp_unit=c(1), group=c("sugars"),group_unit=c(3) , unif_factor=c(0.69586515))) # litres of honey in a kg
      r=rbind(r,data.frame(item=c(11001),lwp_unit=c(1), group=c("oil"),group_unit=c(3) , unif_factor=c(1.07759))) # litres of cooking_oil in a kg
      r=rbind(r,data.frame(item=c(11002),lwp_unit=c(1), group=c("oil"),group_unit=c(3) , unif_factor=c(1.253))) # litres of butter_margarine in a kg
      r=rbind(r,data.frame(item=c(10902),lwp_unit=c(3), group=c("fat"),group_unit=c(1) , unif_factor=c(1.02287))) # kgs of youghurt in a liter
      r=rbind(r,data.frame(item=c(10901),lwp_unit=c(1), group=c("milk"),group_unit=c(3) , unif_factor=c(.977))) # liters of milk in a kg
      r=rbind(r,data.frame(item=c(11106),lwp_unit=c(1), group=c("alcohol"),group_unit=c(3) , unif_factor=c(1/1.00766))) # liters of beer in a kg
      r=rbind(r,data.frame(item=c(11107),lwp_unit=c(1), group=c("alcohol"),group_unit=c(3) , unif_factor=c(1/1.00766))) # liters of beer in a kg
      return(r)
      
    }
    stop(paste("get_group_qconv_factor: Year", year, "not supported"))
  }
  
  
  items_codes_2008<-function() { 
    r=data.frame(item=NULL,code=NULL,stringsAsFactors = FALSE)
    r=rbind(r,data.frame(item='Cigarettes',shortname='cigarettes',code='101', category = "misc"))
    r=rbind(r,data.frame(item='Matches',shortname='matches',code='102', category = "energy"))
    r=rbind(r,data.frame(item='Public Transport',shortname='public_transport',code='103',category="transport"))
    
    r=rbind(r,data.frame(item='Kerosene',shortname='kerosene',code='201',category="energy"))
    r=rbind(r,data.frame(item='Kerosene',shortname='kerosene',code='11201',category="energy"))
    r=rbind(r,data.frame(item='Electricity',shortname='electricity',code='202',category="energy"))
    r=rbind(r,data.frame(item='Gas',shortname='gas', code='203',category="energy"))
    r=rbind(r,data.frame(item='Water',shortname='water',code='204',category="food"))
    r=rbind(r,data.frame(item='Petrol',shortname='petrol',code='205',category="energy"))
    r=rbind(r,data.frame(item='Cellphone voucher',shortname='cellphone_voucher', code='206',category="personal_products"))
    r=rbind(r,data.frame(item='Charcoal',shortname='charcoal',code='207',category="energy"))
    r=rbind(r,data.frame(item='Charcoal',shortname='charcoal',code='11207',category="energy"))
    r=rbind(r,data.frame(item='Milling fees grain',shortname='milling',code='208',category="food"))
    r=rbind(r,data.frame(item='Milling fees grain',shortname='milling',code='11208',category="food"))
    r=rbind(r,data.frame(item='Bar soap',shortname='bar_soap',code='209',category="personal_products"))
    r=rbind(r,data.frame(item='Clothes soap',shortname='clothes_soap',code='210',category="personal_products"))
    r=rbind(r,data.frame(item='Toothpaste',shortname='toothpaste',code='211',category="personal_products"))
    r=rbind(r,data.frame(item='Toilet Paper',shortname='toilet_paper',code='212',category="personal_products"))
    r=rbind(r,data.frame(item='Skin Cream',shortname='skin_cream',code='213',category="personal_products"))
    r=rbind(r,data.frame(item='Others Personal Products',shortname='other_personal',code='214',category="personal_products"))
    r=rbind(r,data.frame(item='Household cleaning Products',shortname='misc_cleaning',code='215',category="personal_products"))
    r=rbind(r,data.frame(item='Light bulbs',shortname='light_bulbs',code='216',category="housing"))
    r=rbind(r,data.frame(item='Phone, internet fees',shortname='phone',code='217',category="personal_products"))
    r=rbind(r,data.frame(item='Donations',shortname='donation',code='218',category="misc"))
    r=rbind(r,data.frame(item='Motor vehicle repairs',shortname='motor_repair',code='219',category="transport"))
    r=rbind(r,data.frame(item='Bicycle service',shortname='bicycle_repair',code='220',category="transport"))
    r=rbind(r,data.frame(item='Servant Wages',shortname='services',code='221',category="social_functions"))
    r=rbind(r,data.frame(item='Mortgage payment',shortname='mortgage',code='222',category="ignored"))
    r=rbind(r,data.frame(item='House repair',shortname='house_repair',code='223',category="ignored"))
    ####
    r=rbind(r,data.frame(item='CarpetsRugs',shortname='carpet',code='301',category="housing"))
    r=rbind(r,data.frame(item='Linen',shortname='linen',code='302',category="housing"))
    r=rbind(r,data.frame(item='Mats',shortname='mat',code='303',category="housing"))
    r=rbind(r,data.frame(item='MosquitoNet',shortname='mosquito_net',code='304',category="housing"))
    r=rbind(r,data.frame(item='Mattress',shortname='mattress',code='305',category="housing"))
    r=rbind(r,data.frame(item='SportsHobbyEquipment',shortname='sports_hobby',code='306',category="personal_products"))
    r=rbind(r,data.frame(item='FilmCamera',shortname='camera',code='307',category="personal_products"))
    r=rbind(r,data.frame(item='BuidingMaterial',shortname='building_material',code='308',category="housing"))
    r=rbind(r,data.frame(item='CouncilRates',shortname='council_rates',code='309',category="ignored"))
    r=rbind(r,data.frame(item='Insurance',shortname='insurance',code='310',category="housing"))
    r=rbind(r,data.frame(item='TheftLosses',shortname='theft_losses',code='311',category="misc"))
    r=rbind(r,data.frame(item='LegalFeesFines',shortname='legalfines',code='312',category="misc"))
    r=rbind(r,data.frame(item='BrideMarriageCeremonyCosts',shortname='marriage',code='313',category="social_functions"))
    r=rbind(r,data.frame(item='FuneralCosts',shortname='funeral',code='314',category="social_functions"))
    r=rbind(r,data.frame(item='OherCosts',shortname='other_costs',code='315',category="social_functions"))
    
    r=rbind(r,data.frame(item='ConsumerDurableRepairs',shortname='consumer_durables_repair',code='316',category="personal_products"))
    r=rbind(r,data.frame(item='PropertyIncomeTax',shortname='taxes',code='317',category="misc"))
    
    r=rbind(r,data.frame(item='Dwelling Repairs-maintenance',shortname='houserepair',code='318',category="housing"))
    
    r=rbind(r,data.frame(item='mensclothes',shortname='mensclothes',code='319',category="personal_products"))
    r=rbind(r,data.frame(item='womensclothes',shortname='womensclothes',code='320',category="personal_products"))
    r=rbind(r,data.frame(item='childrensclothes',shortname='childrensclothes',code='321',category="personal_products"))
    r=rbind(r,data.frame(item='mensshoes',shortname='mensshoes',code='322',category="personal_products"))
    r=rbind(r,data.frame(item='womensshoes',shortname='womensshoes',code='323',category="personal_products"))
    r=rbind(r,data.frame(item='childrensshoes',shortname='childrensshoes',code='324',category="personal_products"))
    
    r=rbind(r,data.frame(item='Woodpoles',shortname='bamboo',code='325',category="housing"))
    r=rbind(r,data.frame(item='ThatchingGrass',shortname='grass',code='326',category="housing"))
    
    
    r=rbind(r,data.frame(item='Radio and Radio Cassette',shortname='radio',code='401',category="personal_products"))                                  
    r=rbind(r,data.frame(item='Telephone(landline)',shortname='landline',code='402',category="personal_products"))                                    
    r=rbind(r,data.frame(item='Telephone(mobile)',shortname='mobile',code='403',category="personal_products"))                                        
    r=rbind(r,data.frame(item='Refridgerator or freezer',shortname='refrigerator',code='404',category="housing"))                           
    r=rbind(r,data.frame(item='Sewing Machine',shortname='sewingmachine',code='405',category="housing"))                                    
    r=rbind(r,data.frame(item='Television',shortname='tv',code='406',category="housing"))                                                   
    r=rbind(r,data.frame(item='Video / DVD',shortname='videoplayer',code='407',category="personal_products"))                                         
    r=rbind(r,data.frame(item='Chairs',shortname='chair',code='408',category="housing"))                                                    
    r=rbind(r,data.frame(item='Sofas',shortname='sofa',code='409',category="housing"))                                                      
    r=rbind(r,data.frame(item='Tables',shortname='table',code='410',category="housing"))                                                    
    r=rbind(r,data.frame(item='Watches',shortname='watch',code='411',category="personal_products"))                                                   
    r=rbind(r,data.frame(item='Beds',shortname='bed',code='412',category="housing"))                                                        
    r=rbind(r,data.frame(item='Cupboards, chest-of-drawers, boxes, wardrobes,bookcases',shortname='cupboard',code='413',category="housing"))
    r=rbind(r,data.frame(item='Lanterns',shortname='lantern',code='414',category="housing"))                                                
    r=rbind(r,data.frame(item='Computer',shortname='computer',code='415',category="personal_products"))                                               
    r=rbind(r,data.frame(item='Cooking pots, Cups, other kitchen utencils',shortname='cookingpot',code='416',category="food"))           
    r=rbind(r,data.frame(item='Mosquito net',shortname='mosquitonet',code='417',category="housing"))                                        
    r=rbind(r,data.frame(item='Iron (Charcoal or electric)',shortname='iron',code='418',category="personal_products"))                                
    r=rbind(r,data.frame(item='Electric/gas stove',shortname='stove_electricgas',code='419',category="housing"))
    r=rbind(r,data.frame(item='Other stove',shortname='stove_other',code='420',category="housing"))
    r=rbind(r,data.frame(item='Water-heater',shortname='waterheater',code='421',category="personal_products"))
    r=rbind(r,data.frame(item='Record/cassette player, tape recorder',shortname='musicplayer',code='422',category="personal_products"))               
    r=rbind(r,data.frame(item='Complete music system',shortname='musicsystem',code='423',category="personal_products"))                               
    r=rbind(r,data.frame(item='Books (not school books)',shortname='bookexschool',code='424',category="personal_products"))                           
    r=rbind(r,data.frame(item='Motor Vehicles',shortname='car',code='425',category="transport"))                                              
    r=rbind(r,data.frame(item='Motor cycle',shortname='motorbike',code='426',category="transport"))                                           
    r=rbind(r,data.frame(item='Bicycle',shortname='bike',code='427',category="transport"))                                                    
    r=rbind(r,data.frame(item='Carts',shortname='cart',code='428',category="transport"))                                                      
    r=rbind(r,data.frame(item='Animal Cart',shortname='animalcart',code='429',category="transport"))                                          
    r=rbind(r,data.frame(item='Boat/canoe',shortname='boat',code='430',category="transport"))                                               
    r=rbind(r,data.frame(item='Wheel barrow',shortname='wheelbarrow',code='431',category="housing"))                                        
    r=rbind(r,data.frame(item='Livestock',shortname='livestock',code='432',category="housing"))                                             
    r=rbind(r,data.frame(item='Poultry',shortname='poultry',code='433',category="housing"))                                                 
    r=rbind(r,data.frame(item='Outboard engine',shortname='engine_outboard',code='434',category="housing"))                                 
    r=rbind(r,data.frame(item='Donkeys',shortname='donkey',code='435',category="housing"))                                                  
    r=rbind(r,data.frame(item='Fields/Land',shortname='land',code='436',category="housing"))                                                
    r=rbind(r,data.frame(item='House(s)',shortname='house',code='437',category="housing"))                                                  
    r=rbind(r,data.frame(item='Airconditioner/Fans',shortname='ac_fan',code='438',category="housing"))                                      
    r=rbind(r,data.frame(item='Dish antena/decoder',shortname='dishtv',code='439',category="housing"))                                     
    r=rbind(r,data.frame(item='Hoes',shortname='hoe',code='440',category="housing"))                                                        
    r=rbind(r,data.frame(item='Spraying machine',shortname='spraymachine',code='441',category="housing"))                                   
    r=rbind(r,data.frame(item='Water pumping set',shortname='waterpump',code='442',category="housing"))                                     
    r=rbind(r,data.frame(item='Reapers',shortname='reaper',code='443',category="housing"))                                                  
    r=rbind(r,data.frame(item='Tractor',shortname='tractor',code='444',category="housing"))                                                 
    r=rbind(r,data.frame(item='Trailer for tractors etc.',shortname='trailer',code='445',category="housing"))                               
    r=rbind(r,data.frame(item='Plough etc.',shortname='plough',code='446',category="housing"))                                              
    r=rbind(r,data.frame(item='Harrow',shortname='harrow',code='447',category="housing"))                                                   
    r=rbind(r,data.frame(item='Milking machine',shortname='milkingmachine',code='448',category="housing"))                                  
    r=rbind(r,data.frame(item='Harvesting and threshing machine',shortname='harvester',code='449',category="housing"))                      
    r=rbind(r,data.frame(item='Hand milling machine',shortname='handmill',code='450',category="housing"))                                   
    r=rbind(r,data.frame(item='Coffee pulping machine',shortname='coffeepulpingmachine',code='451',category="housing"))                     
    r=rbind(r,data.frame(item='Fertilizer distributor',shortname='fertiliserdistributor',code='452',category="housing"))                    
    r=rbind(r,data.frame(item='Power tiller',shortname='powertiller',code='453',category="housing"))                    
    
    r=rbind(r,data.frame(item='Firewood',shortname='firewood',code='10999',category="housing"))                    
    
    
    r=rbind(r,data.frame(item='Rice (paddy)',shortname='rice_paddy',code='10101',category="food"))
    r=rbind(r,data.frame(item='Rice (husked)',shortname='rice_husked',code='10102',category="food"))
    r=rbind(r,data.frame(item='Maize (green, cob)',shortname='maize_green',code='10103',category="food"))
    r=rbind(r,data.frame(item='Maize (grain)',shortname='maize_grain',code='10104',category="food"))
    r=rbind(r,data.frame(item='Maize (flour)',shortname='maize_flour',code='10105',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (grain)',shortname='millet_grain',code='10106',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (flour)',shortname='millet_flour',code='10107',category="food"))
    r=rbind(r,data.frame(item='Wheat, barley etc.',shortname='wheat',code='10108',category="food"))
    r=rbind(r,data.frame(item='Bread',shortname='bread',code='10109',category="food"))
    r=rbind(r,data.frame(item='Buns, cakes and biscuts',shortname='bunscakes',code='10110',category="food"))
    r=rbind(r,data.frame(item='Macaroini, spaghetti',shortname='pasta',code='10111',category="food"))
    r=rbind(r,data.frame(item='Other cereal products',shortname='othercereal',code='10112',category="food"))
    r=rbind(r,data.frame(item='Cassava fresh',shortname='cassava_fresh',code='10201',category="food"))
    r=rbind(r,data.frame(item='Cassava dry/flour',shortname='cassava_flour',code='10202',category="food"))
    r=rbind(r,data.frame(item='Sweet potatoes',shortname='sweet_potato',code='10203',category="food"))
    r=rbind(r,data.frame(item='Yams/cocoyams',shortname='yam',code='10204',category="food"))
    r=rbind(r,data.frame(item='Irish potatoes',shortname='potatoes',code='10205',category="food"))
    r=rbind(r,data.frame(item='Cooking bananas, plantains',shortname='banana_green', code='10206',category="food"))
    r=rbind(r,data.frame(item='Other starches',shortname='othervegstarch',code='10207',category="food"))
    r=rbind(r,data.frame(item='Sugar',shortname='sugar',code='10301',category="food"))
    r=rbind(r,data.frame(item='Sweets',shortname='sweet',code='10302',category="food"))
    r=rbind(r,data.frame(item='Honey, jams etc.',shortname='honey',code='10303',category="food"))
    r=rbind(r,data.frame(item='Peas, beans',shortname='pulses',code='10401',category="food"))
    r=rbind(r,data.frame(item='Groundnuts in shell/shelled',shortname='peanuts',code='10501',category="food"))
    r=rbind(r,data.frame(item='Coconuts (mature/immature)',shortname='coconut',code='10502',category="food"))
    r=rbind(r,data.frame(item='Cashew, almonds, nuts',shortname='cashew_almonds', code='10503',category="food"))
    r=rbind(r,data.frame(item='Seeds from nuts',shortname='nut_products', code='10504',category="food"))
    r=rbind(r,data.frame(item='Onions, tomatoes, carrots and green pepper, other viungo',shortname='onion',code='10601',category="food"))
    r=rbind(r,data.frame(item='Spinach, cabbage and other green vegetables',shortname='greens',code='10602',category="food"))
    r=rbind(r,data.frame(item='Canned, dried and wild vegetables',shortname='dried_canned_veg',code='10603',category="food"))
    r=rbind(r,data.frame(item='Ripe bananas',shortname='banana_ripe',code='10701',category="food"))
    r=rbind(r,data.frame(item='Citrus fruits (oranges, lemon, tangarines, etc.)',shortname='citrus',code='10702',category="food"))
    r=rbind(r,data.frame(item='Mangoes, avocadoes and other fruits',shortname='mangoes',code='10703',category="food"))
    r=rbind(r,data.frame(item='Sugarcane',shortname='sugarcane',code='10704',category="food"))
    r=rbind(r,data.frame(item='Goat meat',shortname='goat',code='10801',category="food"))
    r=rbind(r,data.frame(item='Beef including minced sausage',shortname='beef', code='10802',category="food"))
    r=rbind(r,data.frame(item='Pork including sauages and bacon',shortname='pork',code='10803',category="food"))
    r=rbind(r,data.frame(item='Chicken and other poultry',shortname='chicken',code='10804',category="food"))
    r=rbind(r,data.frame(item='Wild birds and insects',shortname='wild_birds',code='10805',category="food"))
    r=rbind(r,data.frame(item='Other domestic/wild meat products',shortname='wild_meat',code='10806',category="food"))
    r=rbind(r,data.frame(item='Eggs',shortname='eggs',code='10807',category="food"))
    r=rbind(r,data.frame(item='Fresh fish and seafood (including dagaa)',shortname='fish_seafood',code='10808',category="food"))
    r=rbind(r,data.frame(item='Dried/salted/canned fish and seafood',shortname='dried_canned_fish', code='10809',category="food"))
    r=rbind(r,data.frame(item='Package fish',shortname='packaged_fish', code='10810',category="food"))
    r=rbind(r,data.frame(item='Fresh milk',shortname='fresh_milk', code='10901',category="food"))
    r=rbind(r,data.frame(item='Milk products (like cream, cheese, yoghurt etc)',shortname='milk_products',code='10902',category="food"))
    r=rbind(r,data.frame(item='Canned milk/milk powder',shortname='canned_milk', code='10903',category="food"))
    r=rbind(r,data.frame(item='Cooking oil',shortname='cooking_oil', code='11001',category="food"))
    r=rbind(r,data.frame(item='Butter, margarine, ghee and other fat products',shortname='butter_margarine', code='11002',category="food"))
    r=rbind(r,data.frame(item='Salt',shortname='salt',code='11003',category="food"))
    r=rbind(r,data.frame(item='Other spices',shortname='spices', code='11004',category="food"))
    r=rbind(r,data.frame(item='Tea dry',shortname='tea', code='11101',category="food"))
    r=rbind(r,data.frame(item='Coffee and cocoa',shortname='coffee',code='11102',category="food"))
    r=rbind(r,data.frame(item='Other raw materals for drinks',shortname='miscdrinkpowder',code='11103',category="food"))
    r=rbind(r,data.frame(item='Bottled/canned soft drinks (soda, juice, water)',shortname='canned_drink',code='11104',category="food"))
    r=rbind(r,data.frame(item='Prepared tea, coffee',shortname='readymade_tea_coffee',code='11105',category="food"))
    r=rbind(r,data.frame(item='Bottled beer',shortname='beer',code='11106',category="food"))
    r=rbind(r,data.frame(item='Local brews',shortname='brews',code='11107',category="food"))
    r=rbind(r,data.frame(item='Wine and spirits',shortname='winespirits',code='11108',category="food"))
    return(r)
  }
  
  
  
  
  items_codes_2010<-function() { 
    r=data.frame(item=NULL,code=NULL,stringsAsFactors = FALSE)
    r=rbind(r,data.frame(item='Cigarettes',shortname='cigarettes',code='101', category = "misc"))
    r=rbind(r,data.frame(item='Matches',shortname='matches',code='102', category = "energy"))
    r=rbind(r,data.frame(item='Public Transport',shortname='public_transport',code='103',category="transport"))
    r=rbind(r,data.frame(item='Kerosene',shortname='kerosene',code='201',category="energy"))
    r=rbind(r,data.frame(item='Electricity',shortname='electricity',code='202',category="energy"))
    r=rbind(r,data.frame(item='Gas',shortname='gas', code='203',category="energy"))
    r=rbind(r,data.frame(item='Water',shortname='water',code='204',category="food"))
    r=rbind(r,data.frame(item='Petrol',shortname='petrol',code='205',category="energy"))
    r=rbind(r,data.frame(item='Cellphone voucher',shortname='cellphone_voucher', code='206',category="personal_products"))
    r=rbind(r,data.frame(item='Charcoal',shortname='charcoal',code='207',category="energy"))
    r=rbind(r,data.frame(item='Milling fees grain',shortname='milling',code='208',category="food"))
    r=rbind(r,data.frame(item='Bar soap',shortname='bar_soap',code='209',category="personal_products"))
    r=rbind(r,data.frame(item='Clothes soap',shortname='clothes_soap',code='210',category="personal_products"))
    r=rbind(r,data.frame(item='Toothpaste',shortname='toothpaste',code='211',category="personal_products"))
    r=rbind(r,data.frame(item='Toilet Paper',shortname='toilet_paper',code='212',category="personal_products"))
    r=rbind(r,data.frame(item='Skin Cream',shortname='skin_cream',code='213',category="personal_products"))
    r=rbind(r,data.frame(item='Others Personal Products',shortname='other_personal',code='214',category="personal_products"))
    r=rbind(r,data.frame(item='Household cleaning Products',shortname='misc_cleaning',code='215',category="personal_products"))
    r=rbind(r,data.frame(item='Light bulbs',shortname='light_bulbs',code='216',category="housing"))
    r=rbind(r,data.frame(item='Phone, internet fees',shortname='phone',code='217',category="personal_products"))
    r=rbind(r,data.frame(item='Donations',shortname='donation',code='218',category="misc"))
    r=rbind(r,data.frame(item='Motor vehicle repairs',shortname='motor_repair',code='219',category="transport"))
    r=rbind(r,data.frame(item='Bicycle service',shortname='bicycle_repair',code='220',category="transport"))
    r=rbind(r,data.frame(item='Servant Wages',shortname='services',code='221',category="social_functions"))
    r=rbind(r,data.frame(item='Mortgage payment',shortname='mortgage',code='222',category="ignored"))
    r=rbind(r,data.frame(item='House repair',shortname='house_repair',code='223',category="ignored"))
    r=rbind(r,data.frame(item='Household items repair',shortname='household_products_repair',code='224',category="personal_products"))
    r=rbind(r,data.frame(item='CarpetsRugs',shortname='carpet',code='301',category="housing"))
    r=rbind(r,data.frame(item='Linen',shortname='linen',code='302',category="housing"))
    r=rbind(r,data.frame(item='Mats',shortname='mat',code='303',category="housing"))
    r=rbind(r,data.frame(item='MosquitoNet',shortname='mosquito_net',code='304',category="housing"))
    r=rbind(r,data.frame(item='Mattress',shortname='mattress',code='305',category="housing"))
    r=rbind(r,data.frame(item='SportsHobbyEquipment',shortname='sports_hobby',code='306',category="personal_products"))
    r=rbind(r,data.frame(item='FilmCamera',shortname='camera',code='307',category="personal_products"))
    r=rbind(r,data.frame(item='BuidingMaterial',shortname='building_material',code='308',category="housing"))
    r=rbind(r,data.frame(item='CouncilRates',shortname='council_rates',code='309',category="ignored"))
    r=rbind(r,data.frame(item='Insurance',shortname='insurance',code='310',category="housing"))
    r=rbind(r,data.frame(item='TheftLosses',shortname='theft_losses',code='311',category="misc"))
    r=rbind(r,data.frame(item='LegalFeesFines',shortname='legalfines',code='312',category="misc"))
    r=rbind(r,data.frame(item='BridePrice',shortname='bride_price',code='313',category="social_functions"))
    r=rbind(r,data.frame(item='MarriageCeremonyCosts',shortname='marriage',code='314',category="social_functions"))
    r=rbind(r,data.frame(item='FuneralCosts',shortname='funeral',code='315',category="social_functions"))
    r=rbind(r,data.frame(item='ConsumerDurableRepairs',shortname='consumer_durables_repair',code='316',category="personal_products"))
    r=rbind(r,data.frame(item='PropertyIncomeTax',shortname='taxes',code='317',category="misc"))
    r=rbind(r,data.frame(item='Woodpoles',shortname='bamboo',code='318',category="housing"))
    r=rbind(r,data.frame(item='ThatchingGrass',shortname='grass',code='319',category="housing"))
    
    
    r=rbind(r,data.frame(item='Radio and Radio Cassette',shortname='radio',code='401',category="personal_products"))                                  
    r=rbind(r,data.frame(item='Telephone(landline)',shortname='landline',code='402',category="personal_products"))                                    
    r=rbind(r,data.frame(item='Telephone(mobile)',shortname='mobile',code='403',category="personal_products"))                                        
    r=rbind(r,data.frame(item='Refridgerator or freezer',shortname='refrigerator',code='404',category="housing"))                           
    r=rbind(r,data.frame(item='Sewing Machine',shortname='sewingmachine',code='405',category="housing"))                                    
    r=rbind(r,data.frame(item='Television',shortname='tv',code='406',category="housing"))                                                   
    r=rbind(r,data.frame(item='Video / DVD',shortname='videoplayer',code='407',category="personal_products"))                                         
    r=rbind(r,data.frame(item='Chairs',shortname='chair',code='408',category="housing"))                                                    
    r=rbind(r,data.frame(item='Sofas',shortname='sofa',code='409',category="housing"))                                                      
    r=rbind(r,data.frame(item='Tables',shortname='table',code='410',category="housing"))                                                    
    r=rbind(r,data.frame(item='Watches',shortname='watch',code='411',category="personal_products"))                                                   
    r=rbind(r,data.frame(item='Beds',shortname='bed',code='412',category="housing"))                                                        
    r=rbind(r,data.frame(item='Cupboards, chest-of-drawers, boxes, wardrobes,bookcases',shortname='cupboard',code='413',category="housing"))
    r=rbind(r,data.frame(item='Lanterns',shortname='lantern',code='414',category="housing"))                                                
    r=rbind(r,data.frame(item='Computer',shortname='computer',code='415',category="personal_products"))                                               
    r=rbind(r,data.frame(item='Cooking pots, Cups, other kitchen utencils',shortname='cookingpot',code='416',category="food"))           
    r=rbind(r,data.frame(item='Mosquito net',shortname='mosquitonet',code='417',category="housing"))                                        
    r=rbind(r,data.frame(item='Iron (Charcoal or electric)',shortname='iron',code='418',category="personal_products"))                                
    r=rbind(r,data.frame(item='Electric/gas stove',shortname='stove_electricgas',code='419',category="housing"))
    r=rbind(r,data.frame(item='Other stove',shortname='stove_other',code='420',category="housing"))
    r=rbind(r,data.frame(item='Water-heater',shortname='waterheater',code='421',category="personal_products"))
    r=rbind(r,data.frame(item='Record/cassette player, tape recorder',shortname='musicplayer',code='422',category="personal_products"))               
    r=rbind(r,data.frame(item='Complete music system',shortname='musicsystem',code='423',category="personal_products"))                               
    r=rbind(r,data.frame(item='Books (not school books)',shortname='bookexschool',code='424',category="personal_products"))                           
    r=rbind(r,data.frame(item='Motor Vehicles',shortname='car',code='425',category="transport"))                                              
    r=rbind(r,data.frame(item='Motor cycle',shortname='motorbike',code='426',category="transport"))                                           
    r=rbind(r,data.frame(item='Bicycle',shortname='bike',code='427',category="transport"))                                                    
    r=rbind(r,data.frame(item='Carts',shortname='cart',code='428',category="transport"))                                                      
    r=rbind(r,data.frame(item='Animal Cart',shortname='animalcart',code='429',category="transport"))                                          
    r=rbind(r,data.frame(item='Boat/canoe',shortname='boat',code='430',category="transport"))                                               
    r=rbind(r,data.frame(item='Wheel barrow',shortname='wheelbarrow',code='431',category="housing"))                                        
    r=rbind(r,data.frame(item='Livestock',shortname='livestock',code='432',category="housing"))                                             
    r=rbind(r,data.frame(item='Poultry',shortname='poultry',code='433',category="housing"))                                                 
    r=rbind(r,data.frame(item='Outboard engine',shortname='engine_outboard',code='434',category="housing"))                                 
    r=rbind(r,data.frame(item='Donkeys',shortname='donkey',code='435',category="housing"))                                                  
    r=rbind(r,data.frame(item='Fields/Land',shortname='land',code='436',category="housing"))                                                
    r=rbind(r,data.frame(item='House(s)',shortname='house',code='437',category="housing"))                                                  
    r=rbind(r,data.frame(item='Airconditioner/Fans',shortname='ac_fan',code='438',category="housing"))                                      
    r=rbind(r,data.frame(item='Dish antena/decoder',shortname='dishtv',code='439',category="housing"))                                     
    r=rbind(r,data.frame(item='Hoes',shortname='hoe',code='440',category="housing"))                                                        
    r=rbind(r,data.frame(item='Spraying machine',shortname='spraymachine',code='441',category="housing"))                                   
    r=rbind(r,data.frame(item='Water pumping set',shortname='waterpump',code='442',category="housing"))                                     
    r=rbind(r,data.frame(item='Reapers',shortname='reaper',code='443',category="housing"))                                                  
    r=rbind(r,data.frame(item='Tractor',shortname='tractor',code='444',category="housing"))                                                 
    r=rbind(r,data.frame(item='Trailer for tractors etc.',shortname='trailer',code='445',category="housing"))                               
    r=rbind(r,data.frame(item='Plough etc.',shortname='plough',code='446',category="housing"))                                              
    r=rbind(r,data.frame(item='Harrow',shortname='harrow',code='447',category="housing"))                                                   
    r=rbind(r,data.frame(item='Milking machine',shortname='milkingmachine',code='448',category="housing"))                                  
    r=rbind(r,data.frame(item='Harvesting and threshing machine',shortname='harvester',code='449',category="housing"))                      
    r=rbind(r,data.frame(item='Hand milling machine',shortname='handmill',code='450',category="housing"))                                   
    r=rbind(r,data.frame(item='Coffee pulping machine',shortname='coffeepulpingmachine',code='451',category="housing"))                     
    r=rbind(r,data.frame(item='Fertilizer distributor',shortname='fertiliserdistributor',code='452',category="housing"))                    
    
    r=rbind(r,data.frame(item='Rice (paddy)',shortname='rice_paddy',code='10101',category="food"))
    r=rbind(r,data.frame(item='Rice (husked)',shortname='rice_husked',code='10102',category="food"))
    r=rbind(r,data.frame(item='Maize (green, cob)',shortname='maize_green',code='10103',category="food"))
    r=rbind(r,data.frame(item='Maize (grain)',shortname='maize_grain',code='10104',category="food"))
    r=rbind(r,data.frame(item='Maize (flour)',shortname='maize_flour',code='10105',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (grain)',shortname='millet_grain',code='10106',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (flour)',shortname='millet_flour',code='10107',category="food"))
    r=rbind(r,data.frame(item='Wheat, barley etc.',shortname='wheat',code='10108',category="food"))
    r=rbind(r,data.frame(item='Bread',shortname='bread',code='10109',category="food"))
    r=rbind(r,data.frame(item='Buns, cakes and biscuts',shortname='bunscakes',code='10110',category="food"))
    r=rbind(r,data.frame(item='Macaroini, spaghetti',shortname='pasta',code='10111',category="food"))
    r=rbind(r,data.frame(item='Other cereal products',shortname='othercereal',code='10112',category="food"))
    r=rbind(r,data.frame(item='Cassava fresh',shortname='cassava_fresh',code='10201',category="food"))
    r=rbind(r,data.frame(item='Cassava dry/flour',shortname='cassava_flour',code='10202',category="food"))
    r=rbind(r,data.frame(item='Sweet potatoes',shortname='sweet_potato',code='10203',category="food"))
    r=rbind(r,data.frame(item='Yams/cocoyams',shortname='yam',code='10204',category="food"))
    r=rbind(r,data.frame(item='Irish potatoes',shortname='potatoes',code='10205',category="food"))
    r=rbind(r,data.frame(item='Cooking bananas, plantains',shortname='banana_green', code='10206',category="food"))
    r=rbind(r,data.frame(item='Other starches',shortname='othervegstarch',code='10207',category="food"))
    r=rbind(r,data.frame(item='Sugar',shortname='sugar',code='10301',category="food"))
    r=rbind(r,data.frame(item='Sweets',shortname='sweet',code='10302',category="food"))
    r=rbind(r,data.frame(item='Honey, jams etc.',shortname='honey',code='10303',category="food"))
    r=rbind(r,data.frame(item='Peas, beans',shortname='pulses',code='10401',category="food"))
    r=rbind(r,data.frame(item='Groundnuts in shell/shelled',shortname='peanuts',code='10501',category="food"))
    r=rbind(r,data.frame(item='Coconuts (mature/immature)',shortname='coconut',code='10502',category="food"))
    r=rbind(r,data.frame(item='Cashew, almonds, nuts',shortname='cashew_almonds', code='10503',category="food"))
    r=rbind(r,data.frame(item='Seeds from nuts',shortname='nut_products', code='10504',category="food"))
    r=rbind(r,data.frame(item='Onions, tomatoes, carrots and green pepper, other viungo',shortname='onion',code='10601',category="food"))
    r=rbind(r,data.frame(item='Spinach, cabbage and other green vegetables',shortname='greens',code='10602',category="food"))
    r=rbind(r,data.frame(item='Canned, dried and wild vegetables',shortname='dried_canned_veg',code='10603',category="food"))
    r=rbind(r,data.frame(item='Ripe bananas',shortname='banana_ripe',code='10701',category="food"))
    r=rbind(r,data.frame(item='Citrus fruits (oranges, lemon, tangarines, etc.)',shortname='citrus',code='10702',category="food"))
    r=rbind(r,data.frame(item='Mangoes, avocadoes and other fruits',shortname='mangoes',code='10703',category="food"))
    r=rbind(r,data.frame(item='Sugarcane',shortname='sugarcane',code='10704',category="food"))
    r=rbind(r,data.frame(item='Goat meat',shortname='goat',code='10801',category="food"))
    r=rbind(r,data.frame(item='Beef including minced sausage',shortname='beef', code='10802',category="food"))
    r=rbind(r,data.frame(item='Pork including sauages and bacon',shortname='pork',code='10803',category="food"))
    r=rbind(r,data.frame(item='Chicken and other poultry',shortname='chicken',code='10804',category="food"))
    r=rbind(r,data.frame(item='Wild birds and insects',shortname='wild_birds',code='10805',category="food"))
    r=rbind(r,data.frame(item='Other domestic/wild meat products',shortname='wild_meat',code='10806',category="food"))
    r=rbind(r,data.frame(item='Eggs',shortname='eggs',code='10807',category="food"))
    r=rbind(r,data.frame(item='Fresh fish and seafood (including dagaa)',shortname='fish_seafood',code='10808',category="food"))
    r=rbind(r,data.frame(item='Dried/salted/canned fish and seafood',shortname='dried_canned_fish', code='10809',category="food"))
    r=rbind(r,data.frame(item='Package fish',shortname='packaged_fish', code='10810',category="food"))
    r=rbind(r,data.frame(item='Fresh milk',shortname='fresh_milk', code='10901',category="food"))
    r=rbind(r,data.frame(item='Milk products (like cream, cheese, yoghurt etc)',shortname='milk_products',code='10902',category="food"))
    r=rbind(r,data.frame(item='Canned milk/milk powder',shortname='canned_milk', code='10903',category="food"))
    r=rbind(r,data.frame(item='Cooking oil',shortname='cooking_oil', code='11001',category="food"))
    r=rbind(r,data.frame(item='Butter, margarine, ghee and other fat products',shortname='butter_margarine', code='11002',category="food"))
    r=rbind(r,data.frame(item='Salt',shortname='salt',code='11003',category="food"))
    r=rbind(r,data.frame(item='Other spices',shortname='spices', code='11004',category="food"))
    r=rbind(r,data.frame(item='Tea dry',shortname='tea', code='11101',category="food"))
    r=rbind(r,data.frame(item='Coffee and cocoa',shortname='coffee',code='11102',category="food"))
    r=rbind(r,data.frame(item='Other raw materals for drinks',shortname='miscdrinkpowder',code='11103',category="food"))
    r=rbind(r,data.frame(item='Bottled/canned soft drinks (soda, juice, water)',shortname='canned_drink',code='11104',category="food"))
    r=rbind(r,data.frame(item='Prepared tea, coffee',shortname='readymade_tea_coffee',code='11105',category="food"))
    r=rbind(r,data.frame(item='Bottled beer',shortname='beer',code='11106',category="food"))
    r=rbind(r,data.frame(item='Local brews',shortname='brews',code='11107',category="food"))
    r=rbind(r,data.frame(item='Wine and spirits',shortname='winespirits',code='11108',category="food"))
    return(r)
  }
  
  
  items_market_price_codes_2014 <- function(){
    r=data.frame(item=NULL,code=NULL,stringsAsFactors = FALSE)
    r=rbind(r,data.frame(item='Rice (paddy)',shortname='rice_paddy',code='10101',category="food"))
    r=rbind(r,data.frame(item='Rice (husked)',shortname='rice_husked',code='10102',category="food"))
    r=rbind(r,data.frame(item='Maize (green, cob)',shortname='maize_green',code='10103',category="food"))
    r=rbind(r,data.frame(item='Maize (grain)',shortname='maize_grain',code='10104',category="food"))
    r=rbind(r,data.frame(item='Maize (flour)',shortname='maize_flour',code='10105',category="food"))
  
    r=rbind(r,data.frame(item='Millet and sorghum (grain)',shortname='millet_grain',code='11061',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (grain)',shortname='millet_grain',code='11062',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (flour)',shortname='millet_flour',code='11071',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (flour)',shortname='millet_flour',code='11072',category="food"))
    
    r=rbind(r,data.frame(item='Bread',shortname='bread',code='10109',category="food"))
    r=rbind(r,data.frame(item='Buns, cakes and biscuts',shortname='bunscakes',code='10110',category="food"))
    r=rbind(r,data.frame(item='Cassava fresh',shortname='cassava_fresh',code='10201',category="food"))
    r=rbind(r,data.frame(item='Cassava dry/flour',shortname='cassava_flour',code='12021',category="food"))
    r=rbind(r,data.frame(item='Cassava dry/flour',shortname='cassava_flour',code='12022',category="food"))
    r=rbind(r,data.frame(item='Sweet potatoes',shortname='sweet_potato',code='10203',category="food"))
    r=rbind(r,data.frame(item='Yams/cocoyams',shortname='yam',code='12041',category="food"))
    r=rbind(r,data.frame(item='Yams/cocoyams',shortname='yam',code='12042',category="food"))
    r=rbind(r,data.frame(item='Irish potatoes',shortname='potatoes',code='10205',category="food"))
    r=rbind(r,data.frame(item='Cooking bananas, plantains',shortname='banana_green', code='10206',category="food"))
    r=rbind(r,data.frame(item='Other starches',shortname='othervegstarch',code='10207',category="food"))
    r=rbind(r,data.frame(item='Sugar',shortname='sugar',code='10301',category="food"))
    r=rbind(r,data.frame(item='Sweets',shortname='sweet',code='10302',category="food"))
    r=rbind(r,data.frame(item='Honey, jams etc.',shortname='honey',code='10303',category="food"))
    r=rbind(r,data.frame(item='Peas, beans',shortname='pulses',code='14011',category="food"))
    r=rbind(r,data.frame(item='Peas, beans',shortname='pulses',code='14012',category="food"))
    r=rbind(r,data.frame(item='Groundnuts in shell/shelled',shortname='peanuts',code='10501',category="food"))
    r=rbind(r,data.frame(item='Coconuts (mature/immature)',shortname='coconut',code='10502',category="food"))
    r=rbind(r,data.frame(item='Cashew, almonds, nuts',shortname='cashew_almonds', code='10503',category="food"))
    r=rbind(r,data.frame(item='Seeds from nuts',shortname='nut_products', code='10504',category="food"))
    r=rbind(r,data.frame(item='Onions, tomatoes, carrots and green pepper, other viungo',shortname='onion',code='16011',category="food"))
    r=rbind(r,data.frame(item='Onions, tomatoes, carrots and green pepper, other viungo',shortname='onion',code='16012',category="food"))
    r=rbind(r,data.frame(item='Onions, tomatoes, carrots and green pepper, other viungo',shortname='onion',code='16013',category="food"))
    
    r=rbind(r,data.frame(item='Spinach, cabbage and other green vegetables',shortname='greens',code='16021',category="food"))
    r=rbind(r,data.frame(item='Spinach, cabbage and other green vegetables',shortname='greens',code='16022',category="food"))

    r=rbind(r,data.frame(item='Canned, dried and wild vegetables',shortname='dried_canned_veg',code='10603',category="food"))
    r=rbind(r,data.frame(item='Ripe bananas',shortname='banana_ripe',code='10701',category="food"))
    r=rbind(r,data.frame(item='Citrus fruits (oranges, lemon, tangarines, etc.)',shortname='citrus',code='10702',category="food"))
    r=rbind(r,data.frame(item='Mangoes, avocadoes and other fruits',shortname='mangoes',code='17031',category="food"))
    r=rbind(r,data.frame(item='Mangoes, avocadoes and other fruits',shortname='mangoes',code='17032',category="food"))
    
    r=rbind(r,data.frame(item='Sugarcane',shortname='sugarcane',code='10704',category="food"))
    r=rbind(r,data.frame(item='Goat meat',shortname='goat',code='10801',category="food"))
    r=rbind(r,data.frame(item='Beef including minced sausage',shortname='beef', code='10802',category="food"))
    r=rbind(r,data.frame(item='Pork including sauages and bacon',shortname='pork',code='10803',category="food"))
    r=rbind(r,data.frame(item='Chicken and other poultry',shortname='chicken',code='10804',category="food"))
    r=rbind(r,data.frame(item='Wild birds and insects',shortname='wild_birds',code='10805',category="food"))
    r=rbind(r,data.frame(item='Other domestic/wild meat products',shortname='wild_meat',code='10806',category="food"))
    r=rbind(r,data.frame(item='Eggs',shortname='eggs',code='10807',category="food"))
    r=rbind(r,data.frame(item='Fresh fish and seafood (including dagaa)',shortname='fish_seafood',code='18081',category="food"))
    r=rbind(r,data.frame(item='Fresh fish and seafood (including dagaa)',shortname='fish_seafood',code='18082',category="food"))
    
    r=rbind(r,data.frame(item='Dried/salted/canned fish and seafood',shortname='dried_canned_fish', code='10809',category="food"))
    r=rbind(r,data.frame(item='Package fish',shortname='packaged_fish', code='10810',category="food"))
    r=rbind(r,data.frame(item='Fresh milk',shortname='fresh_milk', code='10901',category="food"))
    r=rbind(r,data.frame(item='Milk products (like cream, cheese, yoghurt etc)',shortname='milk_products',code='10902',category="food"))
    r=rbind(r,data.frame(item='Canned milk/milk powder',shortname='canned_milk', code='10903',category="food"))
    r=rbind(r,data.frame(item='Cooking oil',shortname='cooking_oil', code='11001',category="food"))
    r=rbind(r,data.frame(item='Butter, margarine, ghee and other fat products',shortname='butter_margarine', code='11002',category="food"))
    r=rbind(r,data.frame(item='Salt',shortname='salt',code='11003',category="food"))
    r=rbind(r,data.frame(item='Other spices',shortname='spices', code='11004',category="food"))
    r=rbind(r,data.frame(item='Tea dry',shortname='tea', code='11101',category="food"))
    r=rbind(r,data.frame(item='Coffee and cocoa',shortname='coffee',code='11102',category="food"))
    r=rbind(r,data.frame(item='Other raw materals for drinks',shortname='miscdrinkpowder',code='11103',category="food"))
    r=rbind(r,data.frame(item='Bottled/canned soft drinks (soda, juice, water)',shortname='canned_drink',code='11104',category="food"))
    r=rbind(r,data.frame(item='Prepared tea, coffee',shortname='readymade_tea_coffee',code='11105',category="food"))
    r=rbind(r,data.frame(item='Bottled beer',shortname='beer',code='11106',category="food"))
    r=rbind(r,data.frame(item='Local brews',shortname='brews',code='11107',category="food"))
    r=rbind(r,data.frame(item='Wine and spirits',shortname='winespirits',code='11108',category="food"))
    
    r=rbind(r,data.frame(item='Cigarettes',shortname='cigarettes',code='12101', category = "misc"))
    r=rbind(r,data.frame(item='Matches',shortname='matches',code='12102', category = "energy"))
    
    r=rbind(r,data.frame(item='Kerosene',shortname='kerosene',code='12201',category="energy"))
    r=rbind(r,data.frame(item='Charcoal',shortname='charcoal',code='12207',category="energy"))
    r=rbind(r,data.frame(item='Milling fees grain',shortname='milling',code='12208',category="food"))
    
    r=rbind(r,data.frame(item='Firewood',shortname='firewood',code='19001',category="housing"))                    
    r=rbind(r,data.frame(item='Batteries',shortname='batteries',code='19002',category="housing"))                    
    
    return(r)
  }
  
  items_codes_2012<-function() { 
    r=data.frame(item=NULL,code=NULL,stringsAsFactors = FALSE)
    r=rbind(r,data.frame(item='Cigarettes',shortname='cigarettes',code='101', category = "misc"))
    r=rbind(r,data.frame(item='Matches',shortname='matches',code='102', category = "energy"))
    r=rbind(r,data.frame(item='Public Transport',shortname='public_transport',code='103',category="transport"))
    
    r=rbind(r,data.frame(item='Kerosene',shortname='kerosene',code='201',category="energy"))
    r=rbind(r,data.frame(item='Electricity',shortname='electricity',code='202',category="energy"))
    r=rbind(r,data.frame(item='Gas',shortname='gas', code='203',category="energy"))
    r=rbind(r,data.frame(item='Water',shortname='water',code='204',category="food"))
    r=rbind(r,data.frame(item='Petrol',shortname='petrol',code='205',category="energy"))
    r=rbind(r,data.frame(item='Cellphone voucher',shortname='cellphone_voucher', code='206',category="personal_products"))
    r=rbind(r,data.frame(item='Charcoal',shortname='charcoal',code='207',category="energy"))
    r=rbind(r,data.frame(item='Milling fees grain',shortname='milling',code='208',category="food"))
    r=rbind(r,data.frame(item='Bar soap',shortname='bar_soap',code='209',category="personal_products"))
    r=rbind(r,data.frame(item='Clothes soap',shortname='clothes_soap',code='210',category="personal_products"))
    r=rbind(r,data.frame(item='Toothpaste',shortname='toothpaste',code='211',category="personal_products"))
    r=rbind(r,data.frame(item='Toilet Paper',shortname='toilet_paper',code='212',category="personal_products"))
    r=rbind(r,data.frame(item='Skin Cream',shortname='skin_cream',code='213',category="personal_products"))
    r=rbind(r,data.frame(item='Others Personal Products',shortname='other_personal',code='214',category="personal_products"))
    r=rbind(r,data.frame(item='Household cleaning Products',shortname='misc_cleaning',code='215',category="personal_products"))
    r=rbind(r,data.frame(item='Light bulbs',shortname='light_bulbs',code='216',category="housing"))
    r=rbind(r,data.frame(item='Phone, internet fees',shortname='phone',code='217',category="personal_products"))
    r=rbind(r,data.frame(item='Donations',shortname='donation',code='218',category="misc"))
    r=rbind(r,data.frame(item='Motor vehicle repairs',shortname='motor_repair',code='219',category="transport"))
    r=rbind(r,data.frame(item='Bicycle service',shortname='bicycle_repair',code='220',category="transport"))
    r=rbind(r,data.frame(item='Servant Wages',shortname='services',code='221',category="social_functions"))
    r=rbind(r,data.frame(item='Mortgage payment',shortname='mortgage',code='222',category="ignored"))
    r=rbind(r,data.frame(item='House repair',shortname='house_repair',code='223',category="ignored"))
    ####
    r=rbind(r,data.frame(item='CarpetsRugs',shortname='carpet',code='301',category="housing"))
    r=rbind(r,data.frame(item='Linen',shortname='linen',code='302',category="housing"))
    r=rbind(r,data.frame(item='Mats',shortname='mat',code='303',category="housing"))
    r=rbind(r,data.frame(item='MosquitoNet',shortname='mosquito_net',code='304',category="housing"))
    r=rbind(r,data.frame(item='Mattress',shortname='mattress',code='305',category="housing"))
    r=rbind(r,data.frame(item='SportsHobbyEquipment',shortname='sports_hobby',code='306',category="personal_products"))
    r=rbind(r,data.frame(item='FilmCamera',shortname='camera',code='307',category="personal_products"))
    r=rbind(r,data.frame(item='BuidingMaterial',shortname='building_material',code='308',category="housing"))
    r=rbind(r,data.frame(item='CouncilRates',shortname='council_rates',code='309',category="ignored"))
    r=rbind(r,data.frame(item='Insurance',shortname='insurance',code='310',category="housing"))
    r=rbind(r,data.frame(item='TheftLosses',shortname='theft_losses',code='311',category="misc"))
    r=rbind(r,data.frame(item='LegalFeesFines',shortname='legalfines',code='312',category="misc"))
    r=rbind(r,data.frame(item='BrideMarriageCeremonyCosts',shortname='marriage',code='313',category="social_functions"))
    r=rbind(r,data.frame(item='FuneralCosts',shortname='funeral',code='314',category="social_functions"))
    r=rbind(r,data.frame(item='OherCosts',shortname='other_costs',code='315',category="social_functions"))
    
    r=rbind(r,data.frame(item='ConsumerDurableRepairs',shortname='consumer_durables_repair',code='316',category="personal_products"))
    r=rbind(r,data.frame(item='PropertyIncomeTax',shortname='taxes',code='317',category="misc"))
    
    r=rbind(r,data.frame(item='Dwelling Repairs-maintenance',shortname='houserepair',code='318',category="housing"))
    
    r=rbind(r,data.frame(item='mensclothes',shortname='mensclothes',code='319',category="personal_products"))
    r=rbind(r,data.frame(item='womensclothes',shortname='womensclothes',code='320',category="personal_products"))
    r=rbind(r,data.frame(item='childrensclothes',shortname='childrensclothes',code='321',category="personal_products"))
    r=rbind(r,data.frame(item='mensshoes',shortname='mensshoes',code='322',category="personal_products"))
    r=rbind(r,data.frame(item='womensshoes',shortname='womensshoes',code='323',category="personal_products"))
    r=rbind(r,data.frame(item='childrensshoes',shortname='childrensshoes',code='324',category="personal_products"))
    
    r=rbind(r,data.frame(item='Woodpoles',shortname='bamboo',code='325',category="personal_products"))
    r=rbind(r,data.frame(item='ThatchingGrass',shortname='grass',code='326',category="housing"))
    
    
    r=rbind(r,data.frame(item='Radio and Radio Cassette',shortname='radio',code='401',category="personal_products"))                                  
    r=rbind(r,data.frame(item='Telephone(landline)',shortname='landline',code='402',category="personal_products"))                                    
    r=rbind(r,data.frame(item='Telephone(mobile)',shortname='mobile',code='403',category="personal_products"))                                        
    r=rbind(r,data.frame(item='Refridgerator or freezer',shortname='refrigerator',code='404',category="housing"))                           
    r=rbind(r,data.frame(item='Sewing Machine',shortname='sewingmachine',code='405',category="housing"))                                    
    r=rbind(r,data.frame(item='Television',shortname='tv',code='406',category="housing"))                                                   
    r=rbind(r,data.frame(item='Video / DVD',shortname='videoplayer',code='407',category="personal_products"))                                         
    r=rbind(r,data.frame(item='Chairs',shortname='chair',code='408',category="housing"))                                                    
    r=rbind(r,data.frame(item='Sofas',shortname='sofa',code='409',category="housing"))                                                      
    r=rbind(r,data.frame(item='Tables',shortname='table',code='410',category="housing"))                                                    
    r=rbind(r,data.frame(item='Watches',shortname='watch',code='411',category="personal_products"))                                                   
    r=rbind(r,data.frame(item='Beds',shortname='bed',code='412',category="housing"))                                                        
    r=rbind(r,data.frame(item='Cupboards, chest-of-drawers, boxes, wardrobes,bookcases',shortname='cupboard',code='413',category="housing"))
    r=rbind(r,data.frame(item='Lanterns',shortname='lantern',code='414',category="housing"))                                                
    r=rbind(r,data.frame(item='Computer',shortname='computer',code='415',category="personal_products"))                                               
    r=rbind(r,data.frame(item='Cooking pots, Cups, other kitchen utencils',shortname='cookingpot',code='416',category="food"))           
    r=rbind(r,data.frame(item='Mosquito net',shortname='mosquitonet',code='417',category="housing"))                                        
    r=rbind(r,data.frame(item='Iron (Charcoal or electric)',shortname='iron',code='418',category="personal_products"))                                
    r=rbind(r,data.frame(item='Electric/gas stove',shortname='stove_electricgas',code='419',category="housing"))
    r=rbind(r,data.frame(item='Other stove',shortname='stove_other',code='420',category="housing"))
    r=rbind(r,data.frame(item='Water-heater',shortname='waterheater',code='421',category="personal_products"))
    r=rbind(r,data.frame(item='Record/cassette player, tape recorder',shortname='musicplayer',code='422',category="personal_products"))               
    r=rbind(r,data.frame(item='Complete music system',shortname='musicsystem',code='423',category="personal_products"))                               
    r=rbind(r,data.frame(item='Books (not school books)',shortname='bookexschool',code='424',category="personal_products"))                           
    r=rbind(r,data.frame(item='Motor Vehicles',shortname='car',code='425',category="transport"))                                              
    r=rbind(r,data.frame(item='Motor cycle',shortname='motorbike',code='426',category="transport"))                                           
    r=rbind(r,data.frame(item='Bicycle',shortname='bike',code='427',category="transport"))                                                    
    r=rbind(r,data.frame(item='Carts',shortname='cart',code='428',category="transport"))                                                      
    r=rbind(r,data.frame(item='Animal Cart',shortname='animalcart',code='429',category="transport"))                                          
    r=rbind(r,data.frame(item='Boat/canoe',shortname='boat',code='430',category="transport"))                                               
    r=rbind(r,data.frame(item='Wheel barrow',shortname='wheelbarrow',code='431',category="housing"))                                        
    r=rbind(r,data.frame(item='Livestock',shortname='livestock',code='432',category="housing"))                                             
    r=rbind(r,data.frame(item='Poultry',shortname='poultry',code='433',category="housing"))                                                 
    r=rbind(r,data.frame(item='Outboard engine',shortname='engine_outboard',code='434',category="housing"))                                 
    r=rbind(r,data.frame(item='Donkeys',shortname='donkey',code='435',category="housing"))                                                  
    r=rbind(r,data.frame(item='Fields/Land',shortname='land',code='436',category="housing"))                                                
    r=rbind(r,data.frame(item='House(s)',shortname='house',code='437',category="housing"))                                                  
    r=rbind(r,data.frame(item='Airconditioner/Fans',shortname='ac_fan',code='438',category="housing"))                                      
    r=rbind(r,data.frame(item='Dish antena/decoder',shortname='dishtv',code='439',category="housing"))                                     
    r=rbind(r,data.frame(item='Hoes',shortname='hoe',code='440',category="housing"))                                                        
    r=rbind(r,data.frame(item='Spraying machine',shortname='spraymachine',code='441',category="housing"))                                   
    r=rbind(r,data.frame(item='Water pumping set',shortname='waterpump',code='442',category="housing"))                                     
    r=rbind(r,data.frame(item='Reapers',shortname='reaper',code='443',category="housing"))                                                  
    r=rbind(r,data.frame(item='Tractor',shortname='tractor',code='444',category="housing"))                                                 
    r=rbind(r,data.frame(item='Trailer for tractors etc.',shortname='trailer',code='445',category="housing"))                               
    r=rbind(r,data.frame(item='Plough etc.',shortname='plough',code='446',category="housing"))                                              
    r=rbind(r,data.frame(item='Harrow',shortname='harrow',code='447',category="housing"))                                                   
    r=rbind(r,data.frame(item='Milking machine',shortname='milkingmachine',code='448',category="housing"))                                  
    r=rbind(r,data.frame(item='Harvesting and threshing machine',shortname='harvester',code='449',category="housing"))                      
    r=rbind(r,data.frame(item='Hand milling machine',shortname='handmill',code='450',category="housing"))                                   
    r=rbind(r,data.frame(item='Coffee pulping machine',shortname='coffeepulpingmachine',code='451',category="housing"))                     
    r=rbind(r,data.frame(item='Fertilizer distributor',shortname='fertiliserdistributor',code='452',category="housing"))                    
    r=rbind(r,data.frame(item='Power tiller',shortname='powertiller',code='453',category="housing"))                    
    
    r=rbind(r,data.frame(item='Rice (paddy)',shortname='rice_paddy',code='10101',category="food"))
    r=rbind(r,data.frame(item='Rice (husked)',shortname='rice_husked',code='10102',category="food"))
    r=rbind(r,data.frame(item='Maize (green, cob)',shortname='maize_green',code='10103',category="food"))
    r=rbind(r,data.frame(item='Maize (grain)',shortname='maize_grain',code='10104',category="food"))
    r=rbind(r,data.frame(item='Maize (flour)',shortname='maize_flour',code='10105',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (grain)',shortname='millet_grain',code='10106',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (flour)',shortname='millet_flour',code='10107',category="food"))
    r=rbind(r,data.frame(item='Wheat, barley etc.',shortname='wheat',code='10108',category="food"))
    r=rbind(r,data.frame(item='Bread',shortname='bread',code='10109',category="food"))
    r=rbind(r,data.frame(item='Buns, cakes and biscuts',shortname='bunscakes',code='10110',category="food"))
    r=rbind(r,data.frame(item='Macaroini, spaghetti',shortname='pasta',code='10111',category="food"))
    r=rbind(r,data.frame(item='Other cereal products',shortname='othercereal',code='10112',category="food"))
    r=rbind(r,data.frame(item='Cassava fresh',shortname='cassava_fresh',code='10201',category="food"))
    r=rbind(r,data.frame(item='Cassava dry/flour',shortname='cassava_flour',code='10202',category="food"))
    r=rbind(r,data.frame(item='Sweet potatoes',shortname='sweet_potato',code='10203',category="food"))
    r=rbind(r,data.frame(item='Yams/cocoyams',shortname='yam',code='10204',category="food"))
    r=rbind(r,data.frame(item='Irish potatoes',shortname='potatoes',code='10205',category="food"))
    r=rbind(r,data.frame(item='Cooking bananas, plantains',shortname='banana_green', code='10206',category="food"))
    r=rbind(r,data.frame(item='Other starches',shortname='othervegstarch',code='10207',category="food"))
    r=rbind(r,data.frame(item='Sugar',shortname='sugar',code='10301',category="food"))
    r=rbind(r,data.frame(item='Sweets',shortname='sweet',code='10302',category="food"))
    r=rbind(r,data.frame(item='Honey, jams etc.',shortname='honey',code='10303',category="food"))
    r=rbind(r,data.frame(item='Peas, beans',shortname='pulses',code='10401',category="food"))
    r=rbind(r,data.frame(item='Groundnuts in shell/shelled',shortname='peanuts',code='10501',category="food"))
    r=rbind(r,data.frame(item='Coconuts (mature/immature)',shortname='coconut',code='10502',category="food"))
    r=rbind(r,data.frame(item='Cashew, almonds, nuts',shortname='cashew_almonds', code='10503',category="food"))
    r=rbind(r,data.frame(item='Seeds from nuts',shortname='nut_products', code='10504',category="food"))
    r=rbind(r,data.frame(item='Onions, tomatoes, carrots and green pepper, other viungo',shortname='onion',code='10601',category="food"))
    r=rbind(r,data.frame(item='Spinach, cabbage and other green vegetables',shortname='greens',code='10602',category="food"))
    r=rbind(r,data.frame(item='Canned, dried and wild vegetables',shortname='dried_canned_veg',code='10603',category="food"))
    r=rbind(r,data.frame(item='Ripe bananas',shortname='banana_ripe',code='10701',category="food"))
    r=rbind(r,data.frame(item='Citrus fruits (oranges, lemon, tangarines, etc.)',shortname='citrus',code='10702',category="food"))
    r=rbind(r,data.frame(item='Mangoes, avocadoes and other fruits',shortname='mangoes',code='10703',category="food"))
    r=rbind(r,data.frame(item='Sugarcane',shortname='sugarcane',code='10704',category="food"))
    r=rbind(r,data.frame(item='Goat meat',shortname='goat',code='10801',category="food"))
    r=rbind(r,data.frame(item='Beef including minced sausage',shortname='beef', code='10802',category="food"))
    r=rbind(r,data.frame(item='Pork including sauages and bacon',shortname='pork',code='10803',category="food"))
    r=rbind(r,data.frame(item='Chicken and other poultry',shortname='chicken',code='10804',category="food"))
    r=rbind(r,data.frame(item='Wild birds and insects',shortname='wild_birds',code='10805',category="food"))
    r=rbind(r,data.frame(item='Other domestic/wild meat products',shortname='wild_meat',code='10806',category="food"))
    r=rbind(r,data.frame(item='Eggs',shortname='eggs',code='10807',category="food"))
    r=rbind(r,data.frame(item='Fresh fish and seafood (including dagaa)',shortname='fish_seafood',code='10808',category="food"))
    r=rbind(r,data.frame(item='Dried/salted/canned fish and seafood',shortname='dried_canned_fish', code='10809',category="food"))
    r=rbind(r,data.frame(item='Package fish',shortname='packaged_fish', code='10810',category="food"))
    r=rbind(r,data.frame(item='Fresh milk',shortname='fresh_milk', code='10901',category="food"))
    r=rbind(r,data.frame(item='Milk products (like cream, cheese, yoghurt etc)',shortname='milk_products',code='10902',category="food"))
    r=rbind(r,data.frame(item='Canned milk/milk powder',shortname='canned_milk', code='10903',category="food"))
    r=rbind(r,data.frame(item='Cooking oil',shortname='cooking_oil', code='11001',category="food"))
    r=rbind(r,data.frame(item='Butter, margarine, ghee and other fat products',shortname='butter_margarine', code='11002',category="food"))
    r=rbind(r,data.frame(item='Salt',shortname='salt',code='11003',category="food"))
    r=rbind(r,data.frame(item='Other spices',shortname='spices', code='11004',category="food"))
    r=rbind(r,data.frame(item='Tea dry',shortname='tea', code='11101',category="food"))
    r=rbind(r,data.frame(item='Coffee and cocoa',shortname='coffee',code='11102',category="food"))
    r=rbind(r,data.frame(item='Other raw materals for drinks',shortname='miscdrinkpowder',code='11103',category="food"))
    r=rbind(r,data.frame(item='Bottled/canned soft drinks (soda, juice, water)',shortname='canned_drink',code='11104',category="food"))
    r=rbind(r,data.frame(item='Prepared tea, coffee',shortname='readymade_tea_coffee',code='11105',category="food"))
    r=rbind(r,data.frame(item='Bottled beer',shortname='beer',code='11106',category="food"))
    r=rbind(r,data.frame(item='Local brews',shortname='brews',code='11107',category="food"))
    r=rbind(r,data.frame(item='Wine and spirits',shortname='winespirits',code='11108',category="food"))
    return(r)
  }
  
  
  
  
  items_codes_2014<-function() { 
    r=data.frame(item=NULL,code=NULL,stringsAsFactors = FALSE)
    r=rbind(r,data.frame(item='Cigarettes',shortname='cigarettes',code='101', category = "misc"))
    r=rbind(r,data.frame(item='Matches',shortname='matches',code='102', category = "energy"))
    r=rbind(r,data.frame(item='Public Transport',shortname='public_transport',code='103',category="transport"))
    
    r=rbind(r,data.frame(item='Kerosene',shortname='kerosene',code='201',category="energy"))
    r=rbind(r,data.frame(item='Electricity',shortname='electricity',code='202',category="energy"))
    r=rbind(r,data.frame(item='Gas',shortname='gas', code='203',category="energy"))
    r=rbind(r,data.frame(item='Water',shortname='water',code='204',category="food"))
    r=rbind(r,data.frame(item='Petrol',shortname='petrol',code='205',category="energy"))
    r=rbind(r,data.frame(item='Cellphone voucher',shortname='cellphone_voucher', code='206',category="personal_products"))
    r=rbind(r,data.frame(item='Charcoal',shortname='charcoal',code='207',category="energy"))
    r=rbind(r,data.frame(item='Milling fees grain',shortname='milling',code='208',category="food"))
    r=rbind(r,data.frame(item='Bar soap',shortname='bar_soap',code='209',category="personal_products"))
    r=rbind(r,data.frame(item='Clothes soap',shortname='clothes_soap',code='210',category="personal_products"))
    
    r=rbind(r,data.frame(item='Toothpaste',shortname='toothpaste',code='211',category="personal_products"))
    r=rbind(r,data.frame(item='Toilet Paper',shortname='toilet_paper',code='212',category="personal_products"))
    r=rbind(r,data.frame(item='Skin Cream',shortname='skin_cream',code='213',category="personal_products"))
    r=rbind(r,data.frame(item='Others Personal Products',shortname='other_personal',code='214',category="personal_products"))
    r=rbind(r,data.frame(item='Household cleaning Products',shortname='misc_cleaning',code='215',category="personal_products"))
    r=rbind(r,data.frame(item='Light bulbs',shortname='light_bulbs',code='216',category="housing"))
    r=rbind(r,data.frame(item='Phone, internet fees',shortname='phone',code='217',category="personal_products"))
    r=rbind(r,data.frame(item='Donations',shortname='donation',code='218',category="misc"))
    r=rbind(r,data.frame(item='Motor vehicle repairs',shortname='motor_repair',code='219',category="transport"))
    r=rbind(r,data.frame(item='Bicycle service',shortname='bicycle_repair',code='220',category="transport"))
    
    r=rbind(r,data.frame(item='Servant Wages',shortname='services',code='221',category="social_functions"))
    r=rbind(r,data.frame(item='Mortgage payment',shortname='mortgage',code='222',category="ignored"))
    r=rbind(r,data.frame(item='House repair',shortname='house_repair',code='223',category="ignored"))
    ####
    r=rbind(r,data.frame(item='CarpetsRugs',shortname='carpet',code='301',category="housing"))
    r=rbind(r,data.frame(item='Linen',shortname='linen',code='302',category="housing"))
    r=rbind(r,data.frame(item='Mats',shortname='mat',code='303',category="housing"))
    r=rbind(r,data.frame(item='MosquitoNet',shortname='mosquito_net',code='304',category="housing"))
    r=rbind(r,data.frame(item='Mattress',shortname='mattress',code='305',category="housing"))
    r=rbind(r,data.frame(item='SportsHobbyEquipment',shortname='sports_hobby',code='306',category="personal_products"))
    r=rbind(r,data.frame(item='FilmCamera',shortname='camera',code='307',category="personal_products"))
    r=rbind(r,data.frame(item='BuidingMaterial',shortname='building_material',code='308',category="housing"))
    r=rbind(r,data.frame(item='CouncilRates',shortname='council_rates',code='309',category="ignored"))
    r=rbind(r,data.frame(item='Insurance',shortname='insurance',code='310',category="housing"))
    
    r=rbind(r,data.frame(item='TheftLosses',shortname='theft_losses',code='311',category="misc"))
    r=rbind(r,data.frame(item='LegalFeesFines',shortname='legalfines',code='312',category="misc"))
    r=rbind(r,data.frame(item='BrideMarriageCeremonyCosts',shortname='marriage',code='313',category="social_functions"))
    r=rbind(r,data.frame(item='FuneralCosts',shortname='funeral',code='314',category="social_functions"))
    r=rbind(r,data.frame(item='OherCosts',shortname='other_costs',code='315',category="social_functions"))
    
    r=rbind(r,data.frame(item='ConsumerDurableRepairs',shortname='consumer_durables_repair',code='316',category="personal_products"))
    r=rbind(r,data.frame(item='PropertyIncomeTax',shortname='taxes',code='317',category="misc"))
    
    r=rbind(r,data.frame(item='Dwelling Repairs-maintenance',shortname='houserepair',code='318',category="housing"))
    
    r=rbind(r,data.frame(item='mensclothes',shortname='mensclothes',code='319',category="personal_products"))
    r=rbind(r,data.frame(item='womensclothes',shortname='womensclothes',code='320',category="personal_products"))
    r=rbind(r,data.frame(item='childrensclothes',shortname='childrensclothes',code='321',category="personal_products"))
    r=rbind(r,data.frame(item='mensshoes',shortname='mensshoes',code='322',category="personal_products"))
    r=rbind(r,data.frame(item='womensshoes',shortname='womensshoes',code='323',category="personal_products"))
    r=rbind(r,data.frame(item='childrensshoes',shortname='childrensshoes',code='324',category="personal_products"))
    
    r=rbind(r,data.frame(item='Woodpoles',shortname='bamboo',code='325',category="personal_products"))
    r=rbind(r,data.frame(item='ThatchingGrass',shortname='grass',code='326',category="housing"))
    
    
    r=rbind(r,data.frame(item='Radio and Radio Cassette',shortname='radio',code='401',category="personal_products"))                                  
    r=rbind(r,data.frame(item='Telephone(landline)',shortname='landline',code='402',category="personal_products"))                                    
    r=rbind(r,data.frame(item='Telephone(mobile)',shortname='mobile',code='403',category="personal_products"))                                        
    r=rbind(r,data.frame(item='Refridgerator or freezer',shortname='refrigerator',code='404',category="housing"))                           
    r=rbind(r,data.frame(item='Sewing Machine',shortname='sewingmachine',code='405',category="housing"))                                    
    r=rbind(r,data.frame(item='Television',shortname='tv',code='406',category="housing"))                                                   
    r=rbind(r,data.frame(item='Video / DVD',shortname='videoplayer',code='407',category="personal_products"))                                         
    r=rbind(r,data.frame(item='Chairs',shortname='chair',code='408',category="housing"))                                                    
    r=rbind(r,data.frame(item='Sofas',shortname='sofa',code='409',category="housing"))                                                      
    r=rbind(r,data.frame(item='Tables',shortname='table',code='410',category="housing"))                                                    
    r=rbind(r,data.frame(item='Watches',shortname='watch',code='411',category="personal_products"))                                                   
    r=rbind(r,data.frame(item='Beds',shortname='bed',code='412',category="housing"))                                                        
    r=rbind(r,data.frame(item='Cupboards, chest-of-drawers, boxes, wardrobes,bookcases',shortname='cupboard',code='413',category="housing"))
    r=rbind(r,data.frame(item='Lanterns',shortname='lantern',code='414',category="housing"))                                                
    r=rbind(r,data.frame(item='Computer',shortname='computer',code='415',category="personal_products"))                                               
    r=rbind(r,data.frame(item='Cooking pots, Cups, other kitchen utencils',shortname='cookingpot',code='416',category="food"))           
    r=rbind(r,data.frame(item='Mosquito net',shortname='mosquitonet',code='417',category="housing"))                                        
    r=rbind(r,data.frame(item='Iron (Charcoal or electric)',shortname='iron',code='418',category="personal_products"))                                
    r=rbind(r,data.frame(item='Electric/gas stove',shortname='stove_electricgas',code='419',category="housing"))
    r=rbind(r,data.frame(item='Other stove',shortname='stove_other',code='420',category="housing"))
    r=rbind(r,data.frame(item='Water-heater',shortname='waterheater',code='421',category="personal_products"))
    r=rbind(r,data.frame(item='Record/cassette player, tape recorder',shortname='musicplayer',code='422',category="personal_products"))               
    r=rbind(r,data.frame(item='Complete music system',shortname='musicsystem',code='423',category="personal_products"))                               
    r=rbind(r,data.frame(item='Books (not school books)',shortname='bookexschool',code='424',category="personal_products"))                           
    r=rbind(r,data.frame(item='Motor Vehicles',shortname='car',code='425',category="transport"))                                              
    r=rbind(r,data.frame(item='Motor cycle',shortname='motorbike',code='426',category="transport"))                                           
    r=rbind(r,data.frame(item='Bicycle',shortname='bike',code='427',category="transport"))                                                    
    r=rbind(r,data.frame(item='Carts',shortname='cart',code='428',category="transport"))                                                      
    r=rbind(r,data.frame(item='Animal Cart',shortname='animalcart',code='429',category="transport"))                                          
    r=rbind(r,data.frame(item='Boat/canoe',shortname='boat',code='430',category="transport"))                                               
    r=rbind(r,data.frame(item='Wheel barrow',shortname='wheelbarrow',code='431',category="housing"))                                        
    r=rbind(r,data.frame(item='Livestock',shortname='livestock',code='432',category="housing"))                                             
    r=rbind(r,data.frame(item='Poultry',shortname='poultry',code='433',category="housing"))                                                 
    r=rbind(r,data.frame(item='Outboard engine',shortname='engine_outboard',code='434',category="housing"))                                 
    r=rbind(r,data.frame(item='Donkeys',shortname='donkey',code='435',category="housing"))                                                  
    r=rbind(r,data.frame(item='Fields/Land',shortname='land',code='436',category="housing"))                                                
    r=rbind(r,data.frame(item='House(s)',shortname='house',code='437',category="housing"))                                                  
    r=rbind(r,data.frame(item='Airconditioner/Fans',shortname='ac_fan',code='438',category="housing"))                                      
    r=rbind(r,data.frame(item='Dish antena/decoder',shortname='dishtv',code='439',category="housing"))                                     
    r=rbind(r,data.frame(item='Hoes',shortname='hoe',code='440',category="housing"))                                                        
    r=rbind(r,data.frame(item='Spraying machine',shortname='spraymachine',code='441',category="housing"))                                   
    r=rbind(r,data.frame(item='Water pumping set',shortname='waterpump',code='442',category="housing"))                                     
    r=rbind(r,data.frame(item='Reapers',shortname='reaper',code='443',category="housing"))                                                  
    r=rbind(r,data.frame(item='Tractor',shortname='tractor',code='444',category="housing"))                                                 
    r=rbind(r,data.frame(item='Trailer for tractors etc.',shortname='trailer',code='445',category="housing"))                               
    r=rbind(r,data.frame(item='Plough etc.',shortname='plough',code='446',category="housing"))                                              
    r=rbind(r,data.frame(item='Harrow',shortname='harrow',code='447',category="housing"))                                                   
    r=rbind(r,data.frame(item='Milking machine',shortname='milkingmachine',code='448',category="housing"))                                  
    r=rbind(r,data.frame(item='Harvesting and threshing machine',shortname='harvester',code='449',category="housing"))                      
    r=rbind(r,data.frame(item='Hand milling machine',shortname='handmill',code='450',category="housing"))                                   
    r=rbind(r,data.frame(item='Coffee pulping machine',shortname='coffeepulpingmachine',code='451',category="housing"))                     
    r=rbind(r,data.frame(item='Fertilizer distributor',shortname='fertiliserdistributor',code='452',category="housing"))                    
    r=rbind(r,data.frame(item='Power tiller',shortname='powertiller',code='453',category="housing"))                    
    
    r=rbind(r,data.frame(item='Rice (paddy)',shortname='rice_paddy',code='10101',category="food"))
    r=rbind(r,data.frame(item='Rice (husked)',shortname='rice_husked',code='10102',category="food"))
    r=rbind(r,data.frame(item='Maize (green, cob)',shortname='maize_green',code='10103',category="food"))
    r=rbind(r,data.frame(item='Maize (grain)',shortname='maize_grain',code='10104',category="food"))
    r=rbind(r,data.frame(item='Maize (flour)',shortname='maize_flour',code='10105',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (grain)',shortname='millet_grain',code='10106',category="food"))
    r=rbind(r,data.frame(item='Millet and sorghum (flour)',shortname='millet_flour',code='10107',category="food"))
    r=rbind(r,data.frame(item='Wheat, barley etc.',shortname='wheat',code='11081',category="food"))
    r=rbind(r,data.frame(item='Wheat, barley etc.',shortname='wheat',code='11082',category="food"))
    r=rbind(r,data.frame(item='Bread',shortname='bread',code='10109',category="food"))
    r=rbind(r,data.frame(item='Buns, cakes and biscuts',shortname='bunscakes',code='10110',category="food"))
    r=rbind(r,data.frame(item='Macaroini, spaghetti',shortname='pasta',code='10111',category="food"))
    r=rbind(r,data.frame(item='Other cereal products',shortname='othercereal',code='10112',category="food"))
    
    r=rbind(r,data.frame(item='Cassava fresh',shortname='cassava_fresh',code='10201',category="food"))
    r=rbind(r,data.frame(item='Cassava dry/flour',shortname='cassava_flour',code='10202',category="food"))
    r=rbind(r,data.frame(item='Sweet potatoes',shortname='sweet_potato',code='10203',category="food"))
    r=rbind(r,data.frame(item='Yams/cocoyams',shortname='yam',code='10204',category="food"))
    r=rbind(r,data.frame(item='Irish potatoes',shortname='potatoes',code='10205',category="food"))
    r=rbind(r,data.frame(item='Cooking bananas, plantains',shortname='banana_green', code='10206',category="food"))
    r=rbind(r,data.frame(item='Other starches',shortname='othervegstarch',code='10207',category="food"))
    
    r=rbind(r,data.frame(item='Sugar',shortname='sugar',code='10301',category="food"))
    r=rbind(r,data.frame(item='Sweets',shortname='sweet',code='10302',category="food"))
    r=rbind(r,data.frame(item='Honey, jams etc.',shortname='honey',code='10303',category="food"))
    
    r=rbind(r,data.frame(item='Peas, beans',shortname='pulses',code='10401',category="food"))
    
    r=rbind(r,data.frame(item='Groundnuts in shell/shelled',shortname='peanuts',code='10501',category="food"))
    r=rbind(r,data.frame(item='Coconuts (mature/immature)',shortname='coconut',code='10502',category="food"))
    r=rbind(r,data.frame(item='Cashew, almonds, nuts',shortname='cashew_almonds', code='10503',category="food"))
    r=rbind(r,data.frame(item='Seeds from nuts',shortname='nut_products', code='10504',category="food"))
    
    r=rbind(r,data.frame(item='Onions, tomatoes, carrots and green pepper, other viungo',shortname='onion',code='10601',category="food"))
    r=rbind(r,data.frame(item='Spinach, cabbage and other green vegetables',shortname='greens',code='10602',category="food"))
    r=rbind(r,data.frame(item='Canned, dried and wild vegetables',shortname='dried_canned_veg',code='10603',category="food"))
    
    r=rbind(r,data.frame(item='Ripe bananas',shortname='banana_ripe',code='10701',category="food"))
    r=rbind(r,data.frame(item='Citrus fruits (oranges, lemon, tangarines, etc.)',shortname='citrus',code='10702',category="food"))
    r=rbind(r,data.frame(item='Mangoes, avocadoes and other fruits',shortname='mangoes',code='10703',category="food"))
    r=rbind(r,data.frame(item='Sugarcane',shortname='sugarcane',code='10704',category="food"))
    
    r=rbind(r,data.frame(item='Goat meat',shortname='goat',code='10801',category="food"))
    r=rbind(r,data.frame(item='Beef including minced sausage',shortname='beef', code='10802',category="food"))
    r=rbind(r,data.frame(item='Pork including sauages and bacon',shortname='pork',code='10803',category="food"))
    r=rbind(r,data.frame(item='Chicken and other poultry',shortname='chicken',code='10804',category="food"))
    r=rbind(r,data.frame(item='Wild birds and insects',shortname='wild_birds',code='10805',category="food"))
    r=rbind(r,data.frame(item='Other domestic/wild meat products',shortname='wild_meat',code='10806',category="food"))
    r=rbind(r,data.frame(item='Eggs',shortname='eggs',code='10807',category="food"))
    r=rbind(r,data.frame(item='Fresh fish and seafood (including dagaa)',shortname='fish_seafood',code='10808',category="food"))
    r=rbind(r,data.frame(item='Dried/salted/canned fish and seafood',shortname='dried_canned_fish', code='10809',category="food"))
    r=rbind(r,data.frame(item='Package fish',shortname='packaged_fish', code='10810',category="food"))
    
    r=rbind(r,data.frame(item='Fresh milk',shortname='fresh_milk', code='10901',category="food"))
    r=rbind(r,data.frame(item='Milk products (like cream, cheese, yoghurt etc)',shortname='milk_products',code='10902',category="food"))
    r=rbind(r,data.frame(item='Canned milk/milk powder',shortname='canned_milk', code='10903',category="food"))
    
    r=rbind(r,data.frame(item='Cooking oil',shortname='cooking_oil', code='11001',category="food"))
    r=rbind(r,data.frame(item='Butter, margarine, ghee and other fat products',shortname='butter_margarine', code='11002',category="food"))
    r=rbind(r,data.frame(item='Salt',shortname='salt',code='11003',category="food"))
    r=rbind(r,data.frame(item='Other spices',shortname='spices', code='11004',category="food"))
    
    r=rbind(r,data.frame(item='Tea dry',shortname='tea', code='11101',category="food"))
    r=rbind(r,data.frame(item='Coffee and cocoa',shortname='coffee',code='11102',category="food"))
    r=rbind(r,data.frame(item='Other raw materals for drinks',shortname='miscdrinkpowder',code='11103',category="food"))
    r=rbind(r,data.frame(item='Bottled/canned soft drinks (soda, juice, water)',shortname='canned_drink',code='11104',category="food"))
    r=rbind(r,data.frame(item='Prepared tea, coffee',shortname='readymade_tea_coffee',code='11105',category="food"))
    r=rbind(r,data.frame(item='Bottled beer',shortname='beer',code='11106',category="food"))
    r=rbind(r,data.frame(item='Local brews',shortname='brews',code='11107',category="food"))
    r=rbind(r,data.frame(item='Wine and spirits',shortname='winespirits',code='11108',category="food"))
    return(r)
  }
  
  ########################################################################
  # The relative order of assets - and assets alone - is used to generate
  # bit masks for ownership score 
  ########################################################################
  
  assets_order_2010_2012 <- function(shortnames) {
    
    if (!is.character(shortnames)){
      stop("shortnames must be a character array")
    }
    
    x<-NULL
    
    x<-rbind(x,data.frame(shortname ="bookexschool",     has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname="bamboo",            has_expenditure=TRUE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname="grass",             has_expenditure=TRUE,stringsAsFactors=FALSE))
    
    
    x<-rbind(x,data.frame(shortname="mosquito_net",      has_expenditure=TRUE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname="mat",               has_expenditure=TRUE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='bike',              has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='lantern',           has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname="light_bulbs",       has_expenditure=TRUE, stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname="linen",             has_expenditure=TRUE,stringsAsFactors=FALSE))
    #x<-rbind(x,data.frame(shortname='poultry',           has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    #x<-rbind(x,data.frame(shortname='hoe',               has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    #x<-rbind(x,data.frame(shortname='harrow',            has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    #x<-rbind(x,data.frame(shortname='wheelbarrow',       has_expenditure=FALSE,stringsAsFactors=FALSE))
    #x<-rbind(x,data.frame(shortname='plough',            has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    #x<-rbind(x,data.frame(shortname='handmill',          has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    
    x<-rbind(x,data.frame(shortname='iron',              has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='stove_other',       has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='stove_electricgas', has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='radio',             has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='public_transport',  has_expenditure=TRUE,stringsAsFactors=FALSE))
    
    
    x<-rbind(x,data.frame(shortname="landline",          has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='waterheater',       has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname ="mobile",           has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='sewingmachine',     has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='camera',            has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='musicplayer',       has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='videoplayer',       has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='musicsystem',       has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname="watch",             has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname="mattress",          has_expenditure=TRUE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname="carpet",            has_expenditure=TRUE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname="insurance",         has_expenditure=TRUE, stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname="building_material", has_expenditure=TRUE, stringsAsFactors=FALSE))
    #x<-rbind(x,data.frame(shortname='donkey',            has_expenditure=FALSE,stringsAsFactors=FALSE))
    #x<-rbind(x,data.frame(shortname='livestock',         has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='chair',             has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='table',             has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='bed',               has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='cupboard',          has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='sofa',              has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname="services",          has_expenditure=TRUE, stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='ac_fan',            has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='waterpump',            has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='tv',                has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='dishtv',            has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    #x<-rbind(x,data.frame(shortname='spraymachine',            has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='computer',          has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='phone',                  has_expenditure=TRUE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='sports_hobby',           has_expenditure=TRUE,stringsAsFactors=FALSE))
    #x<-rbind(x,data.frame(shortname='coffeepulpingmachine',   has_expenditure=FALSE,stringsAsFactors=FALSE))
    #x<-rbind(x,data.frame(shortname='milkingmachine',         has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname='refrigerator',           has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    #x<-rbind(x,data.frame(shortname='engine_outboard',        has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    
    x<-rbind(x,data.frame(shortname='motorbike',              has_expenditure=FALSE,stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname='car',                    has_expenditure=FALSE,stringsAsFactors=FALSE))
    #x<-rbind(x,data.frame(shortname='reaper',                 has_expenditure=FALSE,stringsAsFactors=FALSE))
    #x<-rbind(x,data.frame(shortname='fertiliserdistributor',  has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    #x<-rbind(x,data.frame(shortname='tractor',                has_expenditure=FALSE,stringsAsFactors=FALSE))
    #x<-rbind(x,data.frame(shortname='trailer',                has_expenditure=FALSE,stringsAsFactors=FALSE))
    #x<-rbind(x,data.frame(shortname='harvester',              has_expenditure=FALSE,stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname="funeral",                has_expenditure=TRUE, stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname="bride_price",      has_expenditure=TRUE, stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname="marriage",         has_expenditure=TRUE, stringsAsFactors=FALSE))
    
    x<-rbind(x,data.frame(shortname="land",             has_expenditure=FALSE, stringsAsFactors=FALSE))
    x<-rbind(x,data.frame(shortname="house",            has_expenditure=FALSE, stringsAsFactors=FALSE))
    
    #x<-rbind(x,data.frame(shortname ="", has_expenditure=TRUE,stringsAsFactors=FALSE))
    
    if (length(setdiff (unique(x$has_expenditure),c(TRUE,FALSE) ))>0 ) {
      stop("has_expenditure must be TRUE/FALSE")
    }
    y       <-    subset(x,is.element(shortname, shortnames) )
    y$mask  <-    2^seq(0,dim(y)[1]-1)
    return(y)
  }
  
  inheritable_assets_2010_2012 <- function(){
    x<-NULL
    x<-rbind(x,data.frame(shortname="radio",assettype="minor"))
    x<-rbind(x,data.frame(shortname="landline",assettype="minor"))
    x<-rbind(x,data.frame(shortname="mobile",assettype="minor"))
    x<-rbind(x,data.frame(shortname="computer",assettype="general"))
    x<-rbind(x,data.frame(shortname="refrigerator",assettype="general"))
    x<-rbind(x,data.frame(shortname="sewingmachine",assettype="general"))
    x<-rbind(x,data.frame(shortname="tv",assettype="general"))
    x<-rbind(x,data.frame(shortname="videoplayer",assettype="general"))
    x<-rbind(x,data.frame(shortname="waterheater",assettype="general"))
    x<-rbind(x,data.frame(shortname="musicplayer",assettype="general"))
    x<-rbind(x,data.frame(shortname="musicsystem",assettype="general"))
    x<-rbind(x,data.frame(shortname="dishtv",assettype="general"))
    x<-rbind(x,data.frame(shortname="chair",assettype="general"))
    x<-rbind(x,data.frame(shortname="sofa",assettype="general"))
    x<-rbind(x,data.frame(shortname="table",assettype="general"))
    x<-rbind(x,data.frame(shortname="watch",assettype="general"))
    x<-rbind(x,data.frame(shortname="bed",assettype="general"))
    x<-rbind(x,data.frame(shortname="mosquitonet",assettype="minor"))
    x<-rbind(x,data.frame(shortname="iron",assettype="general"))
    x<-rbind(x,data.frame(shortname="stove_other",assettype="general"))
    x<-rbind(x,data.frame(shortname="stove_electricgas",assettype="general"))
    x<-rbind(x,data.frame(shortname="car",assettype="general"))
    x<-rbind(x,data.frame(shortname="motorbike",assettype="general"))
    x<-rbind(x,data.frame(shortname="bike",assettype="general"))
    x<-rbind(x,data.frame(shortname="cart",assettype="general"))
    x<-rbind(x,data.frame(shortname="animalcart",assettype="general"))
    x<-rbind(x,data.frame(shortname="boat",assettype="general"))
    x<-rbind(x,data.frame(shortname="wheelbarrow",assettype="general"))
    x<-rbind(x,data.frame(shortname="livestock",assettype="general"))
    x<-rbind(x,data.frame(shortname="poultry",assettype="general"))
    x<-rbind(x,data.frame(shortname="engine_outboard",assettype="general"))
    x<-rbind(x,data.frame(shortname="donkey",assettype="general"))
    x<-rbind(x,data.frame(shortname="ac_fan",assettype="general"))
    x<-rbind(x,data.frame(shortname="spraymachine",assettype="general"))
    x<-rbind(x,data.frame(shortname="waterpump",assettype="general"))
    x<-rbind(x,data.frame(shortname="tractor",assettype="general"))
    x<-rbind(x,data.frame(shortname="trailer",assettype="general"))
    x<-rbind(x,data.frame(shortname="plough",assettype="general"))
    x<-rbind(x,data.frame(shortname="harrow",assettype="general"))
    x<-rbind(x,data.frame(shortname="handmill",assettype="general"))
    x<-rbind(x,data.frame(shortname="coffeepulpingmachine",assettype="general"))
    x<-rbind(x,data.frame(shortname="powertiller",assettype="general"))
    x<-rbind(x,data.frame(shortname="house",assettype="general"))
    return(x)
    
  }
  
  asset_types_2010_2012 <-function()
  {
    x<-NULL
    x<-rbind(x,data.frame(shortname="radio",assettype="electric"))
    x<-rbind(x,data.frame(shortname="landline",assettype="electric"))
    x<-rbind(x,data.frame(shortname="mobile",assettype="electric"))
    x<-rbind(x,data.frame(shortname="computer",assettype="electric"))
    x<-rbind(x,data.frame(shortname="refrigerator",assettype="electric"))
    x<-rbind(x,data.frame(shortname="sewingmachine",assettype="electric"))
    x<-rbind(x,data.frame(shortname="tv",assettype="electric"))
    x<-rbind(x,data.frame(shortname="videoplayer",assettype="electric"))
    x<-rbind(x,data.frame(shortname="waterheater",assettype="electric"))
    x<-rbind(x,data.frame(shortname="musicplayer",assettype="electric"))
    x<-rbind(x,data.frame(shortname="musicsystem",assettype="electric"))
    x<-rbind(x,data.frame(shortname="dishtv",assettype="electric"))
    x<-rbind(x,data.frame(shortname="chair",assettype="equipment"))
    x<-rbind(x,data.frame(shortname="sofa",assettype="equipment"))
    x<-rbind(x,data.frame(shortname="table",assettype="equipment"))
    x<-rbind(x,data.frame(shortname="watch",assettype="equipment"))
    x<-rbind(x,data.frame(shortname="bed",assettype="equipment"))
    x<-rbind(x,data.frame(shortname="mosquitonet",assettype="equipment"))
    x<-rbind(x,data.frame(shortname="iron",assettype="equipment"))
    x<-rbind(x,data.frame(shortname="stove_other",assettype="equipment"))
    x<-rbind(x,data.frame(shortname="stove_electricgas",assettype="equipment"))
    x<-rbind(x,data.frame(shortname="car",assettype="transport"))
    x<-rbind(x,data.frame(shortname="motorbike",assettype="transport"))
    x<-rbind(x,data.frame(shortname="bike",assettype="transport"))
    x<-rbind(x,data.frame(shortname="cart",assettype="transport"))
    x<-rbind(x,data.frame(shortname="animalcart",assettype="transport"))
    x<-rbind(x,data.frame(shortname="boat",assettype="transport"))
    x<-rbind(x,data.frame(shortname="wheelbarrow",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="livestock",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="poultry",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="engine_outboard",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="donkey",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="ac_fan",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="spraymachine",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="waterpump",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="tractor",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="trailer",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="plough",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="harrow",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="handmill",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="coffeepulpingmachine",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="powertiller",assettype="agricultural"))
    x<-rbind(x,data.frame(shortname="house",assettype="housing"))
    return(x)
  }
  
  lsms_groups_sparsenessbased_2010_2012<- function(){
    
    x<-NULL
    
    x<-rbind(x,data.frame(category='food', group='low', shortname='cooking_oil'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='butter_margarine'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='rice_husked'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='rice_paddy'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='maize_green'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='maize_grain'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='maize_flour'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='millet_grain'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='millet_flour'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='wheat'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='bread'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='bunscakes'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='pasta'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='othercereal'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='pulses'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='sugar'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='sweet'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='honey'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='peanuts'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='coconut'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='cashew_almonds'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='nut_products'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='milk_products'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='fresh_milk'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='canned_milk'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='goat'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='beef'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='pork'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='chicken'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='wild_birds'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='wild_meat'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='fish_seafood'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='dried_canned_fish'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='packaged_fish'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='eggs'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='beer'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='brews'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='winespirits'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='spices'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='salt'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='onion'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='greens'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='dried_canned_veg'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='cassava_fresh'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='cassava_flour'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='sweet_potato'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='yam'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='potatoes'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='banana_green'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='othervegstarch'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='tea'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='coffee'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='miscdrinkpowder'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='canned_drink'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='readymade_tea_coffee'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='banana_ripe'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='citrus'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='mangoes'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='sugarcane'))
    
    x<-rbind(x,data.frame(category = "energy", group="low", shortname="kerosene"))
    x<-rbind(x,data.frame(category = "energy", group="high", shortname="charcoal"))
    x<-rbind(x,data.frame(category = "energy", group="low", shortname="gas"))
    x<-rbind(x,data.frame(category = "energy", group="low",shortname="electricity"))
    
    #x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="cigarettes"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="cellphone_voucher"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="bar_soap"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="clothes_soap"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="toothpaste"))
    x<-rbind(x,data.frame(category = "personal_products", group="high",shortname="toilet_paper"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="skin_cream"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="other_personal"))
    x<-rbind(x,data.frame(category = "personal_products", group="high",shortname="misc_cleaning"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="household_products_repair"))
    x<-rbind(x,data.frame(category = "personal_products", group="high",shortname="consumer_durables_repair"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="mensclothes"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="womensclothes"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="childrensclothes"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="mensshoes"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="womensshoes"))
    x<-rbind(x,data.frame(category = "personal_products", group="low",shortname="childrensshoes"))
    x<-rbind(x,data.frame(category = "personal_products", group="high",shortname="phone"))
    x<-rbind(x,data.frame(category = "personal_products", group="high",shortname="sports_hobby"))
    x<-rbind(x,data.frame(category = "personal_products", group="high",shortname="camera"))
    
    ############
    
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="watch"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="radio"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="landline"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="mobile"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="videoplayer"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="computer"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="iron"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="stove_electricgas"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="stove_other"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="waterheater"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="musicplayer"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="musicsystem"))
    # x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="bookexschool"))
    # 
    
    
    x<-rbind(x,data.frame(category = "transport", group="low",          shortname="public_transport"))
    x<-rbind(x,data.frame(category = "transport", group="high",         shortname="motor_repair"))
    x<-rbind(x,data.frame(category = "transport", group="high",         shortname="petrol"))
    x<-rbind(x,data.frame(category = "transport", group="low",          shortname="bicycle_repair"))
    
    x<-rbind(x,data.frame(category = "social_functions", group="low",shortname="services"))
    x<-rbind(x,data.frame(category = "social_functions", group="low",shortname="bride_price"))
    x<-rbind(x,data.frame(category = "social_functions", group="low",shortname="marriage"))
    x<-rbind(x,data.frame(category = "social_functions", group="low",shortname="funeral"))
    
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="chair"))
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="sofa"))
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="table"))
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="bed"))
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="cupboard"))
    
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="refrigerator"))
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="tv"))
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="ac_fan"))
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="dishtv"))
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="videoplayer"))
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="computer"))
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="waterheater"))
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="musicplayer"))
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="musicsystem"))
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="land"))
    # x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="house")) # (excluded if housingstatus is used as a CV)
    
    
    x<-rbind(x,data.frame(category = "housing", group="low",shortname="light_bulbs"))
    x<-rbind(x,data.frame(category = "housing", group="low",shortname="carpet"))
    x<-rbind(x,data.frame(category = "housing", group="low",shortname="linen"))
    x<-rbind(x,data.frame(category = "housing", group="low",shortname="mat"))
    x<-rbind(x,data.frame(category = "housing", group="low",shortname="mosquito_net"))
    x<-rbind(x,data.frame(category = "housing", group="low",shortname="mattress"))
    x<-rbind(x,data.frame(category = "housing", group="low",shortname="building_material"))
    x<-rbind(x,data.frame(category = "housing", group="low",shortname="insurance"))
    x<-rbind(x,data.frame(category = "housing", group="low",shortname="bamboo"))
    x<-rbind(x,data.frame(category = "housing", group="low",shortname="grass"))
    
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="refrigerator"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="sewingmachine"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="tv"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="chair"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="sofa"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="table"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="bed"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="cupboard"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="lantern"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="mosquitonet"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="wheelbarrow"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="livestock"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="poultry"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="engine_outboard"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="donkey"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="land"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="house"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="ac_fan"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="dishtv"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="hoe"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="spraymachine"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="waterpump"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="reaper"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="tractor"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="trailer"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="plough"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="harrow"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="milkingmachine"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="harvester"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="handmill"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="coffeepulpingmachine"))
    # x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="fertiliserdistributor"))
    
    
    return(x)
  }

  
  add_categories_simple <- function(x)  {
    
    x<-rbind(x,data.frame(category='fat', group='quality', shortname='cooking_oil'))
    x<-rbind(x,data.frame(category='fat', group='quality', shortname='butter_margarine'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='rice_husked'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='rice_paddy'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='maize_green'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='maize_grain'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='maize_flour'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='millet_grain'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='millet_flour'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='wheat'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='bread'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='bunscakes'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='pasta'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='othercereal'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='pulses'))
    x<-rbind(x,data.frame(category='sugars', group='quality', shortname='sugar'))
    x<-rbind(x,data.frame(category='sugars', group='quality', shortname='sweet'))
    x<-rbind(x,data.frame(category='sugars', group='quality', shortname='honey'))
    x<-rbind(x,data.frame(category='fat', group='quality', shortname='peanuts'))
    x<-rbind(x,data.frame(category='fat', group='quality', shortname='coconut'))
    x<-rbind(x,data.frame(category='fat', group='quality', shortname='cashew_almonds'))
    x<-rbind(x,data.frame(category='fat', group='quality', shortname='nut_products'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='milk_products'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='fresh_milk'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='canned_milk'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='goat'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='beef'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='pork'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='chicken'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='wild_birds'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='wild_meat'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='fish_seafood'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='dried_canned_fish'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='packaged_fish'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='eggs'))
    x<-rbind(x,data.frame(category='beverages', group='quality', shortname='beer'))
    x<-rbind(x,data.frame(category='beverages', group='quality', shortname='brews'))
    x<-rbind(x,data.frame(category='beverages', group='quality', shortname='winespirits'))
    x<-rbind(x,data.frame(category='condiments', group='quality', shortname='spices'))
    x<-rbind(x,data.frame(category='condiments', group='quality', shortname='salt'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='onion'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='greens'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='dried_canned_veg'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='cassava_fresh'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='cassava_flour'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='sweet_potato'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='yam'))
    x<-rbind(x,data.frame(category='starch', group='quality', shortname='potatoes'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='banana_green'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='othervegstarch'))
    x<-rbind(x,data.frame(category='beverages', group='quality', shortname='tea'))
    x<-rbind(x,data.frame(category='beverages', group='quality', shortname='coffee'))
    x<-rbind(x,data.frame(category='beverages', group='quality', shortname='miscdrinkpowder'))
    x<-rbind(x,data.frame(category='beverages', group='quality', shortname='canned_drink'))
    x<-rbind(x,data.frame(category='beverages', group='quality', shortname='readymade_tea_coffee'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='banana_ripe'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='citrus'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='mangoes'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='sugarcane'))
    
    x<-rbind(x,data.frame(category = "energy", group="low", shortname="kerosene"))
    x<-rbind(x,data.frame(category = "energy", group="high", shortname="charcoal"))
    x<-rbind(x,data.frame(category = "energy", group="high", shortname="gas"))
    x<-rbind(x,data.frame(category = "energy", group="high",shortname="electricity"))
    x<-rbind(x,data.frame(category = "energy", group="high", shortname="petrol"))
    
    return(x)
  }
  
  
  
  add_categories_price_structured_based <- function(x)  {
    
    x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='cooking_oil'))
    #x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='butter_margarine'))
    x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='rice_husked'))
    x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='rice_paddy'))
    x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='maize_green'))
    x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='maize_grain'))
    x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='maize_flour'))
    x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='millet_grain'))
    x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='millet_flour'))
    x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='wheat'))
    x<-rbind(x,data.frame(category='nonfresh', group='quality', shortname='bread'))
    x<-rbind(x,data.frame(category='densefoods', group='quality', shortname='bunscakes'))
    x<-rbind(x,data.frame(category='densefoods', group='quality', shortname='pasta'))
    x<-rbind(x,data.frame(category='densefoods', group='quality', shortname='othercereal'))
    x<-rbind(x,data.frame(category='densefoods', group='quality', shortname='pulses'))
    x<-rbind(x,data.frame(category='complements', group='quality', shortname='sugar'))
    x<-rbind(x,data.frame(category='complements', group='quality', shortname='sweet'))
    x<-rbind(x,data.frame(category='complements', group='quality', shortname='honey'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='peanuts'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='coconut'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='cashew_almonds'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='nut_products'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='milk_products'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='fresh_milk'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='canned_milk'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='goat'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='beef'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='pork'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='chicken'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='wild_birds'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='wild_meat'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='fish_seafood'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='dried_canned_fish'))
    x<-rbind(x,data.frame(category='protein', group='quality', shortname='packaged_fish'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='eggs'))
    x<-rbind(x,data.frame(category='alcohol', group='quality', shortname='beer'))
    x<-rbind(x,data.frame(category='alcohol', group='quality', shortname='brews'))
    x<-rbind(x,data.frame(category='alcohol', group='quality', shortname='winespirits'))
    x<-rbind(x,data.frame(category='complements', group='quality', shortname='spices'))
    x<-rbind(x,data.frame(category='complements', group='quality', shortname='salt'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='onion'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='greens'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='dried_canned_veg'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='cassava_fresh'))
    x<-rbind(x,data.frame(category='densefoods', group='quality', shortname='cassava_flour'))
    x<-rbind(x,data.frame(category='densefoods', group='quality', shortname='sweet_potato'))
    x<-rbind(x,data.frame(category='densefoods', group='quality', shortname='yam'))
    x<-rbind(x,data.frame(category='densefoods', group='quality', shortname='potatoes'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='banana_green'))
    x<-rbind(x,data.frame(category='densefoods', group='quality', shortname='othervegstarch'))
    x<-rbind(x,data.frame(category='complements', group='quality', shortname='tea'))
    x<-rbind(x,data.frame(category='complements', group='quality', shortname='coffee'))
    x<-rbind(x,data.frame(category='complements', group='quality', shortname='miscdrinkpowder'))
    x<-rbind(x,data.frame(category='complements', group='quality', shortname='canned_drink'))
    x<-rbind(x,data.frame(category='complements', group='quality', shortname='readymade_tea_coffee'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='banana_ripe'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='citrus'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='mangoes'))
    x<-rbind(x,data.frame(category='fruitsveg', group='quality', shortname='sugarcane'))
    
    x<-rbind(x,data.frame(category = "energy", group="quality", shortname="kerosene"))
    x<-rbind(x,data.frame(category = "energy", group="quality", shortname="charcoal"))
    x<-rbind(x,data.frame(category = "energy", group="quality", shortname="gas"))
    x<-rbind(x,data.frame(category = "energy", group="quality",shortname="electricity"))
    x<-rbind(x,data.frame(category = "energy", group="quality", shortname="petrol"))
    
    return(x)
  }
  lsms_groups_qualitybased_2010_2012<- function(){
    
    x<-NULL
    
    x<- add_categories_price_structured_based(x)
    
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="cigarettes"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="cellphone_voucher"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="bar_soap"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="clothes_soap"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="toothpaste"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="toilet_paper"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="skin_cream"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="other_personal"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="misc_cleaning"))
    
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="household_products_repair"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="consumer_durables_repair"))
    
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="mensclothes"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="womensclothes"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="childrensclothes"))
    
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="mensshoes"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="womensshoes"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="childrensshoes"))
    
    #x<-rbind(x,data.frame(category = "transport", group="expenditureonly",          shortname="public_transport"))
    #x<-rbind(x,data.frame(category = "transport", group="expenditureonly",         shortname="motor_repair"))
    #x<-rbind(x,data.frame(category = "transport", group="expenditureonly",         shortname="petrol"))
    #x<-rbind(x,data.frame(category = "transport", group="expenditureonly",          shortname="bicycle_repair"))
    
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="services"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="bride_price"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="marriage"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="funeral"))
    
    #    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="refrigerator"))
    #    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="tv"))
    #    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="ac_fan"))
    #    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="dishtv"))
    #    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="videoplayer"))
    #    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="computer"))
    #    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="waterheater"))
    #    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="musicplayer"))
    #    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="musicsystem"))
    #    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="land"))
    ##x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="house")) # (excluded if housingstatus is used as a CV)
    
    
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="light_bulbs"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="carpet"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="linen"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="mat"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="mosquito_net"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="mattress"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="building_material"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="insurance"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="bamboo"))
    x<-rbind(x,data.frame(category = "household", group="expenditureonly",shortname="grass"))
    
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="refrigerator"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="sewingmachine"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="tv"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="chair"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="sofa"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="table"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="bed"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="cupboard"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="lantern"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="mosquitonet"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="wheelbarrow"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="livestock"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="poultry"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="engine_outboard"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="donkey"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="land"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="house"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="ac_fan"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="dishtv"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="hoe"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="spraymachine"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="waterpump"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="reaper"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="tractor"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="trailer"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="plough"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="harrow"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="milkingmachine"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="harvester"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="handmill"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="coffeepulpingmachine"))
    #x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="fertiliserdistributor"))
    
    return(x)
  }
  
  
  lsms_groups_pricebased_2010_2012<- function(){
    
    x<-NULL
    
    x<-rbind(x,data.frame(category='food', group='low', shortname='cooking_oil'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='butter_margarine'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='rice_husked'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='rice_paddy'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='maize_green'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='maize_grain'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='maize_flour'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='millet_grain'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='millet_flour'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='wheat'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='bread'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='bunscakes'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='pasta'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='othercereal'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='pulses'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='sugar'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='sweet'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='honey'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='peanuts'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='coconut'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='cashew_almonds'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='nut_products'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='milk_products'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='fresh_milk'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='canned_milk'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='goat'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='beef'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='pork'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='chicken'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='wild_birds'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='wild_meat'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='fish_seafood'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='dried_canned_fish'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='packaged_fish'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='eggs'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='beer'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='brews'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='winespirits'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='spices'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='salt'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='onion'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='greens'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='dried_canned_veg'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='cassava_fresh'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='cassava_flour'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='sweet_potato'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='yam'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='potatoes'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='banana_green'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='othervegstarch'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='tea'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='coffee'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='miscdrinkpowder'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='canned_drink'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='readymade_tea_coffee'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='banana_ripe'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='citrus'))
    x<-rbind(x,data.frame(category='food', group='high', shortname='mangoes'))
    x<-rbind(x,data.frame(category='food', group='low', shortname='sugarcane'))
    
    x<-rbind(x,data.frame(category = "energy", group="low", shortname="kerosene"))
    x<-rbind(x,data.frame(category = "energy", group="low", shortname="charcoal"))
    x<-rbind(x,data.frame(category = "energy", group="low", shortname="gas"))
    x<-rbind(x,data.frame(category = "energy", group="high",shortname="electricity"))
    x<-rbind(x,data.frame(category = "energy", group="low", shortname="petrol"))
    
    #x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="cigarettes"))
    x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="cellphone_voucher"))
    x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="bar_soap"))
    x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="clothes_soap"))
    x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="toothpaste"))
    x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="toilet_paper"))
    x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="skin_cream"))
    x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="other_personal"))
    x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="misc_cleaning"))
    
    x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="household_products_repair"))
    x<-rbind(x,data.frame(category = "personal_products", group="expenditure",shortname="consumer_durables_repair"))
    
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="sports_hobby"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="camera"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="watch"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="phone"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="radio"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="landline"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="mobile"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="videoplayer"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="computer"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="iron"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="stove_electricgas"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="stove_other"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="waterheater"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="musicplayer"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="musicsystem"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="bookexschool"))
    
    
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="mensclothes"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="womensclothes"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="childrensclothes"))
    
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="mensshoes"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="womensshoes"))
    x<-rbind(x,data.frame(category = "personal_products", group="asset",shortname="childrensshoes"))
    
    
    #    x<-rbind(x,data.frame(category = "transport", group="assetsonly",shortname="bike"))
    #    x<-rbind(x,data.frame(category = "transport", group="assetsonly",shortname="public_transport"))
    #    x<-rbind(x,data.frame(category = "transport", group="assetsonly",shortname="motorbike"))
    #    x<-rbind(x,data.frame(category = "transport", group="assetsonly",shortname="car"))
    
    x<-rbind(x,data.frame(category = "transport", group="low",          shortname="public_transport"))
    x<-rbind(x,data.frame(category = "transport", group="high",         shortname="motor_repair"))
    x<-rbind(x,data.frame(category = "transport", group="high",         shortname="petrol"))
    x<-rbind(x,data.frame(category = "transport", group="low",          shortname="bicycle_repair"))
    
    x<-rbind(x,data.frame(category = "social_functions", group="expenditure",shortname="services"))
    x<-rbind(x,data.frame(category = "social_functions", group="expenditure",shortname="bride_price"))
    x<-rbind(x,data.frame(category = "social_functions", group="expenditure",shortname="marriage"))
    x<-rbind(x,data.frame(category = "social_functions", group="expenditure",shortname="funeral"))
    
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="chair"))
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="sofa"))
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="table"))
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="bed"))
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="cupboard"))
    
    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="refrigerator"))
    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="tv"))
    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="ac_fan"))
    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="dishtv"))
    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="videoplayer"))
    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="computer"))
    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="waterheater"))
    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="musicplayer"))
    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="musicsystem"))
    x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="land"))
    #x<-rbind(x,data.frame(category = "social_functions", group="asset"      ,shortname="house")) # (excluded if housingstatus is used as a CV)
    
    
    x<-rbind(x,data.frame(category = "housing", group="expenditure",shortname="light_bulbs"))
    x<-rbind(x,data.frame(category = "housing", group="expenditure",shortname="carpet"))
    x<-rbind(x,data.frame(category = "housing", group="expenditure",shortname="linen"))
    x<-rbind(x,data.frame(category = "housing", group="expenditure",shortname="mat"))
    x<-rbind(x,data.frame(category = "housing", group="expenditure",shortname="mosquito_net"))
    x<-rbind(x,data.frame(category = "housing", group="expenditure",shortname="mattress"))
    x<-rbind(x,data.frame(category = "housing", group="expenditure",shortname="building_material"))
    x<-rbind(x,data.frame(category = "housing", group="expenditure",shortname="insurance"))
    x<-rbind(x,data.frame(category = "housing", group="expenditure",shortname="bamboo"))
    x<-rbind(x,data.frame(category = "housing", group="expenditure",shortname="grass"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="refrigerator"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="sewingmachine"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="tv"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="chair"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="sofa"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="table"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="bed"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="cupboard"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="lantern"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="mosquitonet"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="wheelbarrow"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="livestock"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="poultry"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="engine_outboard"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="donkey"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="land"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="house"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="ac_fan"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="dishtv"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="hoe"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="spraymachine"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="waterpump"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="reaper"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="tractor"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="trailer"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="plough"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="harrow"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="milkingmachine"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="harvester"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="handmill"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="coffeepulpingmachine"))
    x<-rbind(x,data.frame(category = "housing", group="asset"      ,shortname="fertiliserdistributor"))
    
    
    return(x)
  }
  
  
  multiplyLsmsQuantities <-function(dat,quantity_field_name,item_field_name,factor,items_list){
    
    if (is.factor(dat[,item_field_name])){
      stop(item_field_name," must be converted to integers")
    }
    if (!is.atomic(factor) || !is.numeric(factor) || factor<1){
      stop("factor must be a numeric greater than or equal to 1")
    }
    
    if (!is.vector(items_list)){
      stop("multiplyLsmsQuantities: itemslist must be a vector")
    }
    # if not a character or has length>0
    
    if (!(length(quantity_field_name) ==1  && is.character(quantity_field_name))){
      stop(paste("Invalid quantity field name",quantity_field_name))
    }
    
    if (!(length(item_field_name) ==1  && is.character(item_field_name))){
      stop(paste("Invalid item field name",item_field_name))
    }
    
    if (!is.data.frame(dat)){
      stop("data must be a data.frame")
    }
    #  if (is.element("mulfactor__",colnames(dat))){
    #    stop("input data already has a column named mulfactor__")
    #  }
    
    mulfactor_1 =  (factor-1)*as.integer(is.element(as.integer(dat[,item_field_name]),as.integer(items_list)))
    dat[,quantity_field_name]<-mulfactor_1*dat[,quantity_field_name]+dat[,quantity_field_name]
    return(dat)
  }
  get_lsms_secj_info_columns_2010<-function(){
    return(c("hhid","housingstatus","houserent","roomsnum_primary","roomsnum_secondary","wallsmaterial","roofmaterial","floormaterial","toilet","cookingfuel","lightingfuel","dryseasonwatersource"))
  }
  
  get_lsms_secj_fields_mapping_2010<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="hh_j01",name="housingstatus")) # 1- owner occupied, 2- EMPLOYER PROVIDED - SUBSIDIZED, 3-EMPLOYER PROVIDED - FREE, 4- RENTED, 5- FREE, 6-NOMADS 
    s= rbind(s,data.frame(iesname="hh_j03",name="houserent"))
    s= rbind(s,data.frame(iesname="hh_j04_1",name="roomsnum_primary"))
    s= rbind(s,data.frame(iesname="hh_j04_2",name="roomsnum_secondary"))
    s= rbind(s,data.frame(iesname="hh_j05",name="wallsmaterial"))
    s= rbind(s,data.frame(iesname="hh_j06",name="roofmaterial"))
    s= rbind(s,data.frame(iesname="hh_j07",name="floormaterial"))
    s= rbind(s,data.frame(iesname="hh_j10",name="toilet"))
    s= rbind(s,data.frame(iesname="hh_j16",name="cookingfuel"))
    s= rbind(s,data.frame(iesname="hh_j17",name="lightingfuel"))
    s= rbind(s,data.frame(iesname="hh_j27_2",name="dryseasonwatersource"))
    return(s)
  }
  
  get_lsms_secj_info_columns_2012<-function(){
    return(c("hhid","housingstatus","houserent","roomsnum_primary","roomsnum_secondary","wallsmaterial","roofmaterial",
             "floormaterial","trash_type","toilet","is_toilet_share","num_toilet_share","youngest_child_stool_disposal",
             "cookingfuel","lightingfuel","electricity_source","rainyseasonwatersource","rainyseasonwatersourcetime",
             "dryseasonwatersource","dryseasonwatersourcetime"))
  }
  
  
  get_lsms_secj_fields_mapping_2012<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y3_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="hh_i01",name="housingstatus")) # 1- owner occupied, 2- EMPLOYER PROVIDED - SUBSIDIZED, 3-EMPLOYER PROVIDED - FREE, 4- RENTED, 5- FREE, 6-NOMADS 
    s= rbind(s,data.frame(iesname="hh_i03",name="houserent"))
    s= rbind(s,data.frame(iesname="hh_i07_1",name="roomsnum_primary"))
    s= rbind(s,data.frame(iesname="hh_i07_2",name="roomsnum_secondary"))
    s= rbind(s,data.frame(iesname="hh_i08",name="wallsmaterial"))
    s= rbind(s,data.frame(iesname="hh_i09",name="roofmaterial"))
    s= rbind(s,data.frame(iesname="hh_i10",name="floormaterial"))
    s= rbind(s,data.frame(iesname="hh_i11",name="trash_type"))
    s= rbind(s,data.frame(iesname="hh_i12",name="toilet"))
    s= rbind(s,data.frame(iesname="hh_i13",name="is_toilet_share"))
    s= rbind(s,data.frame(iesname="hh_i14",name="num_toilet_share"))
    
    s= rbind(s,data.frame(iesname="hh_i15",name="youngest_child_stool_disposal"))
    s= rbind(s,data.frame(iesname="hh_i16",name="cookingfuel"))
    s= rbind(s,data.frame(iesname="hh_i17",name="lightingfuel"))
    s= rbind(s,data.frame(iesname="hh_i18",name="electricity_source"))
    s= rbind(s,data.frame(iesname="hh_i19",name="rainyseasonwatersource"))
    s= rbind(s,data.frame(iesname="hh_i20",name="rainyseasonwatersourcetime"))
    s= rbind(s,data.frame(iesname="hh_i22",name="dryseasonwatersource"))
    s= rbind(s,data.frame(iesname="hh_i23",name="dryseasonwatersourcetime"))
    
    return(s)
  }
  
  get_lsms_secj_fields_mapping_2014<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y4_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="hh_i01",name="housingstatus")) # 1- owner occupied, 2- EMPLOYER PROVIDED - SUBSIDIZED, 3-EMPLOYER PROVIDED - FREE, 4- RENTED, 5- FREE, 6-NOMADS 
    s= rbind(s,data.frame(iesname="hh_i03",name="houserent"))
    s= rbind(s,data.frame(iesname="hh_i07_1",name="roomsnum_primary"))
    s= rbind(s,data.frame(iesname="hh_i07_2",name="roomsnum_secondary"))
    s= rbind(s,data.frame(iesname="hh_i08",name="wallsmaterial"))
    s= rbind(s,data.frame(iesname="hh_i09",name="roofmaterial"))
    s= rbind(s,data.frame(iesname="hh_i10",name="floormaterial"))
    s= rbind(s,data.frame(iesname="hh_i11",name="trash_type"))
    s= rbind(s,data.frame(iesname="hh_i12",name="toilet"))
    s= rbind(s,data.frame(iesname="hh_i13",name="is_toilet_share"))
    s= rbind(s,data.frame(iesname="hh_i14",name="num_toilet_share"))
    
    s= rbind(s,data.frame(iesname="hh_i15",name="youngest_child_stool_disposal"))
    s= rbind(s,data.frame(iesname="hh_i16",name="cookingfuel"))
    s= rbind(s,data.frame(iesname="hh_i17",name="lightingfuel"))
    s= rbind(s,data.frame(iesname="hh_i18",name="electricity_source"))
    s= rbind(s,data.frame(iesname="hh_i19",name="rainyseasonwatersource"))
    s= rbind(s,data.frame(iesname="hh_i22",name="rainyseasonwatersourcetime"))

    return(s)
  }
  
  get_lsms_secj_info_columns_2014<-function(){
    return(c("hhid","housingstatus","houserent","roomsnum_primary","roomsnum_secondary","wallsmaterial","roofmaterial",
             "floormaterial","trash_type","toilet","is_toilet_share","num_toilet_share","youngest_child_stool_disposal",
             "cookingfuel","lightingfuel","electricity_source","rainyseasonwatersource","rainyseasonwatersourcetime"))
  }
  
  
  
  get_lsms_secm_info_columns<-function(year){
    if (year == 2008 || year == 2010 || year ==2012 || year == 2014){
      return(c("hhid","item","is_consumed","cost"))
    } 
    stop(paste("Not secm info columns for year: ",year))
  }
  
  get_region_popdensity_map<-function(){
    return(read.csv('./lsms/tnzpopdensity.csv'))
  }
  
  #########################
  
  read_tnz <- function(filename,convert_factors) {
    if (!is.logical(convert_factors) || !is.atomic(convert_factors)){
      stop("convert_factords must be ")
    }
    dat1 = read.dta(filename,convert.factors = convert_factors);
    dat2 = as.data.frame(dat1,stringsAsFactors=FALSE);
    dat3 = dat2[as.numeric(dat2$y2_hhid)>0,] # only take data with hhid>0
    return(dat3);
  }
  #########################
  
  
  get_lsms_secm_fields_mapping<-function(year){
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="hh_m01_2",name="is_consumed"))
      s= rbind(s,data.frame(iesname="itemcode",name="item"))
      s= rbind(s,data.frame(iesname="hh_m02",name="cost"))
      s= rbind(s,data.frame(iesname="hh_m03",name="price"))
      return(s)
    }
    if (year == 2012){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="y3_hhid",name="hhid"))
      #s= rbind(s,data.frame(iesname="y2_hhid",name="y2_hhid"))
      
      s= rbind(s,data.frame(iesname="hh_l01",name="is_consumed"))
      s= rbind(s,data.frame(iesname="itemcode",name="item"))
      s= rbind(s,data.frame(iesname="hh_l02",name="cost"))
      s= rbind(s,data.frame(iesname="hh_l03",name="price"))
      return(s)
    }
    if (year == 2014){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="y4_hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="hh_l01",name="is_consumed"))
      s= rbind(s,data.frame(iesname="itemcode",name="item"))
      s= rbind(s,data.frame(iesname="hh_l02",name="cost"))
      s= rbind(s,data.frame(iesname="hh_l03",name="price"))
      return(s)
    }
    
    if (year == 2008){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="smq1",name="is_consumed"))
      s= rbind(s,data.frame(iesname="smcode",name="item"))
      s= rbind(s,data.frame(iesname="smq2",name="cost"))
      s= rbind(s,data.frame(iesname="smq3",name="price"))
      return(s)
    }
    stop(paste("Not secm info columns for year: ",year))
  }
  
  get_lsms_secl_info_columns_2010<-function(){
    return(c("hhid","item","is_consumed","cost"))
  }
  
  get_lsms_secl_fields_mapping_2010<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="hh_l01_2",name="is_consumed"))
    s= rbind(s,data.frame(iesname="itemcode",name="item"))
    s= rbind(s,data.frame(iesname="hh_l02",name="cost"))
    return(s)
  }
  
  get_ohs_secc_columns_lsms_2010<-function(){
    return(c("hhid","personid","is_ge5y","litlang","is_literate","highest_educ","schoolowner",
             "schoolconveyance","has_missedschool","educexpense","has_adulteduc","adulteducmonths"))
  }
  
  get_ohs_secc_fields_mapping_lsms_2010<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="indidy2",name="personid"))
    s= rbind(s,data.frame(iesname="hh_c01",name="is_ge5y"))
    s= rbind(s,data.frame(iesname="hh_c02",name="litlang"))
    s= rbind(s,data.frame(iesname="hh_c01",name="is_literate"))
    
    s= rbind(s,data.frame(iesname="hh_c07",name="highest_educ"))
    s= rbind(s,data.frame(iesname="hh_c12",name="schoolowner"))
    s= rbind(s,data.frame(iesname="hh_c14",name="schoolconveyance"))
    s= rbind(s,data.frame(iesname="hh_c17",name="has_missedschool"))
    s= rbind(s,data.frame(iesname="hh_c28_8",name="educexpense"))
    s= rbind(s,data.frame(iesname="hh_c29",name="has_adulteduc"))
    s= rbind(s,data.frame(iesname="hh_c30",name="adulteducmonths"))
    return(s)
  }
  
  get_ohs_secc_columns_lsms_2012<-function(){
    return(c("hhid","personid","is_ge5y","litlang","is_literate","highest_educ","schoolowner",
             "schoolconveyance","has_missedschool","educexpense","has_adulteduc","adulteducmonths",
             "attended_school","school_start_age","school_leaving_year","schoolconveyance","schooltransporttime"));
  }
  
  get_ohs_secc_fields_mapping_lsms_2012<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y3_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="indidy3",name="personid"))
    s= rbind(s,data.frame(iesname="hh_c01",name="is_ge5y"))
    s= rbind(s,data.frame(iesname="hh_c02",name="litlang"))
    s= rbind(s,data.frame(iesname="hh_c03",name="attended_school"))
    s= rbind(s,data.frame(iesname="hh_c04",name="school_start_age"))
    s= rbind(s,data.frame(iesname="hh_c05",name="in_school"))
    s= rbind(s,data.frame(iesname="hh_c07",name="highest_educ"))
    s= rbind(s,data.frame(iesname="hh_c08",name="school_leaving_year"))
    s= rbind(s,data.frame(iesname="hh_c09",name="current_educ_level"))
    s= rbind(s,data.frame(iesname="hh_c11",name="attending_school"))
    
    s= rbind(s,data.frame(iesname="hh_c12",name="schoolowner"))
    s= rbind(s,data.frame(iesname="hh_c13",name="is_boarding_school"))
    s= rbind(s,data.frame(iesname="hh_c14",name="schoolconveyance"))
    s= rbind(s,data.frame(iesname="hh_c15",name="schooltransporttime"))
    s= rbind(s,data.frame(iesname="hh_c16",name="is_meals_at_school"))
    s= rbind(s,data.frame(iesname="hh_c17",name="has_missedschool"))
    s= rbind(s,data.frame(iesname="hh_c19",name="textbook_status"))
    s= rbind(s,data.frame(iesname="hh_c20_1",name="homework_hours"))
    s= rbind(s,data.frame(iesname="hh_c20_2",name="homework_minutes"))
    
    s= rbind(s,data.frame(iesname="hh_c28_8",name="educexpense"))
    s= rbind(s,data.frame(iesname="hh_c29",name="has_adulteduc"))
    s= rbind(s,data.frame(iesname="hh_c30",name="adulteducmonths"))
    return(s)
  }
  
  
  get_ohs_secc_columns_lsms_2014<-function(){
    return(c("hhid","personid","is_ge5y","litlang","is_literate","highest_educ","schoolowner",
             "schoolconveyance","has_missedschool","educexpense","has_adulteduc","adulteducmonths",
             "attended_school","school_start_age","school_leaving_year","schoolconveyance","schooltransporttime"));
  }
  
  get_ohs_secc_fields_mapping_lsms_2014<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y4_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="indidy4",name="personid"))
    s= rbind(s,data.frame(iesname="hh_c01",name="is_ge5y"))
    s= rbind(s,data.frame(iesname="hh_c02",name="litlang"))
    s= rbind(s,data.frame(iesname="hh_c03",name="attended_school"))
    s= rbind(s,data.frame(iesname="hh_c04",name="school_start_age"))
    s= rbind(s,data.frame(iesname="hh_c05",name="in_school"))
    s= rbind(s,data.frame(iesname="hh_c07",name="highest_educ"))
    s= rbind(s,data.frame(iesname="hh_c08",name="school_leaving_year"))
    s= rbind(s,data.frame(iesname="hh_c09",name="current_educ_level"))
    s= rbind(s,data.frame(iesname="hh_c11",name="attending_school"))
    
    s= rbind(s,data.frame(iesname="hh_c12",name="schoolowner"))
    s= rbind(s,data.frame(iesname="hh_c13",name="is_boarding_school"))
    s= rbind(s,data.frame(iesname="hh_c14",name="schoolconveyance"))
    s= rbind(s,data.frame(iesname="hh_c15",name="schooltransporttime"))
    s= rbind(s,data.frame(iesname="hh_c16",name="is_meals_at_school"))
    s= rbind(s,data.frame(iesname="hh_c17",name="has_missedschool"))
    s= rbind(s,data.frame(iesname="hh_c19",name="textbook_status"))
    s= rbind(s,data.frame(iesname="hh_c20_1",name="homework_hours"))
    s= rbind(s,data.frame(iesname="hh_c20_2",name="homework_minutes"))
    
    s= rbind(s,data.frame(iesname="hh_c28_8",name="educexpense"))
    s= rbind(s,data.frame(iesname="hh_c29",name="has_adulteduc"))
    s= rbind(s,data.frame(iesname="hh_c30",name="adulteducmonths"))
    return(s)
  }
  
  
  ohs_seccb_columns_lsms<-function(year){
    if (year == 2008 || year==2010 || year == 2012)
    {
      return(c("facilitycode","accessibility","distance","region","district","ward","travelcost"))
    }
    if (year == 2014){
      return(c("facilitycode","accessibility","distance","clusterid","travelcost"))
    }
    stop (paste("seccb year not supported:",year))
  }
  
  ohs_seccb_mapping_lsms<-function(year){
    if (year == 2008){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="region",name="region"))
      s= rbind(s,data.frame(iesname="district",name="district"))
      s= rbind(s,data.frame(iesname="ward",name="ward"))
      s= rbind(s,data.frame(iesname="ea",name="ea"))
      s= rbind(s,data.frame(iesname="cb0",name="facilitycode"))
      s= rbind(s,data.frame(iesname="cb1",name="accessibility"))
      s= rbind(s,data.frame(iesname="cb2",name="distance"))
      s= rbind(s,data.frame(iesname="cb3",name="travelcost"))
      return(s)
    }
    if (year == 2010){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="id_01",name="region"))
      s= rbind(s,data.frame(iesname="id_02",name="district"))
      s= rbind(s,data.frame(iesname="id_03",name="ward"))
      s= rbind(s,data.frame(iesname="id_04",name="ea"))
      s= rbind(s,data.frame(iesname="cboa",name="facilitycode"))
      s= rbind(s,data.frame(iesname="cm_b01",name="accessibility"))
      s= rbind(s,data.frame(iesname="cm_b03",name="distance"))
      s= rbind(s,data.frame(iesname="cm_b02",name="travelcost"))
      return(s)
    } 
    if (year ==2012){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="id_01",name="region"))
      s= rbind(s,data.frame(iesname="id_02",name="district"))
      s= rbind(s,data.frame(iesname="id_03",name="ward"))
      s= rbind(s,data.frame(iesname="id_04",name="ea"))
      s= rbind(s,data.frame(iesname="cm_b0a",name="facilitycode"))
      s= rbind(s,data.frame(iesname="cm_b01",name="accessibility"))
      s= rbind(s,data.frame(iesname="cm_b03",name="distance"))
      s= rbind(s,data.frame(iesname="cm_b02",name="travelcost"))
      return(s)
    }
    if (year ==2014){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="y4_cluster",name="clusterid"))
      s= rbind(s,data.frame(iesname="cboa",name="facilitycode"))
      s= rbind(s,data.frame(iesname="cb1",name="accessibility"))
      s= rbind(s,data.frame(iesname="cb2",name="travelcost"))
      s= rbind(s,data.frame(iesname="cb3",name="distance"))
      
      return(s)
    }
    stop (paste("seccb year not supported:",year))
    
  }
  
  ohs_seccj_columns_lsms_2010<-function(){
    return(c("item","lwp","lwp_unit","price","region","district","ward","ea"))
  }
  
  ohs_seccj_mapping_lsms_2010<-function(){
    s = data.frame(iesname=NULL,name=NULL,stringsAsFactors = FALSE)
    s= rbind(s,data.frame(iesname="id_01",name="region"))
    s= rbind(s,data.frame(iesname="id_02",name="district"))
    s= rbind(s,data.frame(iesname="id_03",name="ward"))
    s= rbind(s,data.frame(iesname="id_04",name="ea"))
    s= rbind(s,data.frame(iesname="itemid",name="item"))
    s= rbind(s,data.frame(iesname="cm_j01a",name="lwp_unit"))
    s= rbind(s,data.frame(iesname="cm_j01b",name="lwp"))
    s= rbind(s,data.frame(iesname="cm_j01c",name="price"))
    return(s)
  }
  
  ohs_seccj_columns_lsms_2012<-function(){
    return(c("item","lwp","lwp_unit","price","region","district","ward","ea"))
  }
  ohs_seccj_mapping_lsms_2012<-function(){
    s = data.frame(iesname=NULL,name=NULL,stringsAsFactors = FALSE)
    s= rbind(s,data.frame(iesname="id_01",name="region"))
    s= rbind(s,data.frame(iesname="id_02",name="district"))
    s= rbind(s,data.frame(iesname="id_03",name="ward"))
    s= rbind(s,data.frame(iesname="id_04",name="ea"))
    s= rbind(s,data.frame(iesname="itemid",name="item"))
    s= rbind(s,data.frame(iesname="cm_f06meas",name="lwp_unit"))
    s= rbind(s,data.frame(iesname="cm_f06wght",name="lwp"))
    s= rbind(s,data.frame(iesname="cm_f06pri",name="price"))
    return(s)
  }
  
  
  ohs_seccf_columns_lsms_2014 <-function(){
    return(c("item","lwp1","lwp_unit1","lwp2","lwp_unit2","price1","price2","clusterid"))
  }
  
  ohs_seccf_mapping_lsms_2014<-function(){
    s = data.frame(iesname=NULL,name=NULL,stringsAsFactors = FALSE)
    s= rbind(s,data.frame(iesname="y4_cluster",name="clusterid"))
    s= rbind(s,data.frame(iesname="itemid",name="item"))
    s= rbind(s,data.frame(iesname="cm_f061",name="lwp_unit1"))
    s= rbind(s,data.frame(iesname="cm_f062",name="lwp1"))
    s= rbind(s,data.frame(iesname="cm_f063",name="price1"))
    s= rbind(s,data.frame(iesname="cm_f064",name="lwp2"))
    s= rbind(s,data.frame(iesname="cm_f065",name="lwp_unit2"))
    s= rbind(s,data.frame(iesname="cm_f066",name="price2"))
    return(s)
  }

    
  ohs_seccj_columns_lsms_2008<-function(){
    return(c("item","lwp","lwp_unit","price","region","district","ward","ea"))
  }
  
  ohs_seccj_mapping_lsms_2008<-function(){
    s = data.frame(iesname=NULL,name=NULL,stringsAsFactors = FALSE)
    s= rbind(s,data.frame(iesname="region",name="region"))
    s= rbind(s,data.frame(iesname="district",name="district"))
    s= rbind(s,data.frame(iesname="ward",name="ward"))
    s= rbind(s,data.frame(iesname="ea",name="ea"))
    s= rbind(s,data.frame(iesname="itemid",name="item"))
    s= rbind(s,data.frame(iesname="cj06meas",name="lwp_unit"))
    s= rbind(s,data.frame(iesname="cj06wght",name="lwp"))
    s= rbind(s,data.frame(iesname="cj06pri",name="price"))
    return(s)
  }
  
  decode_clusterid <- function (x){
    x$region <- as.integer(sapply(x$clusterid,function(x) { strsplit(x,"-")[[1]][1] }))
    x$district <- as.integer(sapply(x$clusterid,function(x) { strsplit(x,"-")[[1]][2] }))
    x$ward <- as.integer(sapply(x$clusterid,function(x) { strsplit(x,"-")[[1]][3] }))
    x$village <- as.integer(sapply(x$clusterid,function(x) { strsplit(x,"-")[[1]][4] }))
    x$ea <- as.integer(sapply(x$clusterid,function(x) { strsplit(x,"-")[[1]][5] }))
    return(x)
  }
  
  ohs_seca_columns_lsms<-function(year){
    if (year == 2010 || year == 2008 || year == 2014){
      return(c("hhid","region","district","ward","ea","isrural"))
    } 
    if (year == 2012) {
      return (c("hhid","region","district","ward","ea","isrural","hhid2010"))
    }
    stop (paste("sec a column mapping not found for year:", year))
  }
  
  ohs_seca_mapping_lsms_2010<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="region",name="region"))
    s= rbind(s,data.frame(iesname="district",name="district"))
    s= rbind(s,data.frame(iesname="ward",name="ward"))
    s= rbind(s,data.frame(iesname="ea",name="ea"))
    s= rbind(s,data.frame(iesname="y2_rural",name="isrural"))
    return(s)
  }
  
  ohs_seca_mapping_lsms_2012<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y3_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="hh_a01_1",name="region"))
    s= rbind(s,data.frame(iesname="hh_a02_1",name="district"))
    s= rbind(s,data.frame(iesname="hh_a03_1",name="ward"))
    s= rbind(s,data.frame(iesname="hh_a04_1",name="ea"))
    s= rbind(s,data.frame(iesname="y3_rural",name="isrural"))
    s= rbind(s,data.frame(iesname="hh_a09",name="hhid2010"))
    return(s)
  }

  ohs_seca_mapping_lsms_2014<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y4_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="hh_a01_1",name="region"))
    s= rbind(s,data.frame(iesname="hh_a02_1",name="district"))
    s= rbind(s,data.frame(iesname="hh_a03_1",name="ward"))
    s= rbind(s,data.frame(iesname="hh_a04_1",name="ea"))
    s= rbind(s,data.frame(iesname="y4_rural",name="isrural"))
    return(s)
  }
  
  ohs_seca_mapping_lsms_2008<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="region",name="region"))
    s= rbind(s,data.frame(iesname="district",name="district"))
    s= rbind(s,data.frame(iesname="ward",name="ward"))
    s= rbind(s,data.frame(iesname="ea",name="ea"))
    s= rbind(s,data.frame(iesname="rural",name="isrural"))
    return(s)
  }
  
  get_ohs_secc_columns_lsms_2008<-function(){
    return(c("hhid","personid","is_ge5y","highest_educ","schoolowner",
             "has_missedschool","educexpense"))
  }
  
  get_ohs_secc_fields_mapping_lsms_2008<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="sbmemno",name="personid"))
    s= rbind(s,data.frame(iesname="scq1",name="is_ge5y"))
    #s= rbind(s,data.frame(iesname="hh_c02",name="litlang"))
    #s= rbind(s,data.frame(iesname="hh_c01",name="is_literate"))
    
    s= rbind(s,data.frame(iesname="scq6",name="highest_educ"))
    s= rbind(s,data.frame(iesname="scq10",name="schoolowner"))
    #s= rbind(s,data.frame(iesname="",name="schoolconveyance"))
    s= rbind(s,data.frame(iesname="scq12",name="has_missedschool"))
    s= rbind(s,data.frame(iesname="scq14_tot",name="educexpense"))
    return(s)
  }
  
  
  get_lsms_secj_info_columns_2008<-function(){
    return(c("hhid","housingstatus","houserent","roomsnum_primary","roomsnum_secondary","wallsmaterial","roofmaterial","floormaterial","toilet","cookingfuel","lightingfuel","dryseasonwatersource"))
  }
  
  get_lsms_secj_fields_mapping_2008<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="sjq1",name="housingstatus")) # 1- owner occupied, 2- EMPLOYER PROVIDED - SUBSIDIZED, 3-EMPLOYER PROVIDED - FREE, 4- RENTED, 5- FREE, 6-NOMADS 
    s= rbind(s,data.frame(iesname="sjq2",name="houserent"))
    s= rbind(s,data.frame(iesname="sjq3_1",name="roomsnum_primary"))
    s= rbind(s,data.frame(iesname="sjq3_2",name="roomsnum_secondary"))
    s= rbind(s,data.frame(iesname="sjq4",name="wallsmaterial"))
    s= rbind(s,data.frame(iesname="sjq5",name="roofmaterial"))
    s= rbind(s,data.frame(iesname="sjq6",name="floormaterial"))
    s= rbind(s,data.frame(iesname="sjq16",name="toilet"))
    s= rbind(s,data.frame(iesname="sjq17_1",name="cookingfuel"))
    s= rbind(s,data.frame(iesname="sjq18",name="lightingfuel"))
    s= rbind(s,data.frame(iesname="sjq8",name="dryseasonwatersource"))
    return(s)
  }
  
  
  get_lsms_seck_info_columns_2012<-function(){
    return(c("hhid","item","is_consumed","cost"))
  }
  
  get_lsms_seck_fields_mapping_2012<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y3_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="hh_k01",name="is_consumed"))
    s= rbind(s,data.frame(iesname="itemcode",name="item"))
    s= rbind(s,data.frame(iesname="hh_k02",name="cost"))
    return(s)
  }
  
  
  get_diary_secn_columns_lsms_2010<-function(){
    return(c("hhid","itemcode","number"))
  }
  
  get_diary_secn_columns_lsms_2008<-function(){
    return(c("hhid2008","itemcode","number"))
  }
  
  get_diary_secn_fields_mapping_lsms_2010<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="itemcode",name="itemcode"))
    s= rbind(s,data.frame(iesname="hh_n01_2",name="number"))
    return(s)
  }
  # file corresponds to secn of 2010
  get_diary_secn_columns_lsms_2012<-function(){
    return(c("hhid","itemcode","number","age","cost","mtm"))
  }
  
  # file corresponds to secn of 2010
  get_diary_secn_fields_mapping_lsms_2012<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y3_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="itemcode",name="itemcode"))
    s= rbind(s,data.frame(iesname="hh_m01",name="number"))
    s= rbind(s,data.frame(iesname="hh_m02",name="age"))
    s= rbind(s,data.frame(iesname="hh_m03",name="cost"))
    s= rbind(s,data.frame(iesname="hh_m04",name="mtm"))
    return(s)
  }
  
  get_diary_secn_fields_mapping_lsms_2008<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="hhid",name="hhid2008"))
    s= rbind(s,data.frame(iesname="sncode",name="itemcode"))
    s= rbind(s,data.frame(iesname="snq1",name="number"))
    return(s)
    
  }
  area_code<-function(df,field_array)
  {
    areacode=NULL
    for (field in field_array){
      if (class(df[,field])!="integer" && class(df[,field])!="numeric"){
        stop("field(",field,") is not integer")
      }
      a<-formatC(format="d",flag="0",x=df[,field],width=ceiling(log10(max(df[,field]))))
      areacode<-paste(areacode,a,sep="")
      
    }
    return(as.integer(areacode))
    #return(areacode)
  }
  
  analyse_cj<-function(sl){
    cjdat<-read.dta('../lsms/TZNPS2COMDTA/COMSEC_CJ.dta',convert.factors = FALSE) 
    
    cj <- get_translated_frame(dat=cjdat, names=ohs_seccj_columns_lsms_2010(), m=ohs_seccj_mapping_lsms_2010())
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
  ohs_info_columns_lsms_2010<-function(){
    # hhid, age, gender, educ, race, hsize, areatype, 
    # income file: income
    return(c("hhid", "gender", "personid","YOB", "household_status", "inhouse_consumer",
             "inhouse_days_in_month", "inhouse_resident", "outhouse_days_in_year", 
             "occupation", "fathers_educ", "mothers_educ", "married", "spouse_resident","years_community",
             "outhouse_spouses", "source_migration_name", "source_migration_code", 
             "reason_migration", "birthdistrict", "birthregion"))
  }
  
  ohs_info_columns_lsms_2012<-function(){
    # hhid, age, gender, educ, race, hsize, areatype, 
    # income file: income
    return(c("hhid","gender", "personid","YOB", "household_status", "inhouse_consumer",
             "inhouse_days_in_month", "inhouse_resident", "outhouse_days_in_year", 
             "occupation", "fathers_educ", "mothers_educ", "married", "spouse_resident","years_community",
             "outhouse_spouses", "source_migration_name", "source_migration_code", 
             "reason_migration", "birthdistrict", "birthregion"))
  }
  
  ohs_info_columns_lsms_2014<-function(){
    # hhid, age, gender, educ, race, hsize, areatype, 
    # income file: income
    return(c("hhid","gender", "personid","age", "household_status", "inhouse_consumer",
             "inhouse_days_in_month", "inhouse_resident", "outhouse_days_in_year", 
             "occupation", "fathers_educ", "mothers_educ", "married", "spouse_resident","years_community",
             "outhouse_spouses", "source_migration_name", "source_migration_code", 
             "reason_migration", "birthdistrict", "birthregion"))
  }
  
    get_ohs_info_columns<-function(dataset,year){
    
    if (dataset == "us_cex"){
      if (year ==2004 || year ==2009|| year == 2014){
        return(ohs_info_columns_us_cex_2004());
      }
      
      stop(paste("Year : ",year," not found for us_cex"))
    }
    stop(paste("Could not find ohs info columns for dataset:",dataset))
  }
  
  get_diary_info_columns<-function(dataset,year){
    
    if(dataset== "us_cex"){
      if (year == 2004 || year == 2009|| year == 2014){
        return(diary_info_columns_us_cex_2004())
      }
      stop(paste("Could not find diary info columns for year:",year))
    }
    stop(paste("Unknown dataset:",dataset))
  }
  
  
  diary_info_columns_lsms_2010<-function(){
    return(c("hhid","item","lwp_unit", "lwp", "cost", "own_unit", "own", "gift_unit", "gift"))
  }
  
  hh_mapping_lsms_2010 <-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="itemcode",name="item"))
    s= rbind(s,data.frame(iesname="hh_k03_1",name="lwp_unit"))
    s= rbind(s,data.frame(iesname="hh_k03_2",name="lwp"))
    s= rbind(s,data.frame(iesname="hh_k04",name="cost"))
    s= rbind(s,data.frame(iesname="hh_k05_1",name="own_unit"))
    s= rbind(s,data.frame(iesname="hh_k05_2",name="own"))
    s= rbind(s,data.frame(iesname="hh_k06_1",name="gift_unit"))
    s= rbind(s,data.frame(iesname="hh_k06_2",name="gift"))
    return(s)
  }
  
  ohs_mapping_lsms_2010<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="indidy2",name="personid"))
    s= rbind(s,data.frame(iesname="hh_b02",name="gender"))
    s= rbind(s,data.frame(iesname="hh_b03_1",name="YOB"))
    s= rbind(s,data.frame(iesname="hh_b05",name="household_status"))
    s= rbind(s,data.frame(iesname="hh_b07",name="inhouse_consumer"))
    s= rbind(s,data.frame(iesname="hh_b08",name="inhouse_days_in_month"))
    s= rbind(s,data.frame(iesname="hh_b09_1",name="inhouse_resident"))
    s= rbind(s,data.frame(iesname="hh_b10",name="outhouse_days_in_year"))
    s= rbind(s,data.frame(iesname="hh_b11",name="occupation"))
    s= rbind(s,data.frame(iesname="hh_b14",name="fathers_educ"))
    s= rbind(s,data.frame(iesname="hh_b17",name="mothers_educ"))
    s= rbind(s,data.frame(iesname="hh_b19",name="married"))
    s= rbind(s,data.frame(iesname="hh_b21",name="spouse_resident")) 
    s= rbind(s,data.frame(iesname="hh_b24",name="outhouse_spouses"))
    
    s= rbind(s,data.frame(iesname="hh_b25",name="years_community"))
    
    s= rbind(s,data.frame(iesname="hh_b26_2",name="source_migration_name"))
    s= rbind(s,data.frame(iesname="hh_b26_3",name="source_migration_code"))
    s= rbind(s,data.frame(iesname="hh_b27",name="reason_migration"))
    s= rbind(s,data.frame(iesname="hh_b28_2",name="birthregion"))
    s= rbind(s,data.frame(iesname="hh_b28_3",name="birthdistrict"))
    return(s)
  }
  
  
  get_lsms_sece1_columns_2010<-function(){
    return(c("hhid", "personid", "is_ge5", "mainoccup", "is_wageworker", "employertype", "num_colleagues", 
             "lastpayment_unit", "lastpayment", "workweekhour","workyearmonths", "workyearmonthweeks",
             "workyearweekhours","has_lastpayment_other", "lastpayment_other_unit", 
             "lastpayment_other", "has_secjob", "employertype_secjob", "num_colleagues_secjob", "has_secjobwages",
             "workweekhour_secjob","workyearmonths_secjob", "workyearmonthweeks_secjob","workyearweekhours_secjob",
             "lastpayment_secjobwage_unit", "lastpayment_secjobwage", "has_secjobwages_other", "lastpayment_secjobwage_other_unit",
             "lastpayment_secjobwage_other", "has_selfemployment_week", "has_selfemployment_year", "selfemploymenttype",
             "selfemploymentstockvalue", "selfemploymentincome_unit", "selfemploymentincome","selfemploymentyearmonths",
             "selfemploymentyearmonthincome"))
  }
  
  get_lsms_sece2_columns_2010<-function(){
    return(c("hhid", "personid","selfemploymenttype",
             "selfemploymentstockvalue", "selfemploymentincome_unit", "selfemploymentincome","has_selfemployment_year",
             "selfemploymentyearmonths",
             "selfemploymentyearmonthincome"))
  }
  
  get_lsms_sece_fields_mapping_2010<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y2_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="indidy2",name="personid"))
    s= rbind(s,data.frame(iesname="hh_e01",name="is_ge5"))
    s= rbind(s,data.frame(iesname="hh_e06",name="mainoccup"))
    s= rbind(s,data.frame(iesname="hh_e13",name="is_wageworker"))
    s= rbind(s,data.frame(iesname="hh_e15",name="employertype"))
    s= rbind(s,data.frame(iesname="hh_e18",name="num_colleagues"))
    s= rbind(s,data.frame(iesname="hh_e22_2",name="lastpayment_unit"))
    s= rbind(s,data.frame(iesname="hh_e22_1",name="lastpayment"))
    s= rbind(s,data.frame(iesname="hh_e23",name="has_lastpayment_other"))
    s= rbind(s,data.frame(iesname="hh_e24_2",name="lastpayment_other_unit"))
    s= rbind(s,data.frame(iesname="hh_e24_1",name="lastpayment_other"))
    s= rbind(s,data.frame(iesname="hh_e25",name="workweekhours"))
    s= rbind(s,data.frame(iesname="hh_e26",name="workyearmonths"))
    s= rbind(s,data.frame(iesname="hh_e27",name="workyearmonthweeks"))
    s= rbind(s,data.frame(iesname="hh_e28",name="workyearweekhours"))
    s= rbind(s,data.frame(iesname="hh_e29",name="has_secjob"))
    s= rbind(s,data.frame(iesname="hh_e30",name="employertype_secjob"))
    s= rbind(s,data.frame(iesname="hh_e33",name="num_colleagues_secjob"))
    s= rbind(s,data.frame(iesname="hh_e35",name="has_secjobwages"))
    s= rbind(s,data.frame(iesname="hh_e37_2",name="lastpayment_secjobwage_unit"))
    s= rbind(s,data.frame(iesname="hh_e37_1",name="lastpayment_secjobwage"))
    s= rbind(s,data.frame(iesname="hh_e38",name="has_secjobwages_other"))
    s= rbind(s,data.frame(iesname="hh_e39_2",name="lastpayment_secjobwage_other_unit"))
    s= rbind(s,data.frame(iesname="hh_e39_1",name="lastpayment_secjobwage_other"))
    
    s= rbind(s,data.frame(iesname="hh_e40",name="workweekhours_secjob"))
    s= rbind(s,data.frame(iesname="hh_e41",name="workyearmonths_secjob"))
    s= rbind(s,data.frame(iesname="hh_e42",name="workyearmonthweeks_secjob"))
    s= rbind(s,data.frame(iesname="hh_e43",name="workyearweekhours_secjob"))
    
    s= rbind(s,data.frame(iesname="hh_e51",name="has_selfemployment_week"))
    s= rbind(s,data.frame(iesname="hh_e52",name="has_selfemployment_year"))
    s= rbind(s,data.frame(iesname="hh_e53_2",name="selfemploymenttype"))
    s= rbind(s,data.frame(iesname="hh_e61",name="selfemploymentstockvalue"))
    s= rbind(s,data.frame(iesname="hh_e65_1",name="selfemploymentincome_unit"))
    s= rbind(s,data.frame(iesname="hh_e65_2",name="selfemploymentincome"))
    
    s= rbind(s,data.frame(iesname="hh_e70",name="selfemploymentyearmonths"))
    s= rbind(s,data.frame(iesname="hh_e71",name="selfemploymentyearmonthincome"))
    return(s)
  }
  
  diary_info_columns_lsms_2012<-function(){
    return(c("hhid","item","lwp_unit", "lwp", "cost", "own_unit", "own", "gift_unit", "gift"))
  }
  
  hh_mapping_lsms_2012 <-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y3_hhid",name="hhid"))
    #s= rbind(s,data.frame(iesname="y2_hhid",name="y2_hhid"))
    s= rbind(s,data.frame(iesname="itemcode",name="item"))
    s= rbind(s,data.frame(iesname="hh_j03_1",name="lwp_unit"))
    s= rbind(s,data.frame(iesname="hh_j03_2",name="lwp"))
    s= rbind(s,data.frame(iesname="hh_j04",name="cost"))
    s= rbind(s,data.frame(iesname="hh_j05_1",name="own_unit"))
    s= rbind(s,data.frame(iesname="hh_j05_2",name="own"))
    s= rbind(s,data.frame(iesname="hh_j06_1",name="gift_unit"))
    s= rbind(s,data.frame(iesname="hh_j06_2",name="gift"))
    return(s)
  }
  
  ohs_mapping_lsms_2014 <- function(){
      s = data.frame(iesname=NULL,name=NULL)
      s= rbind(s,data.frame(iesname="y4_hhid",name="hhid"))
      s= rbind(s,data.frame(iesname="indidy4",name="personid"))
      s= rbind(s,data.frame(iesname="hh_b02",name="gender"))
      s= rbind(s,data.frame(iesname="hh_b04",name="age"))
      s= rbind(s,data.frame(iesname="hh_b05",name="household_status"))
      s= rbind(s,data.frame(iesname="hh_b07",name="inhouse_consumer"))
      s= rbind(s,data.frame(iesname="hh_b08",name="inhouse_days_in_month"))
      s= rbind(s,data.frame(iesname="hh_b09_1",name="inhouse_resident"))
      s= rbind(s,data.frame(iesname="hh_b10",name="outhouse_days_in_year"))
      s= rbind(s,data.frame(iesname="hh_b11",name="occupation"))
      s= rbind(s,data.frame(iesname="hh_b14",name="fathers_educ"))
      s= rbind(s,data.frame(iesname="hh_b17",name="mothers_educ"))
      s= rbind(s,data.frame(iesname="hh_b19",name="married"))
      s= rbind(s,data.frame(iesname="hh_b23_1",name="spouse_resident")) 
      s= rbind(s,data.frame(iesname="hh_b25",name="outhouse_spouses"))
      
      s= rbind(s,data.frame(iesname="hh_b26",name="years_community"))
      
      s= rbind(s,data.frame(iesname="hh_b27_2",name="source_migration_region"))
      s= rbind(s,data.frame(iesname="hh_b27_3",name="source_migration_district"))
      s= rbind(s,data.frame(iesname="hh_b28",name="reason_migration"))
      s= rbind(s,data.frame(iesname="hh_b29_2",name="birthregion"))
      s= rbind(s,data.frame(iesname="hh_b29_3",name="birthdistrict"))
      return(s)
    }
  
  
  ohs_mapping_lsms_2012<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y3_hhid",name="hhid"))
    #s= rbind(s,data.frame(iesname="y2_hhid",name="y2_hhid"))
    s= rbind(s,data.frame(iesname="indidy3",name="personid"))
    s= rbind(s,data.frame(iesname="hh_b02",name="gender"))
    s= rbind(s,data.frame(iesname="hh_b03_1",name="YOB"))
    s= rbind(s,data.frame(iesname="hh_b05",name="household_status"))
    s= rbind(s,data.frame(iesname="hh_b07",name="inhouse_consumer"))
    s= rbind(s,data.frame(iesname="hh_b08",name="inhouse_days_in_month"))
    s= rbind(s,data.frame(iesname="hh_b09_1",name="inhouse_resident"))
    s= rbind(s,data.frame(iesname="hh_b10",name="outhouse_days_in_year"))
    s= rbind(s,data.frame(iesname="hh_b11",name="occupation"))
    s= rbind(s,data.frame(iesname="hh_b14",name="fathers_educ"))
    s= rbind(s,data.frame(iesname="hh_b17",name="mothers_educ"))
    s= rbind(s,data.frame(iesname="hh_b19",name="married"))
    s= rbind(s,data.frame(iesname="hh_b23_1",name="spouse_resident")) 
    s= rbind(s,data.frame(iesname="hh_b25",name="outhouse_spouses"))
    
    s= rbind(s,data.frame(iesname="hh_b26",name="years_community"))
    
    s= rbind(s,data.frame(iesname="hh_b27_2",name="source_migration_region"))
    s= rbind(s,data.frame(iesname="hh_b27_3",name="source_migration_district"))
    s= rbind(s,data.frame(iesname="hh_b28",name="reason_migration"))
    s= rbind(s,data.frame(iesname="hh_b29_2",name="birthregion"))
    s= rbind(s,data.frame(iesname="hh_b29_3",name="birthdistrict"))
    return(s)
  }
  
  diary_info_columns_lsms_2014<-function(){
    return(c("hhid","item","lwp_unit", "lwp", "cost", "own_unit", "own", "gift_unit", "gift"))
  }
  
  hh_mapping_lsms_2014 <-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y4_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="itemcode",name="item"))
    s= rbind(s,data.frame(iesname="hh_j03_1",name="lwp_unit"))
    s= rbind(s,data.frame(iesname="hh_j03_2",name="lwp"))
    s= rbind(s,data.frame(iesname="hh_j04",name="cost"))
    s= rbind(s,data.frame(iesname="hh_j05_1",name="own_unit"))
    s= rbind(s,data.frame(iesname="hh_j05_2",name="own"))
    s= rbind(s,data.frame(iesname="hh_j06_1",name="gift_unit"))
    s= rbind(s,data.frame(iesname="hh_j06_2",name="gift"))
    return(s)
  }
  
  
  get_lsms_seck_info_columns_2014<-function(){
    return(c("hhid","item","is_consumed","cost"))
  }
  
  get_lsms_seck_fields_mapping_2014<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="y4_hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="hh_k01",name="is_consumed"))
    s= rbind(s,data.frame(iesname="itemcode",name="item"))
    s= rbind(s,data.frame(iesname="hh_k02",name="cost"))
    return(s)
  }
  
  diary_info_columns_lsms_2008<-function(){
    return(c("hhid","item","lwp_unit", "lwp", "cost", "own_unit", "own", "gift_unit", "gift"))
  }
  
  hh_mapping_lsms_2008 <-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="skcode",name="item"))
    s= rbind(s,data.frame(iesname="skq3_meas",name="lwp_unit"))
    s= rbind(s,data.frame(iesname="skq3_amount",name="lwp"))
    s= rbind(s,data.frame(iesname="skq4",name="cost"))
    s= rbind(s,data.frame(iesname="skq5_meas",name="own_unit"))
    s= rbind(s,data.frame(iesname="skq5_amount",name="own"))
    s= rbind(s,data.frame(iesname="skq6_meas",name="gift_unit"))
    s= rbind(s,data.frame(iesname="skq6_amount",name="gift"))
    return(s)
  }
  
  get_lsms_secl_info_columns_2008<-function(){
    return(c("hhid","item","is_consumed","cost"))
  }
  
  get_lsms_secl_fields_mapping_2008<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="slq1",name="is_consumed"))
    s= rbind(s,data.frame(iesname="slcode",name="item"))
    s= rbind(s,data.frame(iesname="slq2",name="cost"))
    return(s)
  }
  
  ohs_info_columns_lsms_2008<-function(){
    # hhid, age, gender, educ, race, hsize, areatype, 
    # income file: income
    return(c("hhid", "gender", "personid","YOB", "household_status", "inhouse_consumer",
             "inhouse_resident", "outhouse_days_in_year", 
             "occupation", "fathers_educ", "mothers_educ", "married", "spouse_resident","years_community",
             "outhouse_spouses", "source_migration_code", 
             "reason_migration", "birthdistrict"))
  }
  
  ohs_mapping_lsms_2008<-function(){
    s = data.frame(iesname=NULL,name=NULL)
    s= rbind(s,data.frame(iesname="hhid",name="hhid"))
    s= rbind(s,data.frame(iesname="sbmemno",name="personid"))
    s= rbind(s,data.frame(iesname="sbq2",name="gender"))
    s= rbind(s,data.frame(iesname="sbq3yr",name="YOB"))
    s= rbind(s,data.frame(iesname="sbq5",name="household_status"))
    s= rbind(s,data.frame(iesname="sbq7",name="inhouse_consumer"))
    #s= rbind(s,data.frame(iesname="hh_b08",name="inhouse_days_in_month"))
    s= rbind(s,data.frame(iesname="sbq8",name="inhouse_resident"))
    s= rbind(s,data.frame(iesname="sbq9",name="outhouse_days_in_year"))
    s= rbind(s,data.frame(iesname="sbq10",name="occupation"))
    s= rbind(s,data.frame(iesname="sbq13",name="fathers_educ"))
    s= rbind(s,data.frame(iesname="sbq16",name="mothers_educ"))
    s= rbind(s,data.frame(iesname="sbq18",name="married"))
    s= rbind(s,data.frame(iesname="sbq20",name="spouse_resident")) 
    s= rbind(s,data.frame(iesname="sbq23",name="outhouse_spouses"))
    
    s= rbind(s,data.frame(iesname="sbq24",name="years_community"))
    
    #s= rbind(s,data.frame(iesname="",name="source_migration_name"))
    s= rbind(s,data.frame(iesname="sbq25district",name="source_migration_code"))
    s= rbind(s,data.frame(iesname="sbq26",name="reason_migration"))
    #s= rbind(s,data.frame(iesname="",name="birthregion"))
    s= rbind(s,data.frame(iesname="sbq27district",name="birthdistrict"))
    return(s)
  }
  
  
  computeYearValues<-function(dat,
                              unit_field,
                              quantity_field,
                              workyearweekhours_field,
                              workyearmonthweeks_field,
                              workyearmonths_field,
                              output_field)
  {
    #* computeYearValues ((
    ufr <- range(dat[!is.na(dat[,unit_field]),][,unit_field])
    if (ufr[1]<1 || ufr[2]>8){
      stop("unit_field range not supported")
    }
    if (length(grep(output_field,colnames(dat),fixed=TRUE))>0){
      stop("column yearly_pay already present in data-frame")
    }
    
    #* pay frequency can be given in hours, days, weeks, months, fortnights, months, quarter, half year or year
    # last_payment_unit HOUR(1) DAY(2)  WEEK(3) FORTNIGHT(4) MONTH(5) QUATOR(6) HALF YEAR(7) YEAR(8)
    #* if (pay is per hours) then we use number of hours worked per week and multiply it with number of 
    #* weeks worked per month and further multiply the product with number of months worked per year
    h<-dat[!is.na(dat[,unit_field]),]
    h<-h[h[,unit_field]==1 ,]
    total_hour_workers<- dim(h)[1]
    if (total_hour_workers>0){
      print(paste("Total number of hour-wage-workers:",total_hour_workers))
      h<-h[!is.na(h[,workyearweekhours_field]),]
      h<-h[!is.na(h[,workyearmonthweeks_field]),] 
      h<-h[!is.na(h[,workyearmonths_field]),]
      total_hour_workers_considered<- dim(h)[1]
      print(paste("Number of hour-wage-workers ignored because of incomplete data:",total_hour_workers-total_hour_workers_considered))
      factor_hour = h[,workyearweekhours_field] * h[,workyearmonthweeks_field]* h[,workyearmonths_field] 
      h[,output_field] <-factor_hour*h[,quantity_field]
    } else {
      h<-NULL
    }
    #if (pay is per day )
    # assuming a 10 hour work day
    #* if pay is per day, then we assume a 10 hour working day and obtain the effective number of days per 
    #* week (based on the number of hours worked per week) and then multiply the number of days per week with 
    #* the number of weeks worked per month in th year - multiplying this product further with the number
    #* of months worked in an year
    d<-dat[!is.na(dat[,unit_field]),]
    d<-d[d[,unit_field]==2 ,]
    total_day_workers<- dim(d)[1]
    if (total_day_workers>0){
      print(paste("Total number of day-wage-workers:",total_day_workers))
      d<-d[!is.na(d[,workyearweekhours_field]),]
      d<-d[!is.na(d[,workyearmonthweeks_field]),]
      d<-d[!is.na(d[,workyearmonths_field]),]
      
      total_day_workers_considered<- dim(d)[1]
      print(paste("Number of day-wage-workers ignored because of incomplete data:",total_day_workers-total_day_workers_considered))
      factor_day = (d[,workyearweekhours_field]/10)*d[,workyearmonthweeks_field]*d[,workyearmonths_field]
      d[,output_field] <-factor_day*d[,quantity_field]
    } else {
      d<-NULL
    }
    #if (pay is per week )
    w<-dat[!is.na(dat[,unit_field]),]
    w<-w[w[,unit_field]==3,]
    total_week_workers<- dim(w)[1]
    if(total_week_workers>0){
      #* if pay is per week, the number then we multiply weeks worked per month into number of 
      #* months worked per year
      print(paste("Total number of week-wage-workers:",total_week_workers))
      w<-w[!is.na(w[,workyearmonths_field]) & !is.na(w[,workyearmonthweeks_field]),]
      total_week_workers_considered<- dim(w)[1]
      print(paste("Number of week-wage-workers ignored because of incomplete data:",total_week_workers-total_week_workers_considered))
      factor_week = w[,workyearmonthweeks_field] * w[,workyearmonths_field]
      w[,output_field] <-factor_week*w[,quantity_field]
    } else {
      w<-NULL
    }
    #if (pay is per fortnight )
    f<-dat[!is.na(dat[,unit_field]),]
    f<-f[f[,unit_field]==4,]
    total_fortnight_workers<- dim(f)[1]
    if (total_fortnight_workers>0){
      #* if pay is in fortnights, then use 2* the number of months worked in an year to calculate
      #* the total pay over the year
      print(paste("Total number of fortnight-wage-workers:",total_fortnight_workers))
      f<-f[!is.na(f[,workyearmonths_field]),]
      total_fortnight_workers_considered<- dim(f)[1]
      print(paste("Number of fortnight-wage-workers ignored because of incomplete data:",total_fortnight_workers-total_fortnight_workers_considered))
      factor_fortnight = f[,workyearmonths_field]*2
      f[,output_field] <-factor_fortnight*f[,quantity_field]
    } else {
      f<-NULL
    }
    #if (pay is per month )
    m<-dat[!is.na(dat[,unit_field]),]
    m<-m[m[,unit_field]==5 ,]
    total_month_workers<- dim(m)[1]
    if(total_month_workers>0){
      #* if the pay is per month, then the multiplication factor is just the number of months worked
      #* per year
      print(paste("Total number of month-wage-workers:",total_month_workers))
      m<-m[!is.na(m[,workyearmonths_field]),]
      total_month_workers_considered<- dim(m)[1]
      print(paste("Number of month-wage-workers ignored because of incomplete data:",total_month_workers-total_month_workers_considered))
      factor_month = m[,workyearmonths_field]
      m[,output_field] <-factor_month*m[,quantity_field]
    }else{
      m<-NULL
    }
    #if (pay i quartor)
    #* if the pay is per quarter, then we infer the effective number of quarters from the number of 
    #* months worked per year (number_of_months/3) and multiply with the number of 
    #* months worked per year
    q<-dat[!is.na(dat[,unit_field]),]
    q<-q[q[,unit_field]==5,]
    total_quarter_workers<- dim(q)[1]
    if(total_quarter_workers>0){
      print(paste("Total number of quarter-wage-workers:",total_quarter_workers))
      q<-q[!is.na(m[,workyearmonths_field]),]
      total_quarter_workers_considered<- dim(q)[1]
      print(paste("Number of quarter-wage-workers ignored because of incomplete data:",total_quarter_workers-total_quarter_workers_considered))
      factor_quarter = q[,workyearmonths_field]/3
      q[,output_field] <-factor_quarter*q[,quantity_field]
    }else{
      q<-NULL
    }
    # if pay is year 
    # factor = 1
    y<-dat[!is.na(dat[,unit_field]),]
    y<-y[y[,unit_field]==6,]
    total_year_workers<-dim(y)[1]
    if(total_year_workers>0){
      print(paste("Total number of yearly-wage-workers:",total_year_workers))
      qy <-y[,quantity_field]
      y[,output_field] <-qy
    } else {
      y<-NULL
    }
    
    hd<-rbind(h,d,stringsAsFactors=FALSE)
    hdw<-rbind(hd,w,stringsAsFactors=FALSE)
    hdwf<-rbind(hdw,f,stringsAsFactors=FALSE)
    hdwfm<-rbind(hdwf,m,stringsAsFactors=FALSE)
    hdwfmq<-rbind(hdwfm,q,stringsAsFactors=FALSE)
    hdwfmqy<-rbind(hdwfmq,y,stringsAsFactors=FALSE)
    return(hdwfmqy)
    #* ))
  }
  
  computeLsmsSelfemployedValues<-function(dat,has_selfemployment_year_field,selfemploymentyearmonths_field,selfemploymentyearmonthincome_field)
  {
    #* computeLsmsSelfemployedValues ((
    i11<-dat[!is.na(dat[,has_selfemployment_year_field]),]
    i1_selfemployed<-i11[as.integer(i11[,has_selfemployment_year_field])==1,]
    total_self_employed<-dim(i1_selfemployed)[1]
    print(paste("Number of self-employed-workers:",total_self_employed))
    i1_selfemployed<-i1_selfemployed[!is.na(i1_selfemployed[,selfemploymentyearmonths_field]),]
    i1_selfemployed<-i1_selfemployed[!is.na(i1_selfemployed[,selfemploymentyearmonthincome_field]),]
    total_self_employed_considered<-dim(i1_selfemployed)[1]
    print(paste("Number of self-employed-workers ignored because of incomplete data:",total_self_employed-total_self_employed_considered));
    x <-i1_selfemployed
    x$yearly_pay<-x[,selfemploymentyearmonths_field]*x[,selfemploymentyearmonthincome_field]
    #* used months in an year for computing total income from self-employment in an year
    #* ))
    return(x)
  }
  
  infer_lsms_sece_total_income<-function(i1,i2){
    #* ((
    #* Rejecting less than 5 year old members from income data
    ydata<-NULL
    i1 <- i1[!is.na(i1$is_ge5),]
    i1 <- i1[as.integer(i1$is_ge5)==1,]
    i1_w<-i1[!is.na(i1$is_wageworker),]
    i1_w <- i1_w[as.integer(i1_w$is_wageworker)==1,]
    #* Looking only at wage workers
    #* sum up wages into column yearly pay
    i1_w_y <- computeYearValues(dat=i1_w,
                                unit_field="lastpayment_unit",
                                quantity_field="lastpayment",
                                workyearweekhours_field="workyearweekhours",
                                workyearmonthweeks_field="workyearmonthweeks",
                                workyearmonths_field="workyearmonths",
                                output_field="yearly_pay");
    ydata<-rbind(ydata,
                 data.frame(hhid=i1_w_y$hhid,personid=i1_w_y$personid,
                            yearly_pay=i1_w_y$yearly_pay,
                            employertype=i1_w_y$employertype,stringsAsFactors=FALSE),
                 stringsAsFactors=FALSE)
    #other forms of payment
    #* sum up values in other forms of payment as well
    i1_w_other<- i1_w[!is.na(i1_w$has_lastpayment_other),]
    i1_w_other <- i1_w_other[as.integer(i1_w_other$has_lastpayment_other)==1 ,]
    
    i1_w_other_y <- computeYearValues(dat=i1_w_other,
                                      unit_field="lastpayment_other_unit",
                                      quantity_field="lastpayment_other",
                                      workyearweekhours_field="workyearweekhours",
                                      workyearmonthweeks_field="workyearmonthweeks",
                                      workyearmonths_field="workyearmonths",
                                      output_field="yearly_pay");
    
    ydata<-rbind(ydata,
                 data.frame(hhid=i1_w_other_y$hhid,
                            personid=i1_w_other_y$personid,
                            yearly_pay=i1_w_other_y$yearly_pay,
                            employertype=i1_w_other_y$employertype,
                            stringsAsFactors=FALSE
                 ),
                 stringsAsFactors=FALSE)
    #secondary job wages
    
    #* sum up values in from secondary of payment (for wage-workers)
    i1_secjob<-i1[!is.na(i1$has_secjobwages),]
    i1_secjob<-i1_secjob[!is.na(i1_secjob$has_secjob),]
    i1_secjob <- i1_secjob[as.integer(i1_secjob$has_secjobwages)==1,]
    i1_secjob <-i1_secjob[as.integer(i1_secjob$has_secjob)==1,]
    
    i1_secjob_y <- computeYearValues(dat=i1_secjob,
                                     unit_field="lastpayment_secjobwage_unit",
                                     quantity_field="lastpayment_secjobwage",
                                     workyearweekhours_field="workyearweekhours_secjob",
                                     workyearmonthweeks_field="workyearmonthweeks_secjob",
                                     workyearmonths_field="workyearmonths_secjob",
                                     output_field="yearly_pay");
    
    # secondary job must have employertype invalidated (set to -1 in the current convention)
    
    #* Only primary job is used to identify the employer type of the individual 
    print (paste("Setting employertype as -1 (for ",dim(i1_secjob_y)[1],") wage-workers with secondary jobs"))
    ydata<-rbind(ydata,
                 data.frame(hhid=i1_secjob_y$hhid,
                            personid=i1_secjob_y$personid,
                            yearly_pay=i1_secjob_y$yearly_pay,
                            employertype=rep(-1,dim(i1_secjob_y)[1]),
                            stringsAsFactors=FALSE
                 ),
                 stringsAsFactors=FALSE)
    #* collecting other wages from secondary job
    i1_secjob_other<-i1[!is.na(i1$has_secjobwages_other),]
    i1_secjob_other<-i1_secjob_other[!is.na(i1_secjob_other$has_secjob),]
    i1_secjob_other <-i1_secjob_other[as.integer(i1_secjob_other$has_secjob)==1,]
    i1_secjob_other <- i1_secjob_other[as.integer(i1_secjob_other$has_secjobwages_other)==1,]
    
    i1_secjob_other_y <- computeYearValues(dat=i1_secjob_other,
                                           unit_field="lastpayment_secjobwage_other_unit",
                                           quantity_field="lastpayment_secjobwage_other",
                                           workyearweekhours_field="workyearweekhours_secjob",
                                           workyearmonthweeks_field="workyearmonthweeks_secjob",
                                           workyearmonths_field="workyearmonths_secjob",
                                           output_field="yearly_pay");
    print(paste("Setting employertype=-1 for ",dim(i1_secjob_other_y)[1]," wage workers with other payments in their secondary jobs")) 
    ydata<-rbind(ydata,data.frame(hhid=i1_secjob_other_y$hhid,
                                  personid=i1_secjob_other_y$personid,
                                  yearly_pay=i1_secjob_other_y$yearly_pay,
                                  employertype=rep(-1,dim(i1_secjob_other_y)[1]),
                                  stringsAsFactors=FALSE
    ),
    stringsAsFactors=FALSE
    )
    #rbind for the yearly-pay data-frame
    selfemployment_offset<-1000
    if (max(ydata$employertype)>=selfemployment_offset){
      stop(paste("max(employertype)=",max(ydata$employertype)," in income data(ydata) is less than the selected offset (",selfemployment_offset,")"))
    }
    print (paste("Adding ",selfemployment_offset," to selfemployment_type code and setting those values as employertype"))
    #* collecting self-employment income
    i1_selfemployed_y<-computeLsmsSelfemployedValues(dat=i1,
                                                     has_selfemployment_year_field="has_selfemployment_year",
                                                     selfemploymentyearmonths_field="selfemploymentyearmonths",
                                                     selfemploymentyearmonthincome_field="selfemploymentyearmonthincome");
    a1=data.frame(hhid=i1_selfemployed_y$hhid,
                  personid=i1_selfemployed_y$personid,
                  yearly_pay=i1_selfemployed_y$yearly_pay,
                  employertype=selfemployment_offset+i1_selfemployed_y$selfemploymenttype,
                  stringsAsFactors=FALSE
    )
    
    #* calling computeLsmsSelfemployedValues
    
    #i1_selfemployed_y2<-computeLsmsSelfemployedValues(dat=i2,
    #                                                  has_selfemployment_year_field="has_selfemployment_year",
    #                                                  selfemploymentyearmonths_field="selfemploymentyearmonths",
    #                                                  selfemploymentyearmonthincome_field="selfemploymentyearmonthincome");
    #a2=data.frame(hhid=i1_selfemployed_y2$hhid,
    #              personid=i1_selfemployed_y2$personid,
    #              yearly_pay=i1_selfemployed_y2$yearly_pay,
    #              employertype=i1_selfemployed_y2$selfemploymenttype,
    #              stringsAsFactors=FALSE)
    
    #print("Running outer-join (all-merge) for data from files 1 and 2");
    #a=merge(a1,a2,all=TRUE)
    a=a1;
    
    ydata<-rbind(ydata,a,stringsAsFactors=FALSE)
    
    print ("PENDING CONTROL VARS: employment_type, self_owned_business_type")
    
    #* summing up yearly-income from all sources
    
    print ("Running ddply to sum up yearly-pay from all sources")
    ydata <-ddply(ydata,.(hhid,personid),total_income=sum(yearly_pay))
    return(ydata)
    
    #* ))
    
  }
  
  return(new("LSMSNormalizer",diary_info_columns_lsms_2008=diary_info_columns_lsms_2008,
             hh_mapping_lsms_2008=hh_mapping_lsms_2008,
             get_lsms_secl_info_columns_2008=get_lsms_secl_info_columns_2008,
             get_lsms_secl_fields_mapping_2008=get_lsms_secl_fields_mapping_2008,
             ohs_seca_mapping_lsms_2008=ohs_seca_mapping_lsms_2008,
             ohs_info_columns_lsms_2008=ohs_info_columns_lsms_2008,
             ohs_mapping_lsms_2008=ohs_mapping_lsms_2008,
             get_ohs_secc_columns_lsms_2008=get_ohs_secc_columns_lsms_2008,
             get_ohs_secc_fields_mapping_lsms_2008=get_ohs_secc_fields_mapping_lsms_2008,
             get_lsms_secj_info_columns_2008=get_lsms_secj_info_columns_2008,
             get_lsms_secj_fields_mapping_2008=get_lsms_secj_fields_mapping_2008,
             food_categories_lsms_2010=food_categories_lsms_2010,
             items_codes_2008=items_codes_2008,
             items_codes_2010=items_codes_2010,
             lsms_groups_pricebased_2010_2012=lsms_groups_pricebased_2010_2012,
             lsms_groups_qualitybased_2010_2012=lsms_groups_qualitybased_2010_2012,
             lsms_groups_sparsenessbased_2010_2012=lsms_groups_sparsenessbased_2010_2012,
             assets_order_2010_2012=assets_order_2010_2012,
             items_codes_2012=items_codes_2012,
             items_codes_2014=items_codes_2014,
             asset_types_2010_2012=asset_types_2010_2012,
             inheritable_assets_2010_2012=inheritable_assets_2010_2012,
             get_piece_measures=get_piece_measures,
             ignored_bad_units=ignored_bad_units,
             get_group_qconv_factor=get_group_qconv_factor,
             multiplyLsmsQuantities=multiplyLsmsQuantities, 
             get_lsms_secj_info_columns_2010=get_lsms_secj_info_columns_2010, 
             get_lsms_secj_fields_mapping_2010=get_lsms_secj_fields_mapping_2010, 
             get_lsms_secm_info_columns=get_lsms_secm_info_columns, 
             get_region_popdensity_map=get_region_popdensity_map,
             read_tnz=read_tnz, 
             get_lsms_secm_fields_mapping=get_lsms_secm_fields_mapping, 
             get_lsms_secl_info_columns_2010=get_lsms_secl_info_columns_2010, 
             get_lsms_secl_fields_mapping_2010=get_lsms_secl_fields_mapping_2010, 
             get_ohs_secc_columns_lsms_2010=get_ohs_secc_columns_lsms_2010, 
             get_ohs_secc_fields_mapping_lsms_2010=get_ohs_secc_fields_mapping_lsms_2010, 
             ohs_seccb_columns_lsms=ohs_seccb_columns_lsms, 
             ohs_seccb_mapping_lsms=ohs_seccb_mapping_lsms, 
             ohs_seccj_columns_lsms_2010=ohs_seccj_columns_lsms_2010, 
             ohs_seccj_mapping_lsms_2010=ohs_seccj_mapping_lsms_2010, 
             ohs_seccj_columns_lsms_2012=ohs_seccj_columns_lsms_2012, 
             ohs_seccj_mapping_lsms_2012=ohs_seccj_mapping_lsms_2012,
             ohs_seccf_columns_lsms_2014=ohs_seccf_columns_lsms_2014,
             ohs_seccf_mapping_lsms_2014=ohs_seccf_mapping_lsms_2014,
             items_market_price_codes_2014=items_market_price_codes_2014,
             ohs_seccj_columns_lsms_2008=ohs_seccj_columns_lsms_2008, 
             ohs_seccj_mapping_lsms_2008=ohs_seccj_mapping_lsms_2008,
             ohs_seca_columns_lsms=ohs_seca_columns_lsms, 
             ohs_seca_mapping_lsms_2010=ohs_seca_mapping_lsms_2010, 
             area_code=area_code, 
             analyse_cj=analyse_cj, 
             occupation_mapping=occupation_mapping, 
             ohs_info_columns_lsms_2010=ohs_info_columns_lsms_2010, 
             get_ohs_info_columns=get_ohs_info_columns, 
             get_diary_info_columns=get_diary_info_columns, 
             diary_info_columns_lsms_2010=diary_info_columns_lsms_2010, 
             hh_mapping_lsms_2010=hh_mapping_lsms_2010, 
             ohs_mapping_lsms_2010=ohs_mapping_lsms_2010, 
             get_lsms_sece1_columns_2010=get_lsms_sece1_columns_2010, 
             get_lsms_sece2_columns_2010=get_lsms_sece2_columns_2010, 
             get_lsms_sece_fields_mapping_2010=get_lsms_sece_fields_mapping_2010,
             diary_info_columns_lsms_2012=diary_info_columns_lsms_2012,
             diary_info_columns_lsms_2014=diary_info_columns_lsms_2014,
             hh_mapping_lsms_2012=hh_mapping_lsms_2012, 
             hh_mapping_lsms_2014=hh_mapping_lsms_2014,
             get_lsms_seck_info_columns_2012=get_lsms_seck_info_columns_2012, 
             get_lsms_seck_info_columns_2014=get_lsms_seck_info_columns_2014,
             get_lsms_seck_fields_mapping_2012=get_lsms_seck_fields_mapping_2012, 
             get_lsms_seck_fields_mapping_2014=get_lsms_seck_fields_mapping_2014,
             get_diary_secn_columns_lsms_2008=get_diary_secn_columns_lsms_2008,
             get_diary_secn_columns_lsms_2010=get_diary_secn_columns_lsms_2010,
             get_diary_secn_columns_lsms_2012=get_diary_secn_columns_lsms_2012,
             get_diary_secn_fields_mapping_lsms_2008=get_diary_secn_fields_mapping_lsms_2008,
             get_diary_secn_fields_mapping_lsms_2010=get_diary_secn_fields_mapping_lsms_2010,
             get_diary_secn_fields_mapping_lsms_2012=get_diary_secn_fields_mapping_lsms_2012,
             ohs_seca_mapping_lsms_2012=ohs_seca_mapping_lsms_2012,
             ohs_seca_mapping_lsms_2014=ohs_seca_mapping_lsms_2014,
             ohs_info_columns_lsms_2012=ohs_info_columns_lsms_2012,
             ohs_info_columns_lsms_2014=ohs_info_columns_lsms_2014,
             get_ohs_secc_columns_lsms_2012=get_ohs_secc_columns_lsms_2012, 
             get_ohs_secc_columns_lsms_2014=get_ohs_secc_columns_lsms_2014,
             get_ohs_secc_fields_mapping_lsms_2012=get_ohs_secc_fields_mapping_lsms_2012,
             get_ohs_secc_fields_mapping_lsms_2014=get_ohs_secc_fields_mapping_lsms_2014,
             ohs_mapping_lsms_2012=ohs_mapping_lsms_2012,
             ohs_mapping_lsms_2014=ohs_mapping_lsms_2014,
             get_lsms_secj_info_columns_2012=get_lsms_secj_info_columns_2012,
             get_lsms_secj_info_columns_2014=get_lsms_secj_info_columns_2014,
             get_lsms_secj_fields_mapping_2012=get_lsms_secj_fields_mapping_2012,
             get_lsms_secj_fields_mapping_2014=get_lsms_secj_fields_mapping_2014,
             decode_clusterid=decode_clusterid,
             computeYearValues=computeYearValues, 
             computeLsmsSelfemployedValues=computeLsmsSelfemployedValues, 
             infer_lsms_sece_total_income=infer_lsms_sece_total_income))
}