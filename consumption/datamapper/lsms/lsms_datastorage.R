

if (isClass("LSMSDataStorage")){
  print ("Warning !!! previous definition of LSMSDataStorage would be overwritten.")
}

## all exported functions are declared here
setClass("LSMSDataStorage", representation(get_electricity_prices="function",get_household_cpi="function",
                                           get_transportation_cpi="function",get_exchange_rate_usd_tnz="function",
                                           get_gasoline_prices="function",get_price_for_category="function") )


ldat<-function(){
  get_price_for_category <- function(category,yr){
    if (category=="household"){
      return(subset(get_household_cpi(),year==yr))
    } 
    if (category == "transport"){
      return(subset(get_transportation_cpi(),year==yr))
    }
    return(data.frame())  
  }
  
  get_exchange_rate_usd_tnz <- function() {
    x <- data.frame(source=NULL,target=NULL) 
    x <- rbind(x,data.frame( year = 2010, usd_tnz = (1340+1485)/2))
    x <- rbind(x,data.frame( year = 2012, usd_tnz = (1580+1598)/2))
    x <- rbind(x,data.frame( year = 2014, usd_tnz = (1596+1715)/2))
    return(x)
  }
  
  get_gasoline_prices <- function () {
    x <- data.frame(year=NULL, price=NULL)
    x <- rbind(x, data.frame( year= 2010  , price= 1.22 ))
    x <- rbind(x, data.frame( year= 2012  , price= 1.31 ))
    x <- rbind(x, data.frame( year= 2014  , price= 1.3325 ))
    
    x <- ( merge(x, get_exchange_rate_usd_tnz()) %>% mutate ( price = price*usd_tnz) ) [ ,c("year","price") ]
    return(x)
  }
  
  
  get_electricity_prices <- function () { 
    x <- data.frame(year=NULL, price=NULL)
    x<- rbind(x, data.frame( year=2006, price=.38))
    x<- rbind(x, data.frame( year=2007, price=.40))
    x<- rbind(x, data.frame( year=2008, price=.49))
    x<- rbind(x, data.frame( year=2009, price=.49))
    x<- rbind(x, data.frame( year=2010, price=.49))
    x<- rbind(x, data.frame( year=2011, price=.60))
    x<- rbind(x, data.frame( year=2012, price=.60))
    x<- rbind(x, data.frame( year=2013, price=.60))
    x<- rbind(x, data.frame( year=2014, price=.100))
    x<- rbind(x, data.frame( year=2015, price=.100))
    x<- ( merge(x, get_exchange_rate_usd_tnz()) %>% mutate ( price = price*usd_tnz) ) [ ,c("year","price") ]
    return(x)
  }
  
  get_household_cpi <- function()
  {
    # obtained by averaging monthly values
    x <- data.frame(year=NULL, price=NULL)
    x<- rbind(x, data.frame( year=2010, price=56.9275))
    x<- rbind(x, data.frame( year=2012, price=83.2383333333333))
    x<- rbind(x, data.frame( year=2014, price=99.2816666666667))
    
    return(x)
  }
  
  get_transportation_cpi <- function()
  {
    # obtained by averaging monthly values
    x <- data.frame(year=NULL, price=NULL)
    x<- rbind(x, data.frame( year=2010, price=79.0158333333333))
    x<- rbind(x, data.frame( year=2012, price=85.571))
    x<- rbind(x, data.frame( year=2014, price=99.4466416666667))
    return(x)
  }
  
  return(new("LSMSDataStorage",get_electricity_prices=get_electricity_prices,
             get_household_cpi=get_household_cpi,get_transportation_cpi=get_transportation_cpi,
             get_gasoline_prices=get_gasoline_prices,
             get_exchange_rate_usd_tnz=get_exchange_rate_usd_tnz,
             get_price_for_category=get_price_for_category) );
  
}
