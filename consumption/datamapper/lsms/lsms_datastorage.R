

if (isClass("LSMSDataStorage")){
  print ("Warning !!! previous definition of LSMSDataStorage would be overwritten.")
}

## all exported functions are declared here
setClass("LSMSDataStorage", representation(get_electricity_prices="function",get_household_cpi="function",
                                           get_transportation_cpi="function", get_food_inflation = "function") )


ldat<-function(){
  
  
  
  get_electricity_prices <- function () { 
    x <- data.frame(year=NULL, price=NULL)
    x<- rbind(x, data.frame( year=2006, price=38))
    x<- rbind(x, data.frame( year=2007, price=40))
    x<- rbind(x, data.frame( year=2008, price=49))
    x<- rbind(x, data.frame( year=2009, price=49))
    x<- rbind(x, data.frame( year=2010, price=49))
    x<- rbind(x, data.frame( year=2011, price=60))
    x<- rbind(x, data.frame( year=2012, price=60))
    x<- rbind(x, data.frame( year=2013, price=60))
    x<- rbind(x, data.frame( year=2014, price=100))
    x<- rbind(x, data.frame( year=2015, price=100))
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
             get_food_inflation=get_food_inflation) );
  
}
