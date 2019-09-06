

if (isClass("LSMSGroupCollect")){
  print ("Warning !!! previous definition of LSMSGroupCollect would be overwritten.")
}

## all exported functions are declared here
setClass("LSMSGroupCollect", representation(get_regressed_market_price="function",
                                            select_market_price="function",
                                            fill_missing_yearvals="function",
                                            get_substitution_frame="function",
                                            get_exchange_rate_usd_tnz="function") )


lgc<-function(){
  
  get_exchange_rate_usd_tnz () {
    x <- data.frame(source=NULL,target=NULL) 
    x <- rbind(x,data.frame( year = 2010, usd_tnz = (1340+1485)/2))
    x <- rbind(x,data.frame( year = 2012, usd_tnz = (1580+1598)/2))
    x <- rbind(x,data.frame( year = 2014, usd_tnz = (1596+1715)/2))
    return(x)
  }
  
  get_substitution_frame <- function(){
    x <- data.frame(source=NULL,target=NULL)
    x <- rbind(x,data.frame(source="brews",target="beer"))
    x <- rbind(x,data.frame(source="brews",target="winespirits"))
    x <- rbind(x,data.frame(source="beef",target="pork"))
    x <- rbind(x,data.frame(source="petrol",target="gas"))
    return(x)
  }
  get_food_inflation <- function() {
    x <- data.frame(year=NULL, price=NULL)
    x<- rbind(x, data.frame( year= 2010 , price= 6.51666666666667 ))
    x<- rbind(x, data.frame( year= 2012 , price= 20.9158333333333 ))
    x<- rbind(x, data.frame( year= 2014 , price= 7.44166666666667 ))
    return(x)
  }
  
  fill_missing_yearvals <- function(category,year,price,curyear){
    df <- data.frame(year = year, price = price, curyear = rep(curyear,length(year)))
    if ( dim(df[!is.na(df$price),])[1] < 2){
      if (dim(df[!is.na(df$price),])[1] == 0){
        stop("Must have one non-NA price")
      } else {
        #number of NA is exactly one
        categ <- unique(category)
        if (length(categ)>1){
          stop("Cannot handle multiple categories")
        }
        if (is.element(categ, c("densefoods","fruitsveg","nonfresh","protein"))){
          
          datadf = get_food_inflation()
          
          lmres <- lm(data=datadf,price~year)
          df$scaled <- lmres$coefficients[2]*c(2008,2010,2012,2014) + lmres$coefficients[1]
          x <- with(df,price/scaled)
          x <- x[!is.na(x)]
          df$price <- x*df$scaled
          return(subset(df,curyear==year)$price)
        } else {
          stop(paste("Unknown category:",categ,"for extrapolation"))
        }
      }
      
    } else {
      lmres <- lm(data=df,price~year)
      return(lmres$coeff[2]*curyear + lmres$coeff[1])
      
    }
    return(paste("fill_missing_yearvals - year (1):",curyear))
    
  }
  
  select_market_price <- function (nat,reg,dstt) { 
    if (!is.na(dstt)) {
      return (dstt)
    } else if (!is.na(reg)) {
      return (reg)
    } else if (!is.na(nat)) {
      return(nat)
    }
    stop("Could not find valid price")
  }
  
  
  
  get_regressed_market_price <- function (lwp,price){
    isDebug <- FALSE
    if (length(unique(price))>=3 && any(abs(diff(price))>0) && any(abs(diff(lwp))>0)){
      invsqr         <- 1/lwp**2
      lmres          <- lm(price~ invsqr)
      reg_price      <- (lmres$coefficients[[1]]+lmres$coefficients[[2]])
    } else if (length(price)==0){
      stop("No prices")
    } else if (length(unique(price))<3 || !any(abs(diff(price))>0) || !any(abs(diff(lwp))>0) ){
      return(median(price))
    } else {
      stop("Unhandled block")
    }
    if (isDebug){
      print(paste("n=",length(price),"median_price=",median(price),"reg_price=",reg_price))
    }
    if (reg_price <0 || is.na(reg_price) || abs(log(median(price)/reg_price,2))>1 ) {
      return(median(price))
    } else {
      return(reg_price)
    }
    
    
  }
  
  
  return(new("LSMSGroupCollect",get_regressed_market_price=get_regressed_market_price,
             select_market_price=select_market_price,fill_missing_yearvals=fill_missing_yearvals,
             get_substitution_frame=get_substitution_frame,get_exchange_rate_usd_tnz=get_exchange_rate_usd_tnz) );
  
}
