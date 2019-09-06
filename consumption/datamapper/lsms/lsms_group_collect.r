

if (isClass("LSMSGroupCollect")){
  print ("Warning !!! previous definition of LSMSGroupCollect would be overwritten.")
}

## all exported functions are declared here
setClass("LSMSGroupCollect", representation(get_regressed_market_price="function",
                                            select_market_price="function",
                                            fill_missing_yearvals="function") )


lgc<-function(){

  
  fill_missing_yearvals <- function(year,price,curyear){
    df  <- data.frame(year = year, price = price, curyear = rep(curyear,length(year)))
    if ( dim(df[!is.na(df$price),])[1] < 2){
      print("Cannot use regression")
      return(NA)
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
             select_market_price=select_market_price,fill_missing_yearvals=fill_missing_yearvals) );
  
}
