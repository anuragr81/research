
si_factor<-function(units){
  if (!is.character(units)){
    stop("units must be of character type")
  }
  factors = array();
  i <- 1; 
  for (str in units){
    factors[i]<-1
    if (!is.na(str)) {
      if (tolower(str)=="gram" ||
          tolower(str)=="grams" ||
          tolower(str)=="g"){
        factors[i]<-1e-3;
      }
      
      if (tolower(str)=="millilitre" ||
          tolower(str)=="milliliter" ||
          tolower(str)=="ml"){
        factors[i]<-1e-6;
      }
      
      if (tolower(str)=="litre" ||
          tolower(str)=="liter" ||
          tolower(str)=="l"){
        factors[i]<-1e-3;
      }
    }
    i <- i + 1;
  }
  return(factors);
}


get_translated_frame<-function(dat,names,m){
  isDebug<-TRUE
  if (isDebug){
    print(names)
  }
  if (!is.vector(names)){
    stop(paste("names of type(",toString(class(names)),") must be a vector"))
  }
  
  
  mapped<-m[is.element(m$name,names),]
  array_names <- as.character(mapped$iesname)
  
  
  if (isDebug==TRUE){
    print(paste("colnames(dat)=",toString(colnames(dat))))
    print(paste("array_names=",toString(array_names)))
    for (i in seq(1,length(array_names))){
      print(paste("array_names[",i,"]=",array_names[i]))
      if (length(grep(array_names[i],colnames(dat)))== 0){
        print(paste("Not found:",array_names[i]))
      }
    }
  }
  res<-data.frame(dat[,array_names])
  
  if (length(mapped$name)!=length(colnames(res))){
    stop('Error in mapping of columns')
  }
  colnames(res)<-mapped$name
  return(res)
}
