
get_translated_frame<-function(dat,names,m){
  isDebug<-TRUE
  if (isDebug){
    print(names)
  }
  if (!is.vector(names)){
    stop(paste("names of type(",toString(class(names)),") must be a vector"))
  }
  
  #m= mapping();
  
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
