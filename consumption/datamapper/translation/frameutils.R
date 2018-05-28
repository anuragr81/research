

if (isClass("FrameUtils")){
  print ("Warning !!! previous definition of FrameUtils would be overwritten.")
}

## all exported functions are declared here
setClass("FrameUtils", representation(si_factor= "function",get_translated_frame="function",count_higher_than="function",filter_categories_data="function",
                                      removeall_cols_except="function",find_nonzero_percentile="function",
                                      fv="function",rbind_xy="function") )

removeall_cols_except<-function(dat,listColumnNames){
  #c("region","district","ward","accessiblemarket","travelcost"))
  for (colName in setdiff(names(dat),listColumnNames)){
    dat[,colName]<-NULL
  }
  
}

fu<-function(){
  
  filter_extremes <-function(dat,highMultiple,threshold){
    
    dat            <-dat[dat>0]
    dat            <-dat[!is.na(dat)]
    
    stepSize       <-.01
    x              <-1
    while (quantile(dat,x)/quantile(dat,.5)>highMultiple && x > threshold ) {
      x            <- x - stepSize
    }
    res                = list()
    res[["quantile"]]  = x
    res[["value"]]     = quantile(dat,x)
    return(res)
  }
  
  
  rbind_xy<-function(x,y,tagx,tagy) {
    for ( i in setdiff(colnames(x),colnames(y)) ) { y[,c(i)]<-rep(NA,dim(y)[1]) }
    for ( i in setdiff(colnames(y),colnames(x)) ) { x[,c(i)]<-rep(NA,dim(x)[1]) }
    if (missing(tagx)){
      if (!is.element("tag",colnames(x)) ){
        stop("Must have a column tag in x to differentiate added y from x")
      }
    } else {
      x$tag <- tagx
    }
    y$tag <- tagy
    return(rbind(x,y))
  }
  
  fv<-function (x) { 
    return(as.double(filter_extremes(dat=x,highMultiple = 30, threshold = .95)[["value"]])) 
  }
  
  #fq<-function (x) { return(as.double(filter_extremes(dat=x,highMultiple = highMultiple, threshold = .95)[["quantile"]])) }
  
  filter_categories_data<-function(hh,selected_category,item_field,set_depvar){
    
    vis<-hh[is.element(hh[,item_field],selected_category),]
    if (dim(vis)[1] <=0){
      if (set_depvar){
        stop(paste("No entries found in category: ",toString(selected_category)))
      }
    }
    vis<-ddply(vis,.(hhid),summarize,visible_consumption=sum(cost))
    no_vis_hhid<-setdiff(unique(hh$hhid),unique(vis$hhid))
    
    # set all values to zero for the hhids where the data isn't found in the
    # selected_category
    
    no_vis<-data.frame(hhid=no_vis_hhid,visible_consumption=rep(0,length(no_vis_hhid)))
    vis <- rbind(vis,no_vis)
    return(vis)
  }
  
  
  count_higher_than<-function(a,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10) 
  {
    return (sum(as.integer(c(m1>a,
                             m2>a,
                             m3>a,
                             m4>a,
                             m5>a,
                             m6>a,
                             m7>a,
                             m8>a,
                             m9>a,
                             m10>a)
    )));
  }
  
  
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
  
  
  get_translated_frame<-function(dat,names,m,isDebug){
    if (missing(isDebug)||isDebug==FALSE){
      isDebug=FALSE
    } else{
      isDebug=TRUE
    }
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
  
  find_nonzero_percentile<-function (ds,depvar, tol) { 
    if (missing(tol)){
      t=.01
    } else {
      t= tol;
    }
    while (quantile(ds[,depvar],t)<=0){
      t=t+tol
    }
    return(1-t)
  }
  
  return(new("FrameUtils",si_factor= si_factor, 
             get_translated_frame = get_translated_frame ,
             count_higher_than=count_higher_than,
             filter_categories_data=filter_categories_data,
             removeall_cols_except=removeall_cols_except,
             find_nonzero_percentile=find_nonzero_percentile,
             fv=fv,
             rbind_xy=rbind_xy) );
  
}
