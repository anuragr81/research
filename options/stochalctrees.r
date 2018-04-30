

generate_tree_num <-function (u,d,S0,n){
  
  if (abs( u*d-1) >1e-7){
    stop("Invalid u,d")
  }
  
  prevGrid <- S0
  for ( i in seq(1,n)){
    curLevel <- combn(i+1,i)
    #expand.grid(c("u","d"),c("u","d"),c("u","d"))
    #mdply to combine 
    f <-function(...) { dat<-list(...);return(prod(as.numeric(dat)));}
    prevGrid<- mdply(expand.grid(prevGrid,c(u,d)),f)[,c("V1")]
    print(prevGrid)
  }
  
  
} 

generate_tree <-function (u,d,S0,n){
  
  if (abs( u*d-1) >1e-7){
    stop("Invalid u,d")
  }
  ret = list();
  prevGrid <- ""
  for ( i in seq(1,n)){
    curLevel <- combn(i+1,i)
    f<-function(a,b) { return(paste(a,b,sep=""));}
    prevGrid<- mdply(expand.grid(a=prevGrid,b=c("u","d")),f)[,c("V1")]
    ret[[i]] <-prevGrid
  }
  return(ret)
}

