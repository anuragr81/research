
init_classes <-function(){
  setClass("Caller", representation(name = "character", params = "list", evaluate="function"))
}

run_caller<-function(elem){
  outputList = list()
  for (paramName in names(elem@params)){
    # if param is of type Caller then make do.callif
    # otherwise, just call the function
    paramValue = elem@params[[paramName]]
    if (isS4(paramValue) && class(paramValue)[[1]]=="Caller"){
      result <- run_caller(paramValue)
      if (class(result)=="list"){
        #TODO: add support by merging with the outputList
        stop("list output not supported")
      }
      outputList[[paramValue@name]]=result
    } else {
      outputList[[paramName]]=paramValue
    }
    
  } # end for
  return(do.call(elem@evaluate,outputList))
}

### TESTS ####


read_ohs<-function(ohsfilename){
  print(paste("Reading f=",ohsfilename))
  dat<-data.frame(x=c(1,2),y=c(2,3))
  return(dat)
}

merge_ohs <- function(ohs,hh){ 
  print(paste("merging",ohs,hh,sep=";"))
  return(1)
}

read_hh <- function(hhfilename){
  print(paste("reading",hhfilename))
  dat<-data.frame(x=c(1,2),y=c(22,32))
  return(dat)
}

test_function_tree<-function(){
  init_classes()
  
  l = list()
 
   l[["ohsfilename"]]="ohsfile.txt"
  
  elementOhs <-new("Caller",name="ohs",params=l,evaluate=read_ohs) # elementOhs has output stored (it doesn't need to remember the output )
  
  j=list()
  j[["hhfilename"]]="hhfile.txt"
  elementHh <-new("Caller",name="hh",params=j,evaluate=read_hh) # elementOhs has output stored (it doesn't need to remember the output )
  
  mergeInput= list()
  mergeInput[["hh"]] = elementHh
  mergeInput[["ohs"]] = elementOhs
  
  print(paste(toString(mergeInput)));
  
  elementMerge <-new("Caller",name="merge",params=mergeInput,evaluate=merge_ohs) # elementOhs has output stored (it doesn't need to remember the output )
  return(run_caller(elementMerge))
}

