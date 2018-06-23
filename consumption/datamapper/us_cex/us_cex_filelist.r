
if (isClass("USCEXFileList")){
  print ("Warning !!! previous definition of USCEXFileList would be overwritten.")
}

## all exported functions are declared here
setClass("USCEXFileList", representation(get_ucc_file_mapping="function"))

uscexfiles<-function(){
  
  get_ucc_file_mapping<-function(dirprefix){
    return(read.csv(paste(dirprefix,"/ucc_mapping.csv",sep="")))
  }
  
  return(new("USCEXFileList",get_ucc_file_mapping=get_ucc_file_mapping))
} 
