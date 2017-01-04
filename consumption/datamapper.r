


#When data gets prepared, we don't know how to analyze it. As we analyze the data, we need more fields in the table. 
#So one can never really prepare the data so well that it doesn't need any further adjustments. What one can do therefore is 
#to facilitate this change - so that the user adds the method to analyse and the data-fields at one place.

#Currently, we have similar functions for ohs, hh and income files - which are analyzed in regression module. 
# 1. One task would be to make the analysis functions more generic (suuport n>3 types of files) - 
# 2. The other task would be to combine regerssion and addition of fields into one module.

# The both tasks go hand in hand. If the functions for analysis are not generic, 
# then one cannot decouple the functions for regression (every user
# would have to follow the constraint imposed by the regression module (Whtich isn't generic).

# Both tasks can be achieved if one views the framework to evaluate a tree of mapper functions. The steps that we 
# repeat for the household data are the following:
# 1. loading diary
# 2. loading ohs (some secondary fields are calculated at this stage)
# 5. merge all the files into one data-frame - along with creating more secondary fields#

# The generic function that the user may provide should be able to load any number of files and use translation mappings 
# provided in a structure populated by the user. The generic functions themselves are also input by the user. The evaluation 
# tree connects functions with each other. For example, if the total expenditure of a household is a derived (secondary) field 
# then the user must provide a way to combine the fields in the file. The tree - generic and doesn't require the framework to 
# know the name of the functions that the users declare. For the above steps the root of the tree would be the function
# merge_hh_ohs_income_data - which would take hh,ohs, income and depar as inputs frame and creates the final frame with derived fields
# that serve as input for the regressions. The user needs to provide the following entities to the framework (which would merely 
# dfs through the passed tree structure):
# 1. File readers as data source as nodes of the tree.
# 2. Tree vertices made out of the nodes - every vertex has children and parent.This requires every vertex to know it's children which can get data from. 
# 
# There is no built-in support for trees - except for lists. If list of lists follow a schema then the schema can be quite well-formed.


setwd('c:/local_files/research/consumption/')

create_root<-function(data){
  root= list ()
  root[["children"]]=list()
  root[["data"]]=data;
  return(root)
}

add_child<-function(parent,data,isNode){
  if ( (!is.null(parent[["children"]]) && !is.null(parent[["data"]]) && length(parent)==2) || 
       (!is.null(parent[["children"]]) && !is.null(parent[["data"]]) && !is.null(parent[["parent"]]) && length(parent)==3)
  ) {
    
    v = list()
    
    v[["children"]]=list()
    
    v[["data"]]=data
    v[["parent"]]=parent;
    
    # appending to the parent[["children"]] - assuming it's an array
    parent[["children"]][[length(parent[["children"]])+1]]=v
    return(parent)
  } else {
    stop("Invalid parent")
  }
}

traverse<-function(v,depth,logDepth,processFunc){
  if (missing(depth)){
    depth<-0
  }
  if (missing(logDepth)){
    logDepth=FALSE
  }
  
  if ( (!is.null(v[["children"]]) && !is.null(v[["data"]]) && length(v)==2) || 
       (!is.null(v[["children"]]) && !is.null(v[["data"]]) && !is.null(v[["parent"]]) && length(v)==3)
  ) {
    if ( length(v[["children"]])==0) { # leaf of the tree
      res=list()
      if (missing(processFunc)){
        res[[length(res)+1]]=v[["data"]]
      } else {
        res[[length(res)+1]]=processFunc(v[["data"]])
      }
      if (logDepth)
      {
        print(paste("depth(node)=",depth))
      }
      return(res)
    } else { # not a leaf
      res=list()
      if (missing(processFunc)){
        res[[1]]=v$data;
      } else {
        res[[1]]=processFunc(v$data)
      }
      for (child in v[["children"]]){
        if (logDepth){
          print(paste("depth=",depth))
        }
        res=c(res,traverse(child,depth+1))
      }
      return(res)
    }
  } 
  else {
    stop("Invalid traversal start")
  }
  
}

eval_tree<-function(v,processFunc){
  
  if ( (!is.null(v[["children"]]) && !is.null(v[["data"]]) && length(v)==2) || 
       (!is.null(v[["children"]]) && !is.null(v[["data"]]) && !is.null(v[["parent"]]) && length(v)==3)
  ) {
    if ( length(v[["children"]])==0) { # leaf of the tree
      if (missing(processFunc)){
        res<-v[["data"]]
      } else {
        res<-processFunc(v[["data"]])
      }
      return(res)
    } else { # not a leaf
      if (missing(processFunc)){
        res<-v$data;
      } else {
        res<-processFunc(v$data)
      }
      for (child in v[["children"]]){
        #res=c(res,traverse(child,depth+1))
        #TODO: implement
      }
      return(res)
    }
  } 
  else {
    stop("Invalid evaluate start")
  }
  
}


test_tree<-function(){
  root<-create_root(2)
  root<-add_child(parent=root,data=3)
  root<-add_child(parent=root,data=4)
  root$children[[1]]<-add_child(parent=root$children[[1]],data=5)
  root$children[[2]]<-add_child(parent=root$children[[2]],data=6) # can't pass an arbitrary index
  # 2
  # |   |
  # 3   4
  # |   |
  # 5   6
  return(root)
}

load_ohs_file<-function(f){
  print(f)
  return(f)
}

merge_all<-function(ohs,hh,income){
  print(paste("ohs=",ohs,"hh=",hh,"income=",income))
  return(0)
}

init_classes <-function(){
  setClass("Caller", representation(name = "character", params = "list", evaluate="function"))
}
# TODO: remove or move

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
  # merge is the root of the tree 
  # it's children are params that are themselves caller objects. merge takes data frames provided by ohs, hh and income extractor;
  # the node for merge must have ohs, hh and income extractor functions as children. It would call these functions recursively
  # i.e. ohs, hh and income data-frames (or whatever else format they returned e.g. numeric arrays or character strings) would be
  # returned by extractor children. The output there must also be a list.
  # Here is the final format: 
  # 1. merge list of functors { ohs_extractor, hh_extractor, income_extractor}
  # 2. call the functions 
  # With above scheme, the functors should have input variables stored in them - this requires setting up the ohs_extractor.
  # ohs_extractor may take a set of input filenames as input. Thus it's own format would be the following:
  # ohs_extractor (function=code,params=list())
  
  # It appears that instead of an explicit tree - what we need is just the ability to maintain functions as objects - maintaining the 
  # output of a function pluggable to the input params list of another. This would maintain an implicit tree whose leaves would be those
  # with static inputs - while all other nodes would know which functions to call
  l[["ohsfilename"]]="ohsfile.txt"
  
  elementOhs <-new("Caller",name="ohs",params=l,evaluate=read_ohs) # elementOhs has output stored (it doesn't need to remember the output )
  #every node would evaluate as - elementOhs@evaluate(elementOhs@params)
  ohsOutput <- do.call(elementOhs@evaluate,elementOhs@params)
  
  j=list()
  j[["hhfilename"]]="hhfile.txt"
  elementHh <-new("Caller",name="hh",params=j,evaluate=read_hh) # elementOhs has output stored (it doesn't need to remember the output )
  hhOutput<- do.call(elementHh@evaluate,elementHh@params) # this call can be made from inside the tree evaluation method
  
  mergeInput= list()
  mergeInput[["hh"]] = hhOutput;
  mergeInput[["ohs"]] = ohsOutput;
  
  print(paste(toString(mergeInput)))
  elementMerge <-new("Caller",name="merge",params=mergeInput,evaluate=merge_ohs) # elementOhs has output stored (it doesn't need to remember the output )
  #TODO: get inside every function of dynamic caller (as well as dynamic caller to provide output to the code) 
  
  return(do.call(elementMerge@evaluate,elementMerge@params))
}

