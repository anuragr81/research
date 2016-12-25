


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

traverse<-function(v,depth){
  if ( (!is.null(v[["children"]]) && !is.null(v[["data"]]) && length(v)==2) || 
       (!is.null(v[["children"]]) && !is.null(v[["data"]]) && !is.null(v[["parent"]]) && length(v)==3)
  ) {
    if ( length(v[["children"]])==0) { # node of the tree
      res=list()
      res[[length(res)+1]]=v[["data"]]
      print(paste("depth(node)=",depth))
      return(res)
    } else {
      res=list()
      res[[1]]=v$data;
      for (child in v[["children"]]){
        print(paste("depth=",depth))
        res=c(res,traverse(child,depth+1))
      }
      return(res)
    }
  } 
  else {
    stop("Invalid traversal start")
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
