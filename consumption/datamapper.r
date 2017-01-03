


#When data gets prepared, we don't know how to analyze it. As we analyze the data, we need more fields in the table. 
#So one can never really prepare the data so well that it doesn't need any further adjustments. What one can do is 
#to facilitate this change - so that the user adds the method to analyse and the required fields at one place. 

#Currently, we have similar functions for ohs, hh and income files - which are analyzed in regression module. 
# 1. One task would be to make the analysis functions more generic (suuport n>3 types of files) - 
# 2. The other task would be to combine regerssion and addition of fields into one module. 

# The both tasks go hand in hand. If the functions for analysis are not generic, 
# then one cannot decouple the functions for regression (every user
# would have to follow the constraint imposed by the regression module (Whtich isn't generic).

# The basic steps we repeat are the following:
# 1. loading diary
# 2. loading ohs
# 3. loading income
# 4. translating all of the fields of the srurvey into generic field names to be read by regression - some grouping for secondary fields is completed at this stage
# 5. merge all the files into one data-frame - along with creating more secondary fields
# With the goals, cited in the previous section, it would appear that the generic function should load any number of files and use translation mappings 
# provided in a structure populated by the user. These generic functions would tbe input by the user. This could just remain a set of functions interconnected with
# each other. For example, if the total expenditure of a household is a derived (secondary) field then the user must provide a way to combine the fields in the file
# (the fields themselves are selected by the user). This would be a simple function composed of other functions (this is the idea behind get_translated). Still,
# the framework needs to be provide a generic map that would be populated. This map is what needs to be called by the framework (the framework cannot know the
# name of the functions that the users declare). I currently propose is a computer scientist's favourite strucuture - a tree. One quickly observes that the one function
# that we want the root of the tree is merge_hh_ohs_income_data - this takes hh,ohs, income and depar as inputs frame and creates the final frame with derived fields
# that serve as input for the regressions. The user needs to provide the following entities to the framework (which would merely dfs through the passed tree structure):
# 1. File readers as data source as nodes of the tree.
# 2. Tree vertices made out of the nodes - every vertex has children and parent. This obvisouly requires every vertex to know it's children which can get data from. 
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
test<-function(){
  root<-create_root(2)
  root<-add_child(parent=root,data=3)
  root<-add_child(parent=root,data=4)
  root$children[[1]]<-add_child(parent=root$children[[1]],data=5)
  root$children[[1]]<-add_child(parent=root$children[[1]],data=6) # can't pass an arbitrary index
  return(root)
}