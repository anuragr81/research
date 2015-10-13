# In UK expenditure survey
# p51(set43) -> p344(set3)
# set114

# for personal incomes:
# deselecting retired and unoccupied
# dat[as.numeric(dat$V4)<20,]

require(plyr)

calculateSuperCategory <-function (category){
  return (floor(as.numeric(as.character(category))*1e-4));
}

load_expenditure_files<-function (set3_filename, set114_filename){
  winc = read.table(set3_filename);
  wdiary = read.table(set114_filename);
  winc_cases= as.character(winc$V1);
  wdiary_cases = as.character(wdiary$V1);
  cases = intersect(winc_cases,wdiary_cases);
  
  print(wdiary[1,])
  
  results=data.frame();
  count = 1;
  for (caseno in cases){  
    
    income = as.numeric(as.character(
      winc[as.character(winc$V1)==as.character(caseno),]$V8)
    );
    
    
    if (!is.na(income)){
      print(paste("Weekly income for caseno",caseno," = ",income));
      
      # gathering data for week 1
      case_entries=wdiary[as.character(wdiary$V1)==as.character(caseno) & as.character(wdiary$V3)=="1",];
      
      # from case_entries, sum data for every category (use plyr -ddply)
      # the idea would be to i) append the super-category to the frame case_entries 
      
      case_entries$category = calculateSuperCategory(case_entries$V4);
      #print(case_entries);
      
      # and then ii) use ddply to aggregate
      dat = ddply(.data=case_entries,.variables=.(V1,V2,V3,category),summarize,
                  expenditure=sum(as.numeric(as.character(V7)))
      );
      results= rbind(results,dat);
      #stop("DONE");
      #print(as.numeric(as.character(case_entries$V7)));
    } #endif na check on income 
  }#end for
  write.csv(results,file='/tmp/results.csv')
}
run_regression<-function(x,w_i){
  # for every caseno i, we have a vector of w_i's (i being a commodity) and outlay x
  # if we have n casenos and k commodities
  # then w_i is a [nxk] vector
  #      x   is a [nx1] vector
  # The equation w_i = a_i + b_i log(x)
  # would imply running k regressions such that
  # w_1[] = a_1 + b_1 log(x)
  
}
#
#require(plyr)
#dfx <- data.frame(
#  group = c(rep('A', 8), rep('B', 15), rep('C', 6)),
#  sex = sample(c("M", "F"), size = 29, replace = TRUE),
#  age = runif(n = 29, min = 18, max = 54)
#)


fit_simple_engel<-function (){
  
}

fit_engel <- function(filename){
  dat = read.csv(filename);
  categories = as.vector(unique(dat$category));
  output = list ();
  for ( category in categories ) { 
    output[[category]] = 1
    category_cost = sum(dat[ dat$category == category ,]$Cost)
    print (paste("Cost of category - ",category," = ",category_cost))
  } 
  #print (output);
}
