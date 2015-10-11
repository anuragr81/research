# In UK expenditure survey
# p51(set43) -> p344(set3)
# set114

# for personal incomes:
# deselecting retired and unoccupied
# dat[as.numeric(dat$V4)<20,]

load_expenditure_files<-function (set3_filename, set114_filename){
  winc = read.table(set3_filename);
  wdiary = read.table(set114_filename);
  winc_cases= as.character(winc$V1);
  wdiary_cases = as.character(wdiary$V1);
  cases = intersect(winc_cases,wdiary_cases);
  for (caseno in cases){
    income = as.numeric(as.character(
      winc[as.character(winc$V1)==as.character(caseno),]$V8)
    );
    print(paste("Weekly income for caseno",caseno," = ",income));
    
    # gathering data for week 1
    case_entries=wdiary[as.character(wdiary$V1)==as.character(caseno) & as.character(wdiary$V3)=="1",];
    print(case_entries)
    # from case_entries, sum data for every category (use plyr -ddply)
    # 
    print(as.numeric(as.character(case_entries$V7)));
  }
  
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
