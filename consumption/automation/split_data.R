library(foreign)
library(haven)

#options(echo=TRUE) # if you want see commands in output file
options(echo=T) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)

fpath=args[1]
split_field=args[2]
output_file_lo = args[3]
output_file_hi = args[4]

if (file.exists(fpath)){
  print(fpath)
  dat = as.data.frame(read_dta(fpath))
  vals =dat[,split_field]
  vals = vals[is.finite(vals)]
  medval = median(vals)
  
  print(paste("Median is ",medval))
  lodat=dat[dat[,split_field]<=medval,]
  hidat=dat[dat[,split_field]>medval,]
  
  write_dta(lodat,output_file_lo)
  write_dta(hidat,output_file_hi)
} else {
  stop("NO file")
}

