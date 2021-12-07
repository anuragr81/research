
#options(echo=TRUE) # if you want see commands in output file
options(echo=F) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
fpath=args[1]

data.frame ( fname = c("linear_nu_food_nonfood_costs_r_ngr.do") );
if (file.exists(fpath)){
fname = basename(fpath)
print("Processing")
} else {
	stop("NO file")
}

