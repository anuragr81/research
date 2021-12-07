
options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
fpath=args[1]

if (file.exists(fpath)){
	stop("NO file")
}

