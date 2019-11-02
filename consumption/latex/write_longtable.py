import pandas as pd
import re,sys

def format_data (x):
	if isinstance(x,int):
		return str(x)
	if isinstance(x,float):
		return str(round(x,4))
	if isinstance(x,str):
		return re.sub("_","\\_",x)
	raise RuntimeException("Unsupported Type to be formatted")

def start_longtable(df):

	colsize = len(df.columns)
	nrows   = df.shape[0]
	start = "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi} \\begin{longtable}{l*{"+str(colsize)+"}{c}} "
	start += "\\hline\\hline "
	for i,col in enumerate(df.columns):
		start += " & \\multicolumn{1}{c}{("+str(i+1)+")}"
	start += " \\\\"
	for i,col in enumerate(df.columns):
		start += " & \\multicolumn{1}{c}{"+str(col)+"}"
	start += "\\\\ \\hline"
	dat = df.to_dict()
	for i in range(nrows):
		start += "\\\\"
		row = [dat[col][i] for col in df.columns]
		for c in row:
			start += " & " + format_data(c)


	end = ""
	end += "\\\\ \\hline\\hline  \\multicolumn{2}{l}{\\footnotesize \\textit{p}-values in parentheses}" 
	end += "\\\\ \\multicolumn{2}{l}{\\footnotesize \sym{*} \\(p<0.05\\), \\sym{**} \\(p<0.01\\), \\sym{***} \\(p<0.001\\)}"
	end += "\\\\ \\end{longtable}" 

	return start + end


if __name__ == "__main__":
	#df = pd.DataFrame( {'A':[1,2],'B':["GBP","USD"]})
	df  = pd.read_csv(sys.argv[1])
	#print(df)
	print(start_longtable(df))