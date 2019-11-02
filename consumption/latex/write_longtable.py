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

def print_table(df, longtable, landscape):
	section = "longtable" if longtable else "tabular"
	
	colsize = len(df.columns)
	nrows   = df.shape[0]
	start = "\\begin{landscape}" if landscape else ""

	if not longtable:
		start += "\\resizebox{\\columnwidth}{!}{"

	start += "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi} \\begin{" + section + "}{l*{"+str(colsize)+"}{c}} "
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
	end += "\\\\ \\end{" + section + "}"

	if not longtable:
		end += "}"

	if landscape:
	    end += "\\end{landscape}" 

	return start + end


if __name__ == "__main__":
	#df = pd.DataFrame( {'A':[1,2],'B':["GBP","USD"]})
	df  = pd.read_csv(sys.argv[1])
	#print(df)
	print(print_table(df,True,True))