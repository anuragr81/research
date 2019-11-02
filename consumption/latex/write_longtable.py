import pandas as pd

def start_longtable(df):

	colsize = len(df.columns)
	nrows   = df.shape[0]
	start = "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi} \\begin{longtable}{l*{"+str(colsize)+"}{c}} "
	start += "\\hline\\hline "
	for i,col in enumerate(df.columns):
		start += " &\\multicolumn{1}{c}{("+str(i+1)+")}"
	start += " \\\\"
	for i,col in enumerate(df.columns):
		start += " &\\multicolumn{1}{c}{"+str(col)+"}"
	start += "\\\\ \\hline"
	dat = df.to_dict()
	for i in range(nrows):
		start += "\\\\"
		row = [dat[col][i] for col in df.columns]
		for c in row:
			start += "& " + str(c)


	end = ""
	end += "\\\\ \\hline\\hline  \\multicolumn{2}{l}{\\footnotesize \\textit{p}-values in parentheses}" 
	end += "\\\\ \\multicolumn{2}{l}{\\footnotesize \sym{*} \\(p<0.05\\), \\sym{**} \\(p<0.01\\), \\sym{***} \\(p<0.001\\)}"
	end += "\\\\ \\end{longtable}" 

	return start + end


if __name__ == "__main__":
	df = pd.DataFrame( {'A':[1,2],'B':["GBP","USD"]})
	print(start_longtable(df))