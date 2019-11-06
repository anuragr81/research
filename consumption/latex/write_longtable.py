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

def print_table(dfinput,idcolumnIndex, longtable, landscape,selected_columns=None,legend=False):

	df = dfinput if selected_columns is None else dfinput[selected_columns]
	
	section = "longtable" if longtable else "tabular"
	
	colsize = len(df.columns)
	nrows   = df.shape[0]
	start = "\\begin{landscape}" if landscape else ""

	if not longtable:
		start += "\\resizebox{\\columnwidth}{!}{"

	start += "\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi} \\begin{" + section + "}{l*{"+str(colsize)+"}{c}} "
	start += "\\hline\\hline "
	
	for i,col in enumerate(df.columns):
	
		if i>idcolumnIndex:
			start += " & \\multicolumn{1}{c}{("+str(i)+")}"
		else:
			start += " & \\multicolumn{1}{c}{}"
	
	start += " \\\\"
	
	for i,col in enumerate(df.columns):
		if i>idcolumnIndex:
		    start += " & \\multicolumn{1}{c}{"+str(col)+"}"
		else:
			start += " & \\multicolumn{1}{c}{}"

	start += "\\\\ \\hline"
	dat = df.to_dict()
	for i in range(nrows):
		start += "\\\\"
		row = [dat[col][i] for col in df.columns]
		for c in row:
			start += " & " + format_data(c)


	end = ""
	if legend:
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
	#selected_columns = ['idname','qfruitsveg', 'qVfruitsveg', 'qVprotein', 'qnonfresh',
	# 'qcomplements', 'qVcomplements', 'qdensefoods', 'qVdensefoods',  
	# 'qtransport', 'qVnonfresh', 'qhousehold', 'qenergy', 'qprotein']
	#selected_columns = ["idname","qbeef","qbeer","qbread","qbunscakes","qcassavaflour","qcassavafresh","qcharcoal","qcoconut","qcookingoil","qdriedcannedfish","qelectricity","qfishseafood","qfreshmilk","qgreens","qkerosene","qmangoes","qonion","qpeanuts","qpotatoes","qpulses","qricehusked","qsalt","qsugar","qsweetpotato"]  
	df  = pd.read_csv(sys.argv[1],keep_default_na=False)
	if "idname" not in df.columns:
		print("Must have idname column")
		sys.exit(1)
	selected_columns = df.columns

	#print(df)
	print(print_table(dfinput=df,idcolumnIndex=0,longtable=False,landscape=False,selected_columns=selected_columns))
