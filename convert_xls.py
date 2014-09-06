
import csv
import sys
import xlrd

filepath=sys.argv[1]
sheet_num=int(sys.argv[2])
outfilepath=sys.argv[3]

bk=xlrd.open_workbook(filepath)
sh=bk.sheet_by_index(sheet_num)
types={}

with open(outfilepath,'w') as csvout:
  cw = csv.writer(csvout,delimiter=',')
  for row in range(0,sh.nrows):
    outr=list()
    for col in range(0,sh.ncols):
       #ch=(sh.cell_value(row,col)).encode('ascii','ignore')
       ch=(sh.cell_value(row,col))
       if isinstance(ch,float) or isinstance (ch,int):
          ch=str(ch)
       else:
          ch=ch.encode('ascii','ignore')
       if (type(ch) not in types): 
            types[type(ch)]=[]
       outr.append(ch)
    cw.writerow(outr)
