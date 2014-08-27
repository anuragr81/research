import csv
import sys
import re
import operator


def replacefunc (string):
   return string.replace("\n"," ")


def remove_emptycells(tagvalues):
   last_empty=len(tagvalues)-1
   while (len(tagvalues[last_empty])==0):
        last_empty=last_empty-1
   return (tagvalues[1:last_empty])

tagslist=["Period Ending","Levered Free Cash Flow"]

csvfilepath=sys.argv[1]
outfilepath=sys.argv[2]

stripfunc = operator.methodcaller('strip')

datadict=dict()


with open(csvfilepath,'r') as csvfile:
    r = csv.reader(csvfile)
    for line in r:
          if len(line)>0:
               for tag in tagslist:
                  res=re.search(tag,line[0]);
                  if (res):
                      tagname = line[0].strip()
                      tagvalues = map(replacefunc,map(stripfunc,map(str,line[1:len(line)])))
                      tagvalues=remove_emptycells(tagvalues);
                      datadict[tagname]=tagvalues

with open(outfilepath,'w') as csvout:
     cw = csv.writer(csvout,delimiter=',')
     for key in datadict.keys():
        outr=list()
        outr.append(tagname);
        outr=outr+datadict[key]
        cw.writerow(outr)
