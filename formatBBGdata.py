import csv
import os
import re
import sys

def searchline(sym,line):
   for i in range(0,len(line)):
      if line[i].strip() == sym.strip(): 
         return i
   return -1
    
inputfile=sys.argv[1]
outdir=sys.argv[2]

headline=open(inputfile).readlines()[0]
pattern="([^,]*US Equity)(.*)"

res=re.search(pattern,headline);
syms=list()
while res:
   syms.append(res.group(1));
   headline=res.group(2)
   res=re.search(pattern,headline);

if len(syms) == 0:
   sys.exit(1)

with open(inputfile,'r') as csvfile:
    r = csv.reader(csvfile)
    headline=None
    nextline=None
    count=0;
    for line in r:
        if count == 0:
            headline=line;
        if count == 1:
            nextline=line;
            break;
        count = count + 1

posmap={}

for sym in syms:
    pos=searchline(sym=sym,line=headline);
    startpos=pos;
    posmap[startpos]={'start':pos,'sym':sym};
    print "sym:",sym,"pos:",pos,"len(nl):",len(nextline)
    while(pos < len(nextline) and nextline[pos]!=""):
        pos = pos + 1
        print "at pos:",pos
    posmap[startpos]['end']=pos;

symmap={}
for pos in posmap:
  symmap[posmap[pos]['sym']]=list()

with open(inputfile,'r') as csvfile:
    r = csv.reader(csvfile)
    for line in r:
       for startpos in sorted(posmap.keys()):
          endpos=posmap[startpos]['end'];
          sym=posmap[startpos]['sym'];
          symmap[sym].append(line[startpos:endpos])

if os.path.exists(outdir):
   print ("dir " + str(outdir) + " exists. Exiting...")
   sys.exit(1)
else:
   os.mkdir(outdir)

for sym in symmap.keys():
    symfname = sym.replace(' ','_');
    with open(outdir.strip()+"/"+symfname+".out.csv",'w') as csvout:
       cw = csv.writer(csvout,delimiter=",")
       for entry in symmap[sym]:
            cw.writerow(entry)
