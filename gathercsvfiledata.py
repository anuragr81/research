
import csv
import os
import re
import sys


class DuplicateFileException(Exception):
  pass
class NonUniqueTimeFieldException(Exception):
  pass
class InvalidRowDataException(Exception):
  pass

class Field: 
  def __init__(self,name,typename,alias):
    self.name=name
    self.typename=typename
    self.alias=alias


def getFieldsDict():
   bsfieldsdict=dict();

   f=Field(name="Total Shares Out. on Balance Sheet Date",typename=int,alias='shares')
   bsfieldsdict[f.alias]=f;
   f=Field(name="Balance Sheet as of:",typename=str,alias='date')
   bsfieldsdict[f.alias]=f;

   icfieldsdict=dict();
   f=Field(name="For the Fiscal Period Ending",typename=int,alias='date')
   icfieldsdict[f.alias]=f;
   f=Field(name="Earnings from Cont. Ops",typename=float,alias='ebit')
   icfieldsdict[f.alias]=f;

   fieldsdict={'bs':bsfieldsdict,'ic':icfieldsdict}

   return fieldsdict

def createTSDict(keylist,listvallist):
   tsdict=dict()
   for key in keylist:
     tsdict[key]=list()

   for vallist in listvallist:
     if (len(keylist)!= len(vallist)): 
        print ("len(keylist)="+str(len(keylist))
            +" while len(vallist)="+str(len(vallist)))
        raise InvalidRowDataException()
     count=0
     for key in keylist:
        tsdict[key].append(vallist[count])
        count=count+1
   return tsdict

def getTimeSeries(filename,spfieldsdict,tsfield):
    with open(filename,'r') as csvfile:
       r  = csv.reader(csvfile)
       keylist=list()
       listvallist=list()
       for line in r:
          for fieldkey in spfieldsdict.keys():
              bsfield = spfieldsdict[fieldkey]
              if line[0].strip() == bsfield.name : 
                  if bsfield.alias ==tsfield:
                    if len(keylist)>0:
                         raise NonUniqueTimeFieldException()
                    else:
                         keylist=line[1:len(line)] 
                  else:
                    listvallist.append(line[1:len(line)])
    return createTSDict(keylist,listvallist)

dfdict={'bs':'_bs_filt.csv',
        'ic':'_out_inc.csv'}

dfiles=dict()

for filename in (os.listdir(".")):
   for dtype in dfdict.keys():
     filterext=dfdict[dtype]
     res = re.search("(.*)("+filterext+"$)",filename)
     if (res): 
         root=res.group(1)
         if root not in dfiles:
            dfiles[root]=dict()
         if dtype in dfiles[root]:
            raise DuplicateFileException()
         dfiles[root][dtype]=filename

fieldsdict=getFieldsDict()

for root in dfiles:
    bfile=dfiles[root]['bs']
    bstsdict = getTimeSeries(filename=bfile,spfieldsdict=fieldsdict['bs'],tsfield='date')
    ifile=dfiles[root]['ic']
    ictsdict = getTimeSeries(filename=ifile,spfieldsdict=fieldsdict['ic'],tsfield='date')
    print ictsdict

