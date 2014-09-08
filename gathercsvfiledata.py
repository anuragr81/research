import csv
import os,re,sys,datetime


class DuplicateFileException(Exception):
  pass
class NonUniqueTimeFieldException(Exception):
  pass
class InvalidRowDataException(Exception):
  pass

datePrint         = lambda x : x.strftime("%Y-%m-%d")

displayTimeSeries = lambda x : str([map(datePrint,sorted(x.keys())),map(x.get,sorted(x))])

def processDate(value):
    startDate=datetime.datetime.strptime("1900-01-01","%Y-%m-%d")
    try : 
       tval = datetime.datetime.strptime(value,"%b-%d-%Y")
       return tval
    except : 
       try: 
         dval = int(float(value));
         return (startDate+datetime.timedelta(days=dval))
       except : 
         return "DATE{"+str(value)+"}"

def processFloat(value):
    try : 
       fval=float(value)
       return fval
    except:
       return "FLOAT{"+str(value)+"}"
    #return "1.0"

class Field: 
  def __init__(self,name,typename,alias,processFunc):
    self.name=name
    self.typename=typename
    self.alias=alias
    self.processFunc=processFunc


def getFieldsDict():
   bsfieldsdict=dict();

   f=Field(name="Total Shares Out. on Balance Sheet Date",typename=int,alias='shares',processFunc=processFloat)
   bsfieldsdict[f.alias]=f;
   f=Field(name="Balance Sheet as of:",typename=str,alias='date',processFunc=processDate)
   bsfieldsdict[f.alias]=f;

   icfieldsdict=dict();
   f=Field(name="For the Fiscal Period Ending",typename=int,alias='date',processFunc=processDate)
   icfieldsdict[f.alias]=f;
   f=Field(name="Earnings from Cont. Ops.",typename=float,alias='ebit',processFunc=processFloat)
   icfieldsdict[f.alias]=f;

   fieldsdict={'bs':bsfieldsdict,'ic':icfieldsdict}

   return fieldsdict

def createTSDict(keylist,listvallist):
   tsdict=dict()
   for key in keylist:
     if isinstance(key,datetime.datetime):
        tsdict[key]=list()

   for vallist in listvallist:
     if (len(keylist)!= len(vallist)): 
        print ("len(keylist)="+str(len(keylist))
            +" while len(vallist)="+str(len(vallist)))
        raise InvalidRowDataException()
     count=0
     for key in keylist:
        if isinstance(key,datetime.datetime):
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
              field = spfieldsdict[fieldkey]
              if line[0].strip() == field.name : 
                  if field.alias ==tsfield:
                    if len(keylist)>0:
                         raise NonUniqueTimeFieldException()
                    else:
                         keylist=map(field.processFunc,line[1:len(line)])
                  else:
                    listvallist.append(map(field.processFunc,line[1:len(line)]))
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
    print "<------BS------>"+displayTimeSeries(bstsdict)+"<-----/BS----->"
    ifile=dfiles[root]['ic']
    ictsdict = getTimeSeries(filename=ifile,spfieldsdict=fieldsdict['ic'],tsfield='date')
    print "<------IC------>"+displayTimeSeries(ictsdict)+"<-----/IC----->"
    sys.exit(1)
