

class InvalidTypeException(Exception):
   pass


def zeros(n):
   out=[]
   for i in range(0,n):
      out.append(0);
   return out

def printCurVisit(ll,listsToCompare,trackers):
   out = ""
   prefix = ""
   for iList in listsToCompare:
      if trackers[iList] != len(ll[iList]):
        out = out + prefix + str(ll[iList][trackers[iList]])
      prefix = ","
   #print "trackers:",trackers
   #print "latest:",out
   return out

def handleOutput(i):
   output = i
   print "output=",output

def compareList(ll):
   if not isinstance(ll,list):
     raise InvalidTypeException()
   for iL in ll:
      if not isinstance(iL,list):
        raise InvalidTypeException()
   listsToCompare = range(0,len(ll))
   trackers = zeros(len(ll)) # start with zero
   while True:
      printCurVisit(ll,listsToCompare,trackers)
      for iList in listsToCompare:
        #print "iList=",iList,"trackers[iList]=",trackers[iList],"len(ll[iList])=",len(ll[iList])
        if trackers[iList] == len(ll[iList]):
           listsToCompare.remove(iList)
           #print "removed:iList=",iList,"listsToCompare:",listsToCompare

      if len(listsToCompare)==0 :
         break;
      
      # in every loop check for the lowest in listsToCompare
      first_visit=True
      for iList in listsToCompare:  
         if first_visit:
            lowest = ll[iList][trackers[iList]] # safe because empty lists have been removed
            lowestList = iList
            first_visit=False
            continue;

         curElem = ll[iList][trackers[iList]]
         if curElem <= lowest:
            lowest = curElem
            lowestList = iList
      #print "lowestList=",lowestList     
      handleOutput(lowest)
      trackers[lowestList] = trackers[lowestList]+1


input=[[1,2],[10,11],[2,3,4]]
print "input:",input
compareList(input)

