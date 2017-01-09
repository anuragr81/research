import datetime
import json
import pandas as pd
import os
import sys

class StoreKey:
    def __init__(self,**kwargs):
        self.types= dict( (arg,type(kwargs[arg])) for arg in kwargs)
        self.values = dict((arg, kwargs[arg]) for arg in kwargs)


class DefaultFormatter:
    def write(self,storeKey,valuesList):
        json_convert = lambda x : x.strftime("%Y-%m-%d") if isinstance(x,datetime.date) or isinstance(x,datetime.datetime) else x
        entry= json.dumps({
            json.dumps( dict(
                zip( storeKey.values.keys(),
                     map(json_convert,storeKey.values.values())
                     )
            ) ):map(json_convert,valuesList)
        })
        return entry

    def convert_back(self,typeVar,valueVar):
        if typeVar == datetime.date or typeVar == datetime.date:
            return datetime.datetime.strptime(valueVar,"%Y-%m-%d")
        return valueVar

    def read(self,entry):
        entryDict=json.loads(entry)
        if not isinstance(entryDict,dict):
            raise ValueError("Parsing Error")
        keys=[]
        values=[]
        for entryKey in entryDict:
            keys.append(json.loads(entryKey))
            values.append(entryDict[entryKey])
        return (keys,values)

class LogWriter:
    def __init__(self,storeKey,fname=None,formatter=None):
        if not isinstance(storeKey,StoreKey):
            raise ValueError("param must be of type StoreKey")
        self.storeKey=storeKey
        self.formatter = formatter if formatter is not None else DefaultFormatter()
        self.fname = fname
        if self.fname is not None and os.path.exists(self.fname) and os.path.isfile(self.fname):
            print "removing file:",self.fname
            os.remove(self.fname)

    def log(self,key,*args,**kwargs):
        if not isinstance(key,StoreKey):
            raise ValueError("param must be of type StoreKey")

        if set(self.storeKey.types)!=set(key.types):
            raise ValueError("key mismatch")
        values=[]
        for arg in kwargs:
            values.append({arg:kwargs[arg]})
        for arg in args:
            values.append(arg)

        entry = self.formatter.write(storeKey=key,valuesList=values)
        if self.fname is None:
            print entry
        else:
            with open(self.fname,"a") as fh:
                print "writing entry:\"",entry,"\""
                fh.write(entry+"\n")


def merge_array(arr):
    if isinstance(arr,list) or isinstance(arr,tuple):
        return tuple(reduce(merge_data, arr))
    else:
        return tuple(arr)

def test_merge_array():
    print merge_array([{3: 4}, {4: 4}, {4: 5}])


class LogReader:
    def __init__(self,fname,formatter=None):
        with open(fname) as fh:
            self.lines = [line.strip() for line in fh.readlines()]
        self.formatter = formatter if formatter is not None else DefaultFormatter()


    def load(self,storeKey):
        if not isinstance(storeKey,StoreKey):
            raise ValueError("key must of type StoreKey")

        allKeys=[]
        allValues=[]
        for line in self.lines:
            (keys,values)=self.formatter.read(entry=line)
            allKeys+=keys
            allValues+=values
        # make sure data is compliant with the provided key
        #infer = lambda x : (x[0],self.formatter.convert_back(x[1],key.types[x[0]])) if x[0] in key.types else (x[0],x[1])
        #infer = lambda x: (x[0], x[1])
        #splitKeys = [map(infer,key) for key in allKeys]
        #print "splitKeys=",splitKeys
        dfDict = dict((k,[]) for k in storeKey.types)
        if 'value' in dfDict:
            raise ValueError("value keyword is reserved")
        dfDict['value']=[]
        for key in allKeys:
            for item in key.items():
                dfDict[str(item[0])].append(self.formatter.convert_back(storeKey.types[item[0]],item[1]))
        for value in allValues:
            dfDict['value'].append(json.dumps(value))

        rawDf=pd.DataFrame(dfDict)

        #TODO: aggregate the rawDf based on storeKey - using aggregate method or by creating a new df by
        #      iterating over the group
        return rawDf.groupby(storeKey.types.keys()).aggregate(merge_array)
        #return rawDf
        # plot can be shown with the following:
        #import matplotlib.pyplot as plt
        #plt.plot(pd.DataFrame({'one': [1, 2, 3, 4]}))
        #plt.show()



def merge_data(a,b):
    if isinstance(a,list):
        if isinstance(b,list) :
            return a+b
        else:
            return a+ [b]
    elif isinstance(a,dict):
        if isinstance(b,list):
            return b + [a]
        elif isinstance(b,dict):
            aKeys = set(a.keys())
            bKeys= set(b.keys())
            resDict = dict((k,merge_data(a[k],b[k])) for k in aKeys.intersection(bKeys))
            #print "Created - ", resDict
            #print "Updating with - ", dict((k,a[k]) for k in aKeys.difference(bKeys))
            resDict.update(dict((k,a[k]) for k in aKeys.difference(bKeys)))
            #print "Updating with - ", dict((k,b[k]) for k in bKeys.difference(aKeys))
            resDict.update(dict((k,b[k]) for k in bKeys.difference(aKeys)))
            return resDict
        else:
            raise ValueError("Cannot merge atomic value with a dictionary")
    else:
        if isinstance(b,list):
            return [a]+b
        elif isinstance(b,dict):
            raise ValueError("Cannot merge atomic value with a dictionary")
        else:
            return [a,b]

def test_merge_data():
    print (merge_data([1], 2) == [1, 2])
    print (merge_data([1], [2]) == [1, 2])
    print (merge_data(1, [2]) == [1, 2])
    print (merge_data(1, 2) == [1, 2])
    try:
        print (merge_data({1: 2}, 2))
    except ValueError as e:
        print e
    try:
        print (merge_data({1: 2}, 2))
    except ValueError as e:
        print e
    print (merge_data({1: 2}, {1: 2}))
    print (merge_data({1: 3}, {1: [3, 4], 2: 3}))
    print (merge_data({1: [3, 4], 2: 3}, {1: 3}))
    print (merge_data({1: 3, 3: 5}, {1: [3, 4], 2: 3}))
    print (merge_data({1: 3, 3: 5}, [1]))
    print (merge_data([1],{1: 3, 3: 5}))


if __name__=="__main__":
    #sys.exit(0)
    k=StoreKey(date=datetime.date(2015,1,5),tag="as")

    sampleStruct = {'a':[1,2,3],'b':3,
                 'c':{'k':1}
                 }
    fname = "c:/temp/test.log";
    toWrite = str(sys.argv[1]).lower() == "w"

    if toWrite:
        l = LogWriter(storeKey=k,fname=fname)
        l.log(StoreKey(tag="CAMP",date=datetime.date(2015,3,3)),2,3,x=3,y=[55,63])
        l.log(StoreKey(tag="CAMP",date=datetime.date(2015,3,3)),2,4,x=4,y=[67])
        l.log(StoreKey(tag="CAMP",date=datetime.date(2015,3,4)),2,4,x=4)
    else:
        l = LogReader(fname)
        df = l.load(StoreKey(tag="CAMP",date=datetime.date(2015,3,4)))
        print df


