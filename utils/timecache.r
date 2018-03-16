
from collections import OrderedDict 
class Cache :
    def __init__(self,sz,func):
        self._cache = OrderedDict()
        self._sz = sz
        self._func=func

    def get_value(self,t,i):
        
        if t not in self._cache:
            self._cache[t]=dict()
        if i not in self._cache[t]:
            self._cache[t][i]=self._func(t,i)

        if len(self._cache)>2  and (self._cache.keys()[-1]-self._cache.keys()[0]).days>self._sz:
            del self._cache[self._cache.keys()[0]]

        return self._cache[t][i]


if __name__ == "__main__":
    import math
    import datetime
    def f (t,i):
        print "calculating f(t=",t,",i=",i,")"
        return (datetime.date(2014,1,4)-t).days+i
    c = Cache(2,f)
    print c.get_value(datetime.date(2015,1,2),2)
    print c.get_value(datetime.date(2015,1,3),3)
    print c.get_value(datetime.date(2015,1,3),3)

    print c.get_value(datetime.date(2015,1,5),4)
    print c.get_value(datetime.date(2015,1,5),4)
    print c._sz
    

