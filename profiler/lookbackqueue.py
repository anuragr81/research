
import datetime



class Rules:
    def latestValue(self,instrument,today):
        return 1
    def shiftDate(self,schedule,shift,today):
        return today + datetime.timedelta(days=shift)

class LookQueueException:
	raise Exception()

class LookBackQueue:
    def __init__(self,schedule,instrument,lookbacksize,instance):
        self.schedule = schedule
        self.lookbacksize=lookbacksize
        self.instance = instance
        self.instrument = instrument
        self.dates=[]
        self.values=[]

    def populate(self,today):
        """ At the return of the function lookback queue must be exactly of size self.lookbacksize"""
        if len(self.dates)==0:
            new_date = today
            for i in xrange(0,self.lookbacksize):
                new_date = self.instance.shiftDate(self.schedule,-1,new_date)
                self.dates.insert(0,new_date)
                new_values = instance
        else:
           new_date = self.instance.shiftDate(self.schedule,-1,today)
           if new_date != self.dates[-1]:
               self.dates.append(new_date)
               self.dates.pop(0)
               new_value = self.instance.latestValue(self.instrument,today)
               self.values.append(new_value)
               self.values.pop(0)
       if len(self.dates)!=self.lookbacksize:
           raise LookQueueException()


if __name__ == "__main__":
    instance = Rules()
    q = LookBackQueue(schedule="default",lookbacksize=4,instrument="GBPUSD",instance=instance)
    q.populate(datetime.date(2011,1,1))
    print q.dates
    q.populate(datetime.date(2011,1,1))
    print q.dates
    