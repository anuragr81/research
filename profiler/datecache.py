

class DateCache:
   def __init__(self):
       self.dateslist = []
       self.dateindicesmap = {}

   def date(self,dt,i):
       ''' searches for dt and then hops back or forward '''
       index = self.dateindicesmap[dt] # must exist in the indices map
       return self.datelist[index+i] # hops back and forth as necessary

   def nearestDate(self,dt):
       raise Exception()


"""
What we need now is just 
i) rewriting of the formula
ii) removal of creating of new dictionaries in positions, nosh (let python garbage collect its own dictionaries as necessary)
iii) 

"""