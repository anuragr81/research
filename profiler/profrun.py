import time

class UnknownFunctionException(Exception):
	pass
class DoubleTickException(Exception):
	pass
    
class ProfileCentre:
    def __init__ (self):
        self.store={}
        self.debug=False

    def tick(self,func):
    	this_visit=time.time()
    	if not isinstance(func,str):
            raise ValueError("func must be a string")
        
    	if func not in self.store:
    		#initialize the entry for profiler function
    		self.store[func]={}
    		self.store[func]['runcount']=0
    		self.store[func]['runtime_mean']=None
    		self.store[func]['is_running']=True
    	else:
    		if self.store[func]['is_running']:
    			raise DoubleTickException()
    	
    	self.store[func]['last_started']=this_visit
    	if self.debug:
    	    print "tick for ",func," at :",this_visit

    def tock(self,func):
    	tock_visit=time.time()
    	if not isinstance(func,str):
            raise ValueError("func must be a string")
        if func not in self.store:
        	raise UnknownFunctionException()
        #print func, "was started at ", self.store[func]['last_started'] , " finished at ", tock_visit
        this_run= tock_visit - self.store[func]['last_started']
        if self.debug:
        	print "tock for ",func," at :", tock_visit, " total time taken=",this_run
        if self.store[func]['runtime_mean'] is None:
        	self.store[func]['runtime_mean']=this_run
        else:
        	self.store[func]['runtime_mean']=(self.store[func]['runtime_mean']*self.store[func]['runcount'] + this_run)/(self.store[func]['runcount']+1)
        self.store[func]['runcount']=self.store[func]['runcount']+1
        self.store[func]['is_running']=False

    def printStats(self):
        for key in self.store.keys():
            print key,"-", self.store[key]
    def printStatsForFunc(self,func):
    	print func,"-",self.store[func]

class ProfilerContext:
	def __init__(self,centre,func):
		self.centre = centre
		self.func = func
	def __enter__(self):
		self.centre.tick(self.func)
	def __exit__(self,type, value, traceback):
		self.centre.tock(self.func)
		#self.centre.printStatsForFunc(self.func)

def runFunc(size):
    for i in range(1,1000):
            for j in range(1,1000):
                x = i+j
                x = x *x * x

def run2():
	runFunc(10000);

def run():
    c=ProfileCentre()
    for i in range(0,2	):
         c.tick('runFunc')
         runFunc(1000)
         c.tick('run2')
    
         run2()
         c.tock('run2')
    
         c.tock('runFunc') 
         
    #c.printStats()
    for i in xrange(0,3):
        with ProfilerContext(centre=c,func="runner") as fh:
    	    runFunc(4000)
    c.printStats()
if __name__ == "__main__":
    run()
