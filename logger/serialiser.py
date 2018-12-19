

from pprint import pprint, pformat
import time,logging,sys
import functools



LOGGER_NAME = "scheduler"
FORMAT = '%(asctime)-15s %(message)s'
logging.basicConfig(format=FORMAT)
logger = logging.getLogger(LOGGER_NAME)
logger.setLevel(logging.DEBUG)


def equals(x,y):
    if (type(x) != type(y)):
        return False
    if isinstance(x,list) or isinstance(x,tuple):
        if len(x)!= len(y):
            return False
        return not any ( not equals(xk,yk) for xk,yk in zip(x,y) )
    if isinstance(x,dict):
        if len(x)!= len(y):
            return False
        return not any ( not equals (x[k],y[k]) for k in x)

    if isinstance(x,float) or isinstance(x,int) or isinstance(x,str) or isinstance(x,bool) or isinstance(x,complex):
        return x==y

    import numpy as np
    import pandas as pd
    if isinstance(x,pd.DataFrame):
        # ensure the shape is the same
        if x.shape != y.shape:
            return False
        else:
            return not np.any(x != y)
    raise TypeError("Unknown type %s" % type(x))


def taskA(k):
    print("Calling A - %s" % k)
    return k

def taskB(m,n):
    res = m+n
    print ("Calling B - %s+%s = %s" % (m,n,res))
    return res

def taskC(a,b):
    res = a*b
    print ("Using (typically) taskA and taskB outputs - %s x %s = %s " % (a,b,res))
    return res


def name (x) :
    return (x.__module__ if x.__module__ is not None and len(x.__module__) >0 else "" ) + "." + x.__name__

def parse_arguments_dict (inputs,function):
    """

    :param inputs: dictionary of inputs
    :param function: function name
    :return: dictionary of arguments to be used for calling
    """
    if any(not isinstance(x, str) for x in inputs):
        # must a dictionary of callables as keys
        if any(not callable(x) for x in inputs):
            raise ValueError("input must be a dictionary with functions as keys or names of the arguments of "
                             "the function as keys")
        else:
            # all are callables
            if function not in inputs:
                raise ValueError("function not specified in inputs")
            else:
                return inputs[function]
    else:
        # all are strings
        return inputs


def is_lambda(v):
  TESTLAMBDA = lambda:0
  return isinstance(v, type(TESTLAMBDA )) and v.__name__ == TESTLAMBDA .__name__

class TaskScheduler:

    def __init__(self, grid= {}):
        self.tasks = []
        self.dependencies = {}
        self.functiondefs = {}
        self.grid = grid

    def get_parent (self, x):
        parents = [parent for parent, deplist in self.dependencies.items() if x in deplist]
        if len(parents) == 0:
            return None
        else:
            return parents

    def add_task(self, func, dependencies=[]):

        if not callable(func):
            raise ValueError("input must be a function")

        if is_lambda(func):
            raise ValueError("function should not be a lambda")

        if name(func) in self.tasks:
            raise ValueError("function %s already defined" % name(func))
        if name(func) in self.grid:
            raise ValueError("function %s already defined in the grid" % name(func))

        if not isinstance(dependencies, list) or any(not callable(x) for x in dependencies):
            raise ValueError("dependencies must be a list of functions")

        if any(is_lambda(x) for x in dependencies):
            raise ValueError("dependencies cannot be lambdas")

        if any (name(x) not in self.tasks for x in dependencies):
            raise ValueError("dependencies not defined")
        if name(func) in [name(y) for y in dependencies]:
            raise ValueError("function cannot be dependent on itself")

        if name(func) not in self.tasks and name(func) not in self.dependencies:
            self.tasks.append(name(func))
            self.functiondefs[name(func)]=func
            self.dependencies[name(func)] = [name(x) for x in dependencies]
        else:
            raise RuntimeError("Cannot add task %s " % func)


    def grid_reset(self,function,*args,**kwargs):
        if not callable(function) or is_lambda(function):
            raise ValueError("argument must be a function")
        if name(function) not in self.tasks:
            self.add_task(function,[])
        runargs = kwargs

        if 'store_args' in runargs:
            store_args = runargs['store_args']
            del runargs['store_args']
        else:
            store_args = []

        self.grid[name(function)] = function(*args,**runargs )

        return self.grid[name(function)]

    def grid_run(self, function, *args, **kwargs):
        if not callable(function) or is_lambda(function):
            raise ValueError("argument must be a function")

        if name(function)not in self.tasks:
            self.add_task(function, [])
        runargs = kwargs

        if 'store_args' in runargs:
            store_args = runargs['store_args']
            del runargs['store_args']
        else:
            store_args = []

        if name(function) not in self.grid:
            self.grid[name(function)] = function(*args,**runargs )

        return self.grid[name(function)]


    def grid_calc(self, function, inputs= {}, grid_inputs = {}, timeout_seconds = None):

        """
        :param function: the function to be evaluated
        :param inputs: the variable that stores the inputs to the function as dictionary with names as strings or
        as strings (no mixings is not allowed)
        :param timeout_seconds: time after which run would return regardless of whether the function has finished
        :return: the output of the function
        """

        if name(function) not in self.tasks:
            raise ValueError("Cannot run function which not defined as a task : %s" % name(function) )
        if any (not callable(x) for x in grid_inputs.values()) :
            raise ValueError("grid_inputs must be dictionary of functions in grid")
        if any(name(x) not in self.grid for x in grid_inputs.values()):
            raise ValueError("grid input %s not in grid" % [name(x) for x in grid_inputs.values() if name(x) not in self.grid ])

        args_dict_input = parse_arguments_dict(inputs=inputs, function=function)

        #TODO: check if function is a task and has dependencies
        runstatus_all_deps = dict((k, False) for k in self.dependencies[name(function)])

        start_time = time.time()
        logger.debug("Function: %s" % function + " has dependencies : %s" % [ x for x in self.dependencies[name(function)]] )
        while (timeout_seconds is None or time.time() - start_time < timeout_seconds ) and any(not v for v in runstatus_all_deps.values()):
            # populate dependencies grid if they are not populated
            for dep_func in self.dependencies[ name(function)]:
                if dep_func in self.grid:
                    runstatus_all_deps[dep_func] = True
                else:
                    # run the function is
                    dep_func_args = inspect.getfullargspec(self.functiondefs[dep_func]).args
                    #getargspec
                    func_args_not_in_inputs = set(dep_func_args) - set(args_dict_input.keys())
                    if len(func_args_not_in_inputs) > 0:
                        raise ValueError("The arguments for the function %s not found in the inputs are: %s" %
                                         (dep_func, func_args_not_in_inputs))
                    else:
                        kwargs = dict((arg, args_dict_input[arg]) for arg in dep_func_args)
                        self.grid[dep_func] = self.functiondefs[dep_func](**kwargs)
                        runstatus_all_deps[dep_func] = True
                        logger.debug("Calling function %s" % dep_func)

        # use grid to call the main function
        print("args_dict_input=%s" % args_dict_input)
        for k,v in grid_inputs.items():
            args_dict_input.update( {k:self.grid[name(v)]} )
        self.grid[name(function)] = function(**args_dict_input)
        return self.grid[name(function)]

    def parents(self):
        return [x for x in self.tasks if self.get_parent(x) == None]

def test_grid_calling():
    t = TaskScheduler()
    t.add_task(taskA)
    t.add_task(taskB,[taskA])
    t.add_task(taskC,[taskB,taskA])

    t.grid_calc(taskA, inputs = {'k': 1})
    t.grid_calc(taskB, inputs={'m': 1,'n':2})
    t.grid_calc(taskC, grid_inputs = {'b':taskB,'a':taskA})

    print(t.grid)

def test_calling():
    t = TaskScheduler()
    t.add_task(taskA)
    t.add_task(taskB,[taskA])
    t.add_task(taskC,[taskB,taskA])

    t.grid_reset(taskB,m=1,n=1)
    t.grid_reset(taskA,10)
    t.grid_run(taskA, k=10)

    print(t.grid)


store = TaskScheduler()

def do_for_bank(b,k):
    print ("do_for_bank - Using bank data : %s" %b)
    return b

def test_reuse():
    do_for_bank(10,k=2)
    functools.partial(do_for_bank,k=2)(20)
def node_(f):
    @wraps(f)
    def wrapper(*args, **kwargs):
        logger.debug("wrapper:: function= %s args = %s kwargs = %s" % (f, args, kwargs))
        return f(*args, **kwargs)
    return wrapper

from functools import  wraps


store_grid = {}


def get_arg_spec(f):
    vfirst,vsecond= sys.version.split(" ")[0].split(".")[0:2]
    if int(vfirst) == 2:
        from inspect import getargspec
        return getargspec(f).args
    elif int(vfirst) == 3:
        from inspect import getfullargspec
        return getfullargspec(f).args
    else:
        raise RuntimeError("Unsupported python version")


def get_full_args (f,args,kwargs):
    func_args = get_arg_spec(f)
    input_args = dict ( (func_args[i],args[i]) for i in range(0,len(args)))
    for k,v in kwargs.items():
        if k in input_args:
            raise RuntimeError("Invalid Input")
        input_args.update({k:v})
    return (tuple(func_args), input_args)

def serialise(x):
    if isinstance(x,int) or isinstance(x,float) or isinstance(x,str) :
        return x
    if isinstance(x,dict) :
        return repr( dict  ([ (serialise(k),serialise(v)) for k,v in x.items() ] ) )
    if isinstance(x,list):
        return repr([serialise(k) for k in x])
    if isinstance(x,pd.DataFrame):
        return x.to_json()
    return repr(x)

def node(f):
    @wraps(f)
    def wrapper(*args, **kwargs):

        func_args, input_args = get_full_args (f,args,kwargs)
        args_tuple = tuple (  serialise(input_args[argkey]) for argkey in func_args)

        logger.debug("wrapper:: function= %s args = %s kwargs = %s input_args=%s" % (f.__name__, args, kwargs, input_args))

        if f.__name__ not in store_grid:
            print("store_grid - adding %s" % (f.__name__))
            store_grid[f.__name__] = {}
        # might need to generalise the not in comparison
        matches = [ x for x in store_grid[f.__name__] if equals(args_tuple,x)]
        #if args_tuple not in store_grid[f.__name__]:
        if len(matches)==0:

            store_grid[f.__name__][args_tuple] = f(**input_args)
        elif len(matches) > 1:
            raise RuntimeError("Multiple entries for the same arguments")
        res = store_grid[f.__name__][args_tuple]

        def ff(*args_, **kwargs_):
            return res

        return ff(res)
    return wrapper

def grid_get(f,*args, **kwargs):
    func_args, input_args = get_full_args(f, args, kwargs)
    args_tuple = tuple(input_args[argkey] for argkey in func_args)


@node
def m():
    return 1

@node
def f(x,y,z):
    print("f::x=%s,y=%s,z=%s" % (x,y,z))
    return x

@node
def g(x):
    print("g::x=%s" % x)
    return x

if __name__ == "__main__":
    print (serialise({1:2}) )

    if True:
        #f(1, 2, z=2)
        #g(2)
        #f(2, y=3, z=4)
        f(2, {3:'T'}, 4)
        f(2, {3: 'T'}, 4)
        #g(3)
        #g(3)
        #print(m())

        pprint(store_grid)
    #do_for_markit(c)
    #create_function(taskC)
    #test_calling()

