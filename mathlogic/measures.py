import itertools
from functools import reduce

class Discrete :
    def __init__(l):
        self.elems = l

def there_exists(f):
    x = general()
    # for a general x, if f can be False or True (both) 
    if f(x) == False:
        return False
    elif f(x) == True:
        return True
    else:
        return True



def is_element(x,A):
    return x in A

def sigma(X,A):
    """ 
    return True is A is a sigma algebra on X False otherwise
    """

    P = powerset(X)
    #A must be a family of subsets of X
    if len(set(X).difference(A))>0:
        return False
    #X in A
    if not is_element(X,A):
        return False
    #if a general A
    x = general()
    y = filter(x,lambda k: is_element(k,A))

    if forall(y,  is_element(set(X).difference(x), set(A))) :





def powerset(X):
    """ 
    return the powerset i.e. the union of all subsets of a discrete set X
    """
    ll = [ [ x for x in itertools.combinations(X,i)] for i in range(len(X)+1) ]
    return list(set(reduce(lambda x,y : x + y , ll , [])))



if __name__ == "__main__":
    print(p)


