
class Variable:

    def __init__(self,x):
        self.x = x
        
    def get(self):
        """ get function can be defined by variables other than Variable """
        return self.x

    def __add__(self,y):
        
        if isinstance(y,Variable):
            return Variable(self.x + y.get())
        else:
            return Variable(self.x + y)

    def __mul__(self,y):
        if isinstance(y,Variable):
            return Variable(self.x * y.get())
        else:
            return Variable(self.x * y)

    def __sub__(self,y):
        if isinstance(y,Variable):
            return Variable(self.x - y.get())
        else:
            return Variable(self.x - y)


    def __str__(self):
        return  str(self.x)


a = Variable("x")
b = Variable("y")
print a+b
