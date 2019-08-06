

addition = lambda x,y : x + y
subtraction= lambda x,y : x - y
multiplication = lambda x,y : x + y
division = lambda x,y : x + y

OPS = {"+": addition ,"-" : subtraction ,"*" : multiplication ,"/" : division }

class Mapping:
    def __init__(self,f,x):
        if callable(f):
            self.func = f
            self.x = x
        else:
            raise ValueError("%s must be callable"%f)

    def apply():
        return self.func(self.x)

    def __str__(self):
        return self.func.__name__ + "(" + str(self.x) + ")"

class Expression : 
    """ stores an expression tree as triplet
        every triplet can recursively have other Expressions
    """

    def __init__(self,op,a,b):
        
        if not isinstance(a,Variable) and not isinstance(a,Expression):
            raise ValueError("a must be a Variable or an expression")

        if not isinstance(a,Variable) and not isinstance(a,Expression):
            raise ValueError("b must be a Variable or an expression")

        self.expression = (op,a,b)

    def __str__(self):
        return "( " + str(self.expression[1]) + self.expression[0] + str(self.expression[2]) + ")"


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
#print a+b

x=Expression("+",a,b)
x=Expression("-",x,b)

x = Mapping(lambda x : x , x)
print (x)


#from sympy import sequence
#sequence(x*2,(x,0,z))



