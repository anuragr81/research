#!/usr/bin/python


class RVal:
    def __init__(x):
        self.x = x


class Expression :
    def __init__(self,x):
        self.x = x

    def __add__(self,x):
        self.x = self.x + x 
    

def equality ( lhs ,rhs ) :
    return lhs == rhs
    

if __name__ == "__main__":
    e=Expression(1)
    e + 2
    print e.x
    #print equality (1,"a")
