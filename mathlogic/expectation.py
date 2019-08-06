


def expectation_discrete(x,p):
    if len(x)!=len(p):
        raise ValueError("Unequal vectors x,p")
    s = 0
    for i in range(0,len(x)):
        s = s + p[i]*x[i]
    return s



