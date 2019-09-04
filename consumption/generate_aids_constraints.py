#from collections import Counter

def generate_constraints(categories):
    results = []
    eqNames = [(c,"q"+c) for c in categories]
    logNames = [(c,"lp"+c.lower()) for c in categories ]
    for c1,lhs in eqNames :
         for c2,rhs in logNames:
             if c1!=c2:
                 rhsEq = [rhseq for rhsc,rhseq in eqNames if rhsc == c2]
                 rhsLog = [rhslog for rhsc,rhslog in logNames if rhsc == c1]
                 results.append(  ("["+lhs+"]"+rhs,"["+rhsEq[0]+"]"+rhsLog[0]) )
    revres = []
    for lhs,rhs in results:
        revres.append((rhs,lhs))
    finres = []
    while len(results)>0:
        entry = results[0]
        revres =[k for k in revres if k!=entry and k!=(entry[1],entry[0])]
        results=[k for k in results if k!=entry and k!=(entry[1],entry[0])]
        finres.append(entry)
    constcount = 1 
    for r in finres:
        print("constraint define " + str(constcount) + " " + r[0]+"="+r[1])
        constcount +=1 
    for _,eqn in eqNames:
        homgn=""
        prefix = ""
        for _,ln in logNames:
            homgn+= (prefix + "["+eqn+"]"+ln)
            prefix=" +"
        homgn+= " = 0"
        print("constraint define "+str(constcount) + " " + homgn)
        constcount +=1 
    print ("("+' '.join([str(x) for x in range(1,constcount)]) +")")


#categ= ["Beef","Pork","Chicken"]

#categ = ["densefoods","nonfresh"]
categ=["nonfresh","densefoods","complements","fruitsveg","protein","alcohol"]
syms = generate_constraints(categ)
