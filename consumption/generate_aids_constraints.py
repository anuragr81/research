#from collections import Counter

def generate_symmetry(categories):
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

    for r in finres:
        print(r[0]+"="+r[1])


#categ= ["Beef","Pork","Chicken","Goat"]
categ = ["densefoods","nonfresh"]

syms = generate_symmetry(categ)
