#from collections import Counter
from itertools import product

def get_log_names (categories):
    return [(c,"lp"+c.lower()) for c in categories ]

def generate_constraints(categories,varlist):
    results = []
    eqNames = [(c,"q"+c) for c in categories]
    logNames = get_log_names(categories)
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
    
    print ('constraint define ' + str(constcount) + ' ' +  '+'.join(['['+v[1]+']'+v[0] for v in product(varlist,[e[1] for e in eqNames])]) + ' =0')
    constcount +=1
    print ('constraint define ' + str(constcount) + ' ' +  '+'.join(['['+x[1]+']ln_tot_exp ' for x in eqNames]) + ' =0')
    constcount +=1

    return ("("+' '.join([str(x) for x in range(1,constcount)]) +")")


def generate_equations(categslist, varlist):
    logNames = [x[1] for x in get_log_names(categslist)]

    for i,cat in enumerate(categslist):
        print ('global demand' + str(i+1) + ' "(' + 'q'+cat+": " + 'w_'+cat + ' ' +  ' ln_tot_exp ' + ' '.join(varlist) + ' ' +  ' '.join(logNames) +')"')  
    return ("sureg " + ' '.join(['$demand'+str(j+1) for j in range(0,len(categslist))])) # $demand1 $demand2 $demand3 $demand4 $demand5 $demand6 , const(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21) isure


#categ= ["Beef","Pork","Chicken"]

#categ = ["densefoods","nonfresh"]
#categ=["nonfresh","densefoods","complements","fruitsveg","protein","alcohol","energy","household","transport"]

#categ=['banana_green', 'banana_ripe', 'beef', 'beer', 'bread', 'brews', 'bunscakes', 'canned_milk', 'cassava_flour', 'cassava_fresh', 'charcoal', 'chicken', 'citrus', 'coconut', 'cooking_oil', 'dried_canned_fish', 'dried_canned_veg', 'eggs', 'electricity', 'fish_seafood', 'fresh_milk', 'gas', 'goat', 'greens', 'kerosene', 'maize_flour', 'maize_grain', 'maize_green', 'mangoes', 'millet_flour', 'millet_grain', 'onion', 'othervegstarch', 'pasta', 'peanuts', 'petrol', 'pork', 'potatoes', 'pulses', 'rice_husked', 'rice_paddy', 'salt', 'sugar', 'sugarcane', 'sweet_potato', 'tea', 'wheat', 'winespirits', 'yam']
#categ=['banana_green', 'banana_ripe', 'beef', 'beer', 'bread', 'bunscakes', 'canned_milk', 'cassava_flour', 'cassava_fresh', 'charcoal', 'chicken']
#categ = [ 'banana_ripe', 'beef', 'beer', 'bread', 'bunscakes', 'cassava_flour', 'cassava_fresh', 'charcoal', 'chicken','eggs', 'electricity', 'fish_seafood', 'fresh_milk', 'greens', 'kerosene',  'mangoes', 'maize_green',  'rice_husked', 'sweet_potato', 'tea', 'peanuts']
#categ=['banana_ripe', 'beef','beer']
#categ=['banana_green', 'banana_ripe', 'beef', 'beer', 'bread', 'bunscakes', 'canned_milk', 'cassava_flour', 'cassava_fresh', 'charcoal', 'chicken', 'citrus', 'coconut', 'cooking_oil', 'dried_canned_fish', 'dried_canned_veg', 'eggs', 'electricity', 'fish_seafood', 'fresh_milk', 'goat', 'greens', 'kerosene', 'maize_flour', 'maize_grain', 'maize_green', 'mangoes', 'millet_flour', 'millet_grain', 'onion', 'othervegstarch', 'pasta', 'peanuts', 'potatoes', 'pulses', 'rice_husked', 'salt', 'sugar', 'sugarcane', 'sweet_potato', 'tea', 'wheat', 'yam']
"""
1. rice_paddy removed
2. pork petrol gas brews winespirits removed
"""
# used but cancelled: categ=[ 'banana_ripe', 'beef', 'beer', 'bread', 'bunscakes', 'cassava_flour', 'cassava_fresh', 'charcoal', 'chicken','eggs', 'electricity', 'fish_seafood', 'fresh_milk', 'greens', 'kerosene',  'mangoes', 'maize_green',  'rice_husked', 'sweet_potato', 'tea', 'peanuts', 'coconut','yam']
#constraint error: categ=[ 'beef', 'beer', 'bread', 'bunscakes', 'cassava_flour', 'cassava_fresh', 'charcoal', 'coconut', 'cooking_oil', 'dried_canned_fish', 'electricity', 'fish_seafood', 'fresh_milk', 'greens', 'kerosene', 'mangoes', 'onion', 'peanuts', 'potatoes', 'pulses', 'rice_husked', 'salt', 'sugar', 'sweet_potato', 'tea']
#categ=[ 'beef', 'beer', 'bread', 'bunscakes', 'cassava_flour', 'cassava_fresh', 'charcoal', 'coconut', 'cooking_oil', 'dried_canned_fish', 'electricity', 'fish_seafood', 'fresh_milk', 'greens', 'kerosene', 'mangoes', 'onion', 'peanuts', 'potatoes', 'pulses', 'rice_husked', 'salt', 'sugar', 'sweet_potato']
#TODO: Add brews petrol, pork goat etc.
#varlist = ["educ_rank", "age", "occupation_rank", "consu", "invmills"]

#################### AGGREGATED AIDS (UNRESTRICTED METHOD) ####################

#categ = [ "nonfresh"    "densefoods"  "complements" "fruitsveg"   "protein"   ]
#categ = [ "nonfresh" , "Vnonfresh",  "densefoods", "Vdensefoods" , "complements", "Vcomplements", "fruitsveg", "Vfruitsveg",  "protein",  "Vprotein", "household", "transport"   ]
#deleted with regexp: \+\[[^\[\]]*\]lpvprotein etc. \+\[[^\[\]]*\]lpvprotein

if False:
    categ = [ "nonfresh" , "Vnonfresh",  "densefoods", "Vdensefoods" , "complements", "Vcomplements", "fruitsveg", "Vfruitsveg",  "protein",  "Vprotein", "energy", "household", "transport"   ]
    varlist = ["educ_rank", "age", "occupation_rank", "consu", "has_electricity"]
else:
    categ =["fat", "Vfat", "meatsproteins", "Vmeatsproteins", "cereals", "Vcereals", "veg", "Vveg", "milk", "Vmilk", "starches", "Vstarches", "complements", "Vcomplements", "tubers", "Vtubers", "fruits", "Vfruits", "fish", "Vfish"]     
    varlist = ["hsize", "hh_age", "expensiveregion"]


constraints = generate_constraints(categ,varlist)
eqns = generate_equations(categ,varlist)
print(eqns + ', const' + constraints + " isure" )
