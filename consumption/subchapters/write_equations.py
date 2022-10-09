import sys

def tostr_commodity(c):

    strout="\\rho_{1,"+c+",\\chi}d_{\\chi}+\\rho_{1,"+c+",\\xi}d_{\\xi}+\\gamma_{1,"+c+",fat}ln(P_{fat})\\\\" + "\n"
    strout=strout+"+\\gamma_{1,"+c+","+c+"}ln(P_{"+c+"})+\\gamma_{1,"+c+",cereals}ln(P_{cereals})\\\\" + "\n"
    strout=strout+"+\\gamma_{1,"+c+",veg}ln(P_{veg})+\\gamma_{1,"+c+",milk}ln(P_{milk})\\\\" + "\n"
    strout=strout+"+\\gamma_{1,"+c+",starches}ln(P_{starches})+\\gamma_{1,"+c+",complements}ln(P_{complements})\\\\" + "\n"
    strout=strout+"+\\gamma_{1,"+c+",tubers}ln(P_{tubers})+\\gamma_{1,"+c+",fruits}ln(P_{fruits})\\\\" + "\n"
    strout=strout+"+\\gamma_{1,"+c+",fish}ln(P_{fish})+\\beta_{"+c+",1}ln(x)+\\epsilon_{"+c+",1}"+"\n"
    return strout


def get_eq1_string(c):
    strout= ("\\begin{multline*}\n")
    strout=strout+"q"+c+":\\\\" + "\n"
    strout = strout+ "w_{"+c+"}="+tostr_commodity(c)
    strout=strout+"\\end{multline*}\n"
    return strout

def get_eq2_string(c):
    strout= ("\\begin{multline*}\n")
    strout=strout+"qV"+c+":\\\\" + "\n"
    strout = strout+ "\\ ln\\ V_{"+c+"}="+tostr_commodity(c)
    strout=strout+"\\end{multline*}\n"
    return strout


def get_symmetry_string(c,all_commodities,recorded_commodities):
    symeqstr="\\begin{gather*}"
    for j in all_commodities:
        if c!=j :
            if  not((c,j) in recorded_commodities or (j,c) in recorded_commodities):
                recorded_commodities.append((c,j))
                recorded_commodities.append((j,c))
                symeqstr=symeqstr+"\\gamma_{1,"+c+","+j+"}=\\gamma_{1,"+j+","+c+"}\\\\"

    symeqstr=symeqstr+"\\end{gather*}"
    return (symeqstr,recorded_commodities)


def get_all_symmetry_strings(commodities):
    recorded_commodities =[]
    strout=""
    for index,c in enumerate(commodities):
        result, recorded_commodities = get_symmetry_string(commodities[index],commodities,recorded_commodities)
        strout=strout+result
    return strout

commodities = ('meatsproteins','fat','cereals','veg','milk','starches','complements','tubers','fruits','fish',)

print(get_all_symmetry_strings(commodities))
sys.exit(0)
for c in commodities:
    a1 =get_eq1_string(c) 
    a2 =get_eq2_string(c)
    print(a1+a2)
