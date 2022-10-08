
def print_commodity(c):
    print ("\\begin{multiline*}")

    print("q"+c+":\\\\")
    print("\\rho_{1,"+c+",\\chi}d_{\\chi}+\\rho_{1,"+c+",\\xi}d_{\\xi}+\\gamma_{1,"+c+",fat}ln(P_{fat})\\\\")
    print("+\\gamma_{1,"+c+","+c+"}ln(P_{"+c+"})+\\gamma_{1,"+c+",cereals}ln(P_{cereals})\\\\")
    print("+\\gamma_{1,"+c+",veg}ln(P_{veg})+\\gamma_{1,"+c+",milk}ln(P_{milk})\\\\")
    print("+\\gamma_{1,"+c+",starches}ln(P_{starches})+\\gamma_{1,"+c+",complements}ln(P_{complements})\\\\")
    print("+\\gamma_{1,"+c+",tubers}ln(P_{tubers})+\\gamma_{1,"+c+",fruits}ln(P_{fruits})\\\\")
    print("+\\gamma_{1,"+c+",fish}ln(P_{fish})+\\beta_{"+c+",1}ln(x)+\\epsilon_{"+c+",1}")





print_commodity("meatsproteins")
