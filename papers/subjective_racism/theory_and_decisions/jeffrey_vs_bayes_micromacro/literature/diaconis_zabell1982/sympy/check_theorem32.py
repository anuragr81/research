import sympy as sp
R = sp.Rational

def jeff1(P, ev, t):
    comp=[x for x in P if x not in ev]; mE=sum(P[x] for x in ev); mC=sum(P[x] for x in comp)
    return {x:(P[x]/mE*t if x in ev else P[x]/mC*(1-t)) for x in P}

Aset={(1,1),(1,0)}; Bset={(1,1),(0,1)}
def margA(P): return P[(1,1)]+P[(1,0)]
def margB(P): return P[(1,1)]+P[(0,1)]

print("=== Example 3.4 (Diaconis-Zabell 1982), reproduced on Paper B's 2x2 layout ===")
P0 = {(1,1):R(1,8), (1,0):R(1,4), (0,1):R(3,8), (0,0):R(1,4)}
p1, q1 = R(1,2), R(7,15)
EF = jeff1(jeff1(P0, Aset, p1), Bset, q1)
FE = jeff1(jeff1(P0, Bset, q1), Aset, p1)
print("route E-then-F:  margA =", margA(EF), " target p1 =", p1, " (holds, as DZ report)")
print("route F-then-E:  margB =", margB(FE), " target q1 =", q1, " (DZ: should NOT equal q1)")
print("DZ's paper states P_EF(E)=1/2=P_EF(Ebar), P_FE(F) != q1 -- both reproduced exactly")

print()
print("=== Theorem 3.2: P_EF == P_FE iff Jeffrey independence; c=0 case ===")
a,b,p,q,c = sp.symbols('alpha beta p q c', positive=True)
Pg = {(1,1):a*b, (1,0):a*(1-b), (0,1):(1-a)*b, (0,0):(1-a)*(1-b)}
route1 = jeff1(jeff1(Pg, Aset, p), Bset, q)
route2 = jeff1(jeff1(Pg, Bset, q), Aset, p)
print("independent prior (c=0): routes identical for all p,q:",
      all(sp.simplify(sp.together(route1[k]-route2[k]))==0 for k in Pg))

Pc = {(1,1):a*b+c, (1,0):a*(1-b)-c, (0,1):(1-a)*b-c, (0,0):(1-a)*(1-b)+c}
r1 = jeff1(jeff1(Pc, Aset, p), Bset, q)
r2 = jeff1(jeff1(Pc, Bset, q), Aset, p)
gap11 = sp.simplify(sp.together(r1[(1,1)]-r2[(1,1)]))
witness = gap11.subs({a:R(3,10), b:R(11,20), c:R(1,20), p:R(2,5), q:R(3,5)})
print("correlated prior (c!=0): gap in cell (1,1) is not identically zero;")
print("numeric witness at (alpha,beta,c,p,q)=(.3,.55,.05,.4,.6):", sp.nsimplify(witness))
