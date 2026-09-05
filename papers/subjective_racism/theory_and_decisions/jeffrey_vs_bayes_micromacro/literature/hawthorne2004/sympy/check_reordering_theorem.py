import sympy as sp
R = sp.Rational

w1,w2,w3,w4 = R(1,10), R(1,5), R(3,10), R(2,5)
prior = {(1,1):w1, (1,2):w2, (2,3):w3, (2,4):w4}
assert sum(prior.values()) == 1

def margD(P): return {d: sum(P[k] for k in P if k[0]==d) for d in (1,2)}
def margE(P): return {e: sum(P[k] for k in P if k[1]==e) for e in (1,2,3,4)}
def jeffrey_D(P, targets):
    m = margD(P); return {k: P[k]/m[k[0]]*targets[k[0]] for k in P}
def jeffrey_E(P, targets):
    m = margE(P); return {k: P[k]/m[k[1]]*targets[k[1]] for k in P}

s1, s2 = R(3,4), R(1,4)   # D-update target: block-A total must match s1, block-B must match s2

print("=== Two DIFFERENT within-block splits, both commute exactly ===")
for (t1,t3,label) in [(R(1,10), R(1,20), "split A"), (R(1,2), R(1,5), "split B")]:
    t2, t4 = s1-t1, s2-t3
    targets = {1:t1,2:t2,3:t3,4:t4}
    route1 = jeffrey_E(jeffrey_D(prior, {1:s1,2:s2}), targets)
    route2 = jeffrey_D(jeffrey_E(prior, targets), {1:s1,2:s2})
    match = all(sp.simplify(route1[k]-route2[k])==0 for k in prior)
    r_blockA = t1/w1   # implied "r" for E=1 within block A (relative to prior)
    r_blockA_other = t2/w2  # implied "r" for E=2 -- DIFFERENT from r_blockA in general!
    print(f"{label}: t=({t1},{t2},{t3},{t4})  commutes: {match}   "
          f"internal ratios t1/w1={sp.nsimplify(r_blockA)}, t2/w2={sp.nsimplify(r_blockA_other)} (differ: {r_blockA!=r_blockA_other})")

print()
print("=== Confirm the ACTUAL necessary-and-sufficient condition is block-total matching,")
print("    not a single global Extended-Rigidity ratio ===")
t1,t2,t3,t4 = sp.symbols('t1 t2 t3 t4', positive=True)
route1 = jeffrey_E(jeffrey_D(prior, {1:s1,2:s2}), {1:t1,2:t2,3:t3,4:t4})
route2 = jeffrey_D(jeffrey_E(prior, {1:t1,2:t2,3:t3,4:t4}), {1:s1,2:s2})
eqs = [sp.Eq(route1[k], route2[k]) for k in prior]
sol = sp.solve(eqs[:3] + [sp.Eq(t1+t2+t3+t4,1)], [t1,t2,t3,t4], dict=True)[0]
print("solution:", sol, " -- i.e. t1+t2 = s1 =", s1, " and t3+t4 = s2 =", s2,
      ", with t1 and t3 each COMPLETELY FREE (no forced single ratio r across all four E_i)")
