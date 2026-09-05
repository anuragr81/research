import sympy as sp
R = sp.Rational

# States: C in {1,0}=cancer/no-cancer; E in {1,0}=mass-image present/absent;
#         F in {1,0}=cancer-like-cells present/absent.
# P(C)=.5; P(E|C), P(F|C) = .95/.05; conditionally independent given C.
PC = {1:R(1,2), 0:R(1,2)}
PE_given_C = {(1,1):R(95,100), (0,1):R(5,100), (1,0):R(5,100), (0,0):R(95,100)}  # (e,c)
PF_given_C = {(1,1):R(95,100), (0,1):R(5,100), (1,0):R(5,100), (0,0):R(95,100)}  # (f,c)

joint = {}
for c in (1,0):
    for e in (1,0):
        for f in (1,0):
            joint[(c,e,f)] = PC[c]*PE_given_C[(e,c)]*PF_given_C[(f,c)]
assert sp.simplify(sum(joint.values())-1)==0

def jeffrey_on_F(P, target_F1):
    mF1 = sum(P[k] for k in P if k[2]==1); mF0 = sum(P[k] for k in P if k[2]==0)
    out = {}
    for k in P:
        m = mF1 if k[2]==1 else mF0
        t = target_F1 if k[2]==1 else (1-target_F1)
        out[k] = P[k]/m*t
    return out

def jeffrey_on_E(P, target_E1):
    mE1 = sum(P[k] for k in P if k[1]==1); mE0 = sum(P[k] for k in P if k[1]==0)
    out = {}
    for k in P:
        m = mE1 if k[1]==1 else mE0
        t = target_E1 if k[1]==1 else (1-target_E1)
        out[k] = P[k]/m*t
    return out

def PofC1(P): return sum(P[k] for k in P if k[0]==1)

# Sequence 1: F first (target F=1 -> .10, i.e. lab tech's Q[~F]=.90), then E (target .90)
Qf = jeffrey_on_F(joint, R(10,100))
print("Q_f[C] =", sp.nsimplify(PofC1(Qf)), "=", float(PofC1(Qf)), " (Hawthorne: .14)")
Qfe = jeffrey_on_E(Qf, R(90,100))
print("Q_fe[C] =", sp.nsimplify(PofC1(Qfe)), "=", float(PofC1(Qfe)), " (Hawthorne: .68)")

# Sequence 2: E first (target .90), then F (target .10)
Qe = jeffrey_on_E(joint, R(90,100))
print("Q_e[C] =", sp.nsimplify(PofC1(Qe)), "=", float(PofC1(Qe)), " (Hawthorne: .86)")
Qef = jeffrey_on_F(Qe, R(10,100))
print("Q_ef[C] =", sp.nsimplify(PofC1(Qef)), "=", float(PofC1(Qef)), " (Hawthorne: .32)")

print()
print("order effect on C:", sp.nsimplify(PofC1(Qfe) - PofC1(Qef)), "=", float(PofC1(Qfe)-PofC1(Qef)))

print()
print("=== Hawthorne's commutation criterion: e,f commute iff neither influences the")
print("    other's basis marginal ===")
def PofE1(P): return sum(P[k] for k in P if k[1]==1)
def PofF1(P): return sum(P[k] for k in P if k[1]==1 or True) and sum(P[k] for k in P if k[2]==1)
print("prior P(E=1) =", PofE1(joint), " P(F=1) =", sum(joint[k] for k in joint if k[2]==1))
print("after F-update, P(E=1) shifts to:", PofE1(Qf), " (unchanged iff no cross-basis influence)")
print("after E-update, P(F=1) shifts to:", sum(Qe[k] for k in Qe if k[2]==1))

print()
print("=== Likelihood-Ratio (Field) Updating: same prior, LR-factor cues instead of ===")
print("    delivered credences -- Hawthorne's claim: this commutes exactly ===")

def PofC1_(P): return sum(P[k] for k in P if k[0]==1)

def lr_reweight_F(P, lr):   # LR[f,F,~F] multiplies the F=1 branch's odds by lr
    w = {k: P[k]*(lr if k[2]==1 else 1) for k in P}
    Z = sum(w.values())
    return {k: w[k]/Z for k in w}

def lr_reweight_E(P, lr):
    w = {k: P[k]*(lr if k[1]==1 else 1) for k in P}
    Z = sum(w.values())
    return {k: w[k]/Z for k in w}

Qf_lr = lr_reweight_F(joint, R(1,2))
print("Q_f[C] (LR model) =", sp.nsimplify(PofC1_(Qf_lr)), "=", float(PofC1_(Qf_lr)), " (Hawthorne: .35)")
Qfe_lr = lr_reweight_E(Qf_lr, 2)
print("Q_fe[C] (LR model) =", sp.nsimplify(PofC1_(Qfe_lr)), "=", float(PofC1_(Qfe_lr)), " (Hawthorne: .50)")

Qe_lr = lr_reweight_E(joint, 2)
Qef_lr = lr_reweight_F(Qe_lr, R(1,2))
print("Q_ef[C] (LR model, opposite order) =", sp.nsimplify(PofC1_(Qef_lr)), "=", float(PofC1_(Qef_lr)))
print("order-independent (Qfe == Qef):", sp.simplify(PofC1_(Qfe_lr) - PofC1_(Qef_lr)) == 0)
