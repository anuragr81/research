import sympy as sp

p, a1, a2 = sp.symbols('p alpha alpha_p', positive=True)

# eq (5): q as a function of alpha and p
def q_of_alpha(p, alpha):
    return p*sp.exp(alpha) / (p*sp.exp(alpha) + (1-p)*sp.exp(-alpha))

alpha = sp.symbols('alpha')
q = q_of_alpha(p, alpha)

# eq (4): alpha recovered from q, p -- check it's the inverse of (5)
alpha_recovered = sp.Rational(1,2)*sp.log((q/p)/((1-q)/(1-p)))
print("eq(4) inverts eq(5):", sp.simplify(alpha_recovered - alpha))

# --- Two-cue setting: F1..F4 = E&E', E&-E', -E&E', -E&-E' (Paper B's 2x2 cells) ---
F11,F10,F01,F00 = sp.symbols('F11 F10 F01 F00', positive=True)  # prior masses, sum to 1
al, ap = sp.symbols('alpha alpha_prime')   # tilt on E (basis 1), tilt on E' (basis 2)

def tilt_step1(F, al):
    # updates the E-partition: E={F11,F10}, -E={F01,F00}, exponential tilt by al
    w = {(1,1):F11*sp.exp(al), (1,0):F10*sp.exp(al), (0,1):F01*sp.exp(-al), (0,0):F00*sp.exp(-al)}
    Z = sum(w.values())
    return {k:w[k]/Z for k in w}

def tilt_step2(F, ap):
    # updates the E'-partition: E'={F11,F01}, -E'={F10,F00}
    w = {(1,1):F[(1,1)]*sp.exp(ap), (1,0):F[(1,0)]*sp.exp(-ap), (0,1):F[(0,1)]*sp.exp(ap), (0,0):F[(0,0)]*sp.exp(-ap)}
    Z = sum(w.values())
    return {k:w[k]/Z for k in w}

prior = {(1,1):F11,(1,0):F10,(0,1):F01,(0,0):F00}
route_EE = tilt_step2(tilt_step1(prior, al), ap)   # E first, then E'
route_EpE = tilt_step1(tilt_step2(prior, ap), al)  # E' first, then E

print()
print("Field eq(7) commutes exactly (route E-then-E' == route E'-then-E):")
for k in prior:
    diff = sp.simplify(sp.together(route_EE[k] - route_EpE[k]))
    print(f"  cell {k}: diff = {diff}")

# closed form matches his eq (7): P''(A&F_k) propto exp(sum of the two alphas with signs matching F_k)
closed = {(1,1):F11*sp.exp(al+ap), (1,0):F10*sp.exp(al-ap), (0,1):F01*sp.exp(-al+ap), (0,0):F00*sp.exp(-al-ap)}
Zc = sum(closed.values())
closed = {k:closed[k]/Zc for k in closed}
print()
print("route E-then-E' matches closed-form eq(7):")
for k in prior:
    print(f"  cell {k}: diff = {sp.simplify(route_EE[k]-closed[k])}")

# --- Contrast: literal Jeffrey (credence q,q' held fixed) does NOT commute in general ---
print()
print("=== Contrast: Jeffrey credence-input update does not commute ===")
q1, q2 = sp.symbols('q1 q2', positive=True)  # delivered credences on E, E' resp.

def jeffrey_step1(F, target):
    mE = F[(1,1)]+F[(1,0)]; mnE = F[(0,1)]+F[(0,0)]
    return {(1,1):F[(1,1)]/mE*target, (1,0):F[(1,0)]/mE*target,
            (0,1):F[(0,1)]/mnE*(1-target), (0,0):F[(0,0)]/mnE*(1-target)}

def jeffrey_step2(F, target):
    mE = F[(1,1)]+F[(0,1)]; mnE = F[(1,0)]+F[(0,0)]
    return {(1,1):F[(1,1)]/mE*target, (0,1):F[(0,1)]/mE*target,
            (1,0):F[(1,0)]/mnE*(1-target), (0,0):F[(0,0)]/mnE*(1-target)}

jr1 = jeffrey_step2(jeffrey_step1(prior, q1), q2)
jr2 = jeffrey_step1(jeffrey_step2(prior, q2), q1)
gap = sp.simplify(sp.together(jr1[(1,1)] - jr2[(1,1)]))
print("gap in cell (1,1), general prior/targets:", gap)

# numeric witness
num = gap.subs({F11:sp.Rational(1,10), F10:sp.Rational(2,10), F01:sp.Rational(3,10), F00:sp.Rational(4,10),
                q1:sp.Rational(7,10), q2:sp.Rational(3,10)})
print("numeric witness (nonzero => confirms asymmetry):", sp.nsimplify(num))
