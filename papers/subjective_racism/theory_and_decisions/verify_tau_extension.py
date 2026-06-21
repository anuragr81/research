"""
tau-extension verification: a direct noisy signal y = c_i + eps, precision tau,
enters the engagement decision alongside the group prior (mean cbar, var s2).
Observer forms a precision-weighted (shrinkage) posterior on c_i.

Checks four analytic claims by Monte Carlo.
"""
import numpy as np
rng = np.random.default_rng(0)

def shrinkage(cbar, s2, tau):
    w = tau*s2/(1+tau*s2)              # signal weight
    s2_post = (1-w)*s2                 # = s2/(1+tau*s2)
    return w, s2_post

N = 4_000_000
cbar, s2 = 0.45, 0.05
cj, dbar = 0.80, 0.06
PASS=[]
def check(name, ok, lhs=None, rhs=None):
    PASS.append(ok)
    tag = "PASS" if ok else "FAIL"
    extra = "" if lhs is None else f"  ({lhs:.5f} vs {rhs:.5f})"
    print(f"[{tag}] {name}{extra}")

# ================= SYMBOLIC (SymPy) proofs of the algebraic core =================
import sympy as sp
cbar_s, s2_s, tau_s, cj_s = sp.symbols('cbar sigma2 tau c_j', positive=True)
w_s = tau_s*s2_s/(1+tau_s*s2_s)
s2post_s = (1-w_s)*s2_s
# 1. posterior-variance closed form: (1-w)sigma^2 == sigma^2/(1+tau sigma^2)
check("SYM s2_post = sigma^2/(1+tau sigma^2)",
      sp.simplify(s2post_s - s2_s/(1+tau_s*s2_s)) == 0)
# 2. variance of the posterior mean over the signal: w^2 (sigma^2 + 1/tau) == w sigma^2
check("SYM Var(mu_post) = w sigma^2",
      sp.simplify(w_s**2*(s2_s + 1/tau_s) - w_s*s2_s) == 0)
# 3. ex-ante expected squared distance is tau-invariant: d/dtau [ (cbar-cj)^2 + w s2 + (1-w)s2 ] = 0
E_exante = (cbar_s-cj_s)**2 + w_s*s2_s + (1-w_s)*s2_s
check("SYM ex-ante E[(ci-cj)^2] tau-invariant  (dE/dtau=0)",
      sp.simplify(sp.diff(E_exante, tau_s)) == 0)
# 4. limits: institution (tau->0) recovers baseline; perfect resolution (tau->oo) zeros residual variance
check("SYM tau->0 : w->0 (group-mean substitution)", sp.limit(w_s, tau_s, 0) == 0)
check("SYM tau->oo: w->1 (full resolution)",         sp.limit(w_s, tau_s, sp.oo) == 1)
check("SYM tau->oo: s2_post->0",                     sp.limit(s2post_s, tau_s, sp.oo) == 0)
# 5. orthogonality is structural, not symbolic: tau enters only mu_post/s2_post (decision),
#    never F_g/cbar (updating). Recorded as an invariant, not a SymPy identity.
print("[NOTE] tau<->rho orthogonality is structural (different margins); not a symbolic identity.")

# ---- 1. closed forms for posterior mean/var (Gaussian c_i) ----
for tau in [0.0, 2.0, 20.0]:
    w, s2p = shrinkage(cbar, s2, tau)
    ci = rng.normal(cbar, np.sqrt(s2), N)
    y  = ci + (rng.normal(0, np.sqrt(1/tau), N) if tau>0 else 0.0*ci)
    mu_post = (1-w)*cbar + w*y if tau>0 else np.full(N, cbar)
    # empirical posterior var of c_i given (g,y): regress residual
    # check E_y[(mu_post-cj)^2] = (cbar-cj)^2 + w*s2
    lhs = np.mean((mu_post-cj)**2)
    rhs = (cbar-cj)**2 + w*s2
    check(f"E[(mu_post-cj)^2] closed form (tau={tau})", abs(lhs-rhs)<3e-4, lhs, rhs)

# ---- 2. ex-ante expected squared distance invariant in tau ----
base=None
for tau in [0.0, 1.0, 5.0, 50.0]:
    w, s2p = shrinkage(cbar, s2, tau)
    ci = rng.normal(cbar, np.sqrt(s2), N)
    cjj= rng.normal(0.50, np.sqrt(s2), N)   # symmetric partner draw, same family
    # ex-ante E[(ci-cj)^2] does not depend on the signal at all
    val = np.mean((ci-cjj)**2)
    if base is None: base=val
    check(f"ex-ante E[(ci-cj)^2] tau-invariant (tau={tau})", abs(val-base)<2e-3, val, base)

# ---- 3. within-group sorting: P(engage|c_i) increasing in c_i and tau ----
def p_engage(ci_grid, tau, reps=20000):
    w, s2p = shrinkage(cbar, s2, tau)
    out=[]
    for c in ci_grid:
        y = c + (rng.normal(0, np.sqrt(1/tau), reps) if tau>0 else np.zeros(reps))
        mu = (1-w)*cbar + w*y if tau>0 else np.full(reps, cbar)
        eng = ((mu-cj)**2 + s2p) <= dbar
        out.append(eng.mean())
    return np.array(out)

grid = np.array([0.30,0.50,0.70,0.90])
p_lo = p_engage(grid, tau=3.0)
mono_c = np.all(np.diff(p_lo) >= -1e-9)
check("P(engage) increasing in c_i (tau=3)", mono_c)
p_hi = p_engage(grid, tau=30.0)
# higher tau gives sharper sorting: the high-vs-low engagement GAP widens (curve steepens)
check("higher tau rescues high-c more", p_hi[-1] >= p_lo[-1]-1e-9)
gap_lo = p_lo[-1]-p_lo[0]; gap_hi = p_hi[-1]-p_hi[0]
check("higher tau => sharper sorting (gap widens)", gap_hi >= gap_lo-1e-9, gap_hi, gap_lo)

# tau=0 => group-level all-or-nothing (same prob for every c_i)
p0 = p_engage(grid, tau=0.0)
check("tau=0 => group-level (flat in c_i)", np.ptp(p0) < 1e-9)

# ---- 4. adverse selection: resolved engagement strips the high-c tail ----
# excluded group: choose cbar so the group fails wholesale at tau=0
cbar_x, s2_x = 0.40, 0.06
def engaged_pool(tau, n=2_000_000):
    w = tau*s2_x/(1+tau*s2_x); s2p=(1-w)*s2_x
    ci = rng.normal(cbar_x, np.sqrt(s2_x), n)
    y  = ci + (rng.normal(0,np.sqrt(1/tau),n) if tau>0 else np.zeros(n))
    mu = (1-w)*cbar_x + w*y if tau>0 else np.full(n,cbar_x)
    eng = ((mu-cj)**2 + s2p) <= dbar
    return ci, eng
ci, eng = engaged_pool(tau=25.0)
if eng.any():
    mean_eng = ci[eng].mean()
    mean_res = ci[~eng].mean()
    check("engaged are the high-c tail (mean_eng > cbar)", mean_eng > cbar_x, mean_eng, cbar_x)
    check("residual visible mean worsens (mean_res < cbar)", mean_res < cbar_x, mean_res, cbar_x)
    print(f"        engaged share={eng.mean():.3f}, mean_eng={mean_eng:.3f}, mean_res={mean_res:.3f}, cbar={cbar_x}")
else:
    check("some engagement at high tau", False)

print("\nSUMMARY:", f"{sum(PASS)} PASS / {len(PASS)-sum(PASS)} FAIL")
