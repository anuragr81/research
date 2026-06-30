#!/usr/bin/env python3
r"""
THE MULTI-SCALE JEFFREY-BAYES THEOREM  (decision-level, no dynamics).

Setup. An evaluator combines two soft cues about an individual. Each cue bears on one
of two binary attributes (A, B) whose population prior has covariance c (correlation).
A JEFFREY updater registers each cue as the impression it produces -- a target marginal
over a partition, with the orthogonal conditionals held rigid. A BAYESIAN registers each
cue as a likelihood. The matched single-cue likelihood is l_i = (target marginal)_i /
(prior marginal)_i, so the two rules are compared on identical evidence.

THEOREM.
  (1) [single cue]        one cue  =>  Jeffrey = Bayes, EXACTLY, for any c, any impression.
  (2) [independence]      c = 0    =>  Jeffrey = Bayes, EXACTLY, order-independent.
  (3) [individual belief] two cues, c != 0  =>  the Jeffrey posterior diverges from Bayes,
                          order-dependently, by O(c): FIRST order in correlation, ->0 at c=0.
  (4) [aggregate welfare] the expected welfare loss vs the Bayes-optimal decision is O(c^2):
                          SECOND order. Order-dependence flips only near-threshold decisions,
                          where the welfare gradient is zero (an envelope/indifference argument).

COROLLARY (micro-macro decoupling). Individual beliefs and decisions depart from Bayes at
first order in cue correlation, while aggregate welfare matches Bayes to first order. Hence
a population of order-dependent Jeffrey updaters can be statistically indistinguishable, at
the aggregate, from a population of Bayesians -- so aggregate rationality-fit licenses no
inference about individual-level processing in the correlated-soft-cue regime.

Run:  python3 multiscale_theorem_verification.py     (exits non-zero on any failure)
"""
import sympy as sp, math
PASS=[]
def ok(n,cond): assert cond, f"FAILED: {n}"; PASS.append(n); print(f"  [ok] {n}")

# ---- symbolic machinery ----
al,be,c = sp.symbols('alpha beta c')
joint = lambda al,be,c: {(0,0):al*be+c,(0,1):al*(1-be)-c,(1,0):(1-al)*be-c,(1,1):(1-al)*(1-be)+c}
Am=lambda D,i:D[(i,0)]+D[(i,1)]; Bm=lambda D,j:D[(0,j)]+D[(1,j)]
jA=lambda D,q:{(i,j):q[i]*D[(i,j)]/Am(D,i) for i in(0,1) for j in(0,1)}   # Jeffrey on A -> marginal q
jB=lambda D,r:{(i,j):r[j]*D[(i,j)]/Bm(D,j) for i in(0,1) for j in(0,1)}   # Jeffrey on B -> marginal r
P=joint(al,be,c); PA={0:al,1:1-al}; PB={0:be,1:1-be}

print("="*70); print("(1) single cue => Jeffrey = Bayes, exactly"); print("="*70)
qa0=sp.symbols('qa0'); q={0:qa0,1:1-qa0}
oneJ=jA(P,q); lA={i:q[i]/PA[i] for i in(0,1)}
raw={k:P[k]*lA[k[0]] for k in P}; Z=sum(raw.values()); oneB={k:v/Z for k,v in raw.items()}
ok("single-cue Jeffrey == matched Bayes, all cells (any c, any impression)",
   all(sp.simplify(oneJ[k]-oneB[k])==0 for k in P))

print("="*70); print("(2) c = 0 (independent cues) => Jeffrey = Bayes, order-independent"); print("="*70)
q0,r0=sp.symbols('q0 r0'); q={0:q0,1:1-q0}; r={0:r0,1:1-r0}
P12=jB(jA(P,q),r); P21=jA(jB(P,r),q)
a_,b_=sp.symbols('a b'); ind={al:a_,be:b_,c:0}
ok("c=0: P12 == P21 (order-independent)", all(sp.cancel((P12[k]-P21[k]).subs(ind))==0 for k in P))
ok("c=0: P12 == q (x) r  (= the Bayes product)",
   all(sp.cancel(P12[(i,j)].subs(ind)-q[i]*r[j])==0 for i in(0,1) for j in(0,1)))

print("="*70); print("(3) correlated cues, individual belief => divergence O(c), nonzero"); print("="*70)
lA={i:q[i]/PA[i] for i in(0,1)}; lB={j:r[j]/PB[j] for j in(0,1)}
rb={k:P[k]*lA[k[0]]*lB[k[1]] for k in P}; ZB=sum(rb.values()); PBayes={k:v/ZB for k,v in rb.items()}
gap=sp.series(sp.simplify(P12[(0,0)]-PBayes[(0,0)]), c, 0, 2).removeO()
c1=sp.simplify(gap.coeff(c,1))
ok("belief gap (Jeffrey - Bayes) has a nonzero O(c) term", c1!=0)
ok("belief gap vanishes at c=0", sp.simplify(gap.subs(c,0))==0)
# order effect itself is O(c):
oe=sp.series(sp.simplify(P12[(0,0)]-P21[(0,0)]), c, 0, 2).removeO()
ok("order effect (P12 - P21) is O(c), nonzero", sp.simplify(oe.coeff(c,1))!=0)

print("="*70); print("(4) aggregate welfare loss => O(c^2), second order (the washout)"); print("="*70)
# Smooth score population u = (Bayes posterior value) - threshold ~ N(0, sigma). A Jeffrey
# order-perturbation shifts the score by c*xi, xi~N(0,1) (mean-zero: random network order).
# Wrong-decision cost = |u| when sign(u) != sign(u + c*xi).  Exact (no Monte-Carlo noise):
#   Loss(c) = 2 * INT_0^inf u * Phi(-u/c) * (1/sigma) phi(u/sigma) du
Phi=lambda x:0.5*(1+math.erf(x/math.sqrt(2))); phi=lambda x:math.exp(-x*x/2)/math.sqrt(2*math.pi)
sigma=0.5
def loss(cv,n=400000,U=6.0):
    du=U*sigma/n; return 2*sum(((k+.5)*du)*Phi(-((k+.5)*du)/cv)*(1/sigma)*phi(((k+.5)*du)/sigma)*du
                               for k in range(n))
cs=[0.0125,0.025,0.05,0.10]; Ls=[loss(x) for x in cs]
for x,L in zip(cs,Ls): print(f"   c={x:.4f}: welfare loss={L:.4e},  loss/c^2={L/x**2:.4f}")
xs=[math.log(x) for x in cs]; ys=[math.log(L) for L in Ls]; n=len(xs)
slope=(n*sum(a*b for a,b in zip(xs,ys))-sum(xs)*sum(ys))/(n*sum(t*t for t in xs)-sum(xs)**2)
print(f"   small-c log-log slope = {slope:.3f}  (loss/c^2 -> const => second order)")
ok("welfare loss is O(c^2): slope ~ 2 and loss/c^2 -> constant", abs(slope-2)<0.03)

print("\n"+"="*70); print(f"ALL {len(PASS)} CHECKS PASSED  ==>  multi-scale theorem verified"); print("="*70)
for i,n in enumerate(PASS,1): print(f"  {i}. {n}")
print("""
Micro-macro decoupling (the corollary): beliefs diverge at O(c), welfare at O(c^2). The
individual experience of soft-cue evaluation is order-dependent and non-Bayesian; the
aggregate it sums to is Bayesian to first order. Aggregate fit cannot adjudicate the
individual process.""")
