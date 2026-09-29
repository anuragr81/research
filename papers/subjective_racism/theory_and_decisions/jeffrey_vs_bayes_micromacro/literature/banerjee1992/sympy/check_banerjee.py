"""
Banerjee, A.V. (1992), "A Simple Model of Herd Behavior", QJE 107(3):797-817.

Checks, in exact rational / symbolic arithmetic:
  (1) Introduction restaurant example (pp.798-799): prior 51/49, equal-quality
      signals; person 2 with a B signal after an A choice goes to A, so her
      choice is uninformative and everyone follows.
  (2) Lemma 1 (p.805): the two posterior weights derived from the model of
      Sec. II (alpha = Pr(signal), beta = Pr(signal true), false signals uniform
      on [0,1]).  The first weight matches the paper.  The paper's second weight
      carries an extra factor beta; the derived weight is
      alpha^2 beta (1-beta)(1-alpha).  The lemma's inequality holds either way.
  (3) The herd probability (p.808, repeated p.810)
          Pi = [1 - alpha(1-beta)]^{-1} (1-alpha)(1-beta)
      as the sum of the geometric series of the herding path, as the limit of the
      exact finite-N probability under decision rule D (Proposition 1), and as a
      lower bound for every finite N; Pi decreasing in alpha and beta; Pi -> 1 as
      beta -> 0 (p.808, p.800 item 2).
  (4) The contrast with independent choices (p.800 item 2, pp.808-809): no one
      correct with probability (1-alpha beta)^N -> 0; fraction correct -> alpha beta.
  (5) The D* formula (p.810): 1 - (1-ab)^{n-1} - (n-1)(1-ab)^{n-2} ab is the
      probability that AT LEAST TWO of the first n-1 agents HAVE received the true
      signal (the text says "have not received").
  (6) The welfare comparison (pp.810-811): per-capita normalisation.
Exit status 0 iff every check passes.
"""
import sys
from fractions import Fraction as Fr
import sympy as sp

ok = True


def check(name, cond, detail=""):
    global ok
    print(("PASS " if cond else "FAIL ") + name + (("  -- " + detail) if detail else ""))
    if not cond:
        ok = False


a, b = sp.symbols("alpha beta", positive=True)

# ---------------------------------------------------------------------------
# (1) restaurant example
# ---------------------------------------------------------------------------
print("(1) restaurant example")
q = sp.symbols("q", positive=True)  # common signal quality, q > 1/2
prior_A = sp.Rational(51, 100)
postA = prior_A * q * (1 - q) / (prior_A * q * (1 - q) + (1 - prior_A) * (1 - q) * q)
check("one A signal and one B signal cancel: posterior = prior = .51 > 1/2",
      sp.simplify(postA - prior_A) == 0 and prior_A > sp.Rational(1, 2))

# ---------------------------------------------------------------------------
# (2) Lemma 1
# ---------------------------------------------------------------------------
print("\n(2) Lemma 1")
# H: persons 1 and 2 chose ibar != 0, person 3 has signal i' (not ibar).
# Hypothesis i* = ibar:
#   person 1 informed with true signal: a*b
#   person 2 chose ibar: uninformed (1-a) [follows 1] or informed-true a*b
#   person 3 informed with a false signal landing at i': a*(1-b) (density 1)
w_bar = (a * b) * ((1 - a) + a * b) * (a * (1 - b))
# Hypothesis i* = i':
#   person 1 informed with false signal at ibar: a*(1-b) (density 1)
#   person 2 chose ibar: only uninformed (a false signal hits ibar w.p. 0;
#     a true signal i' would make her choose i'): (1-a)
#   person 3 informed with true signal: a*b
w_prime = (a * (1 - b)) * (1 - a) * (a * b)
paper_bar = a**3 * b**2 * (1 - b) + a**2 * b * (1 - b) * (1 - a)
paper_prime = a**2 * b * (1 - b) * (1 - a) * b          # as printed, p.805
check("Lemma 1: derived weight for i*=ibar equals the paper's", sp.expand(w_bar - paper_bar) == 0)
check("Lemma 1: derived weight for i*=i' is a^2 b (1-b)(1-a) (paper prints an extra factor b)",
      sp.expand(w_prime - a**2 * b * (1 - b) * (1 - a)) == 0 and sp.expand(w_prime - paper_prime) != 0)
check("Lemma 1: w_bar - w_prime = a^3 b^2 (1-b) > 0 (follow the pair)",
      sp.expand(w_bar - w_prime - a**3 * b**2 * (1 - b)) == 0)
# the herd-following third person's action is uninformative: under D she
# chooses ibar with probability 1 whether i* = ibar or i* is anything not yet chosen
print("NOTE the third agent's herd choice has likelihood 1 under both hypotheses (rule D item 4);"
      " formalized in Lean as `herd_follow_uninformative`.")

# ---------------------------------------------------------------------------
# (3) herd probability Pi
# ---------------------------------------------------------------------------
print("\n(3) herd probability Pi")
Pi = (1 - a) * (1 - b) / (1 - a * (1 - b))
kk = sp.symbols("k", integer=True, nonnegative=True)
r = a * (1 - b)
for K in range(1, 7):
    partial = (1 - b) * sum(r**j * (1 - a) for j in range(K))
    check(f"K={K}: partial herding-path sum = (1-b)(1-a)(1-r^K)/(1-r), r = a(1-b)",
          sp.simplify(partial - (1 - b) * (1 - a) * (1 - r**K) / (1 - r)) == 0)
check("hence the full series (|r| < 1) sums to Pi = (1-a)(1-b)/(1-a(1-b))",
      sp.simplify((1 - b) * (1 - a) / (1 - r) - Pi) == 0)


def exact_chain(al, be, N):
    """Exact distribution after N agents under rule D, on the four-state
    abstraction: S0 (only i=0 chosen), Open (distinct wrong options, no herd),
    Herd (a wrong option chosen twice; absorbing), Found (i* chosen; absorbing)."""
    S0, Op, Hd, Fd = Fr(1), Fr(0), Fr(0), Fr(0)
    for _ in range(N):
        S0, Op, Hd, Fd = (S0 * (1 - al),
                          (S0 + Op) * al * (1 - be),
                          Hd + Op * (1 - al),
                          Fd + (S0 + Op) * al * be)
    return S0, Op, Hd, Fd


for al, be in ((Fr(1, 2), Fr(1, 3)), (Fr(9, 10), Fr(1, 10)), (Fr(1, 5), Fr(3, 4))):
    pi = (1 - al) * (1 - be) / (1 - al * (1 - be))
    nobody = [1 - exact_chain(al, be, N)[3] for N in (1, 2, 5, 10, 50, 200)]
    check(f"alpha={al}, beta={be}: Pr(no one correct | N) >= Pi for every N",
          all(x >= pi for x in nobody), f"Pi = {float(pi):.6f}")
    check(f"alpha={al}, beta={be}: Pr(no one correct | N=200) -> Pi",
          abs(float(nobody[-1] - pi)) < 1e-12, f"{float(nobody[-1]):.12f}")

dPi_a = sp.simplify(sp.diff(Pi, a))
dPi_b = sp.simplify(sp.diff(Pi, b))
check("dPi/dalpha = -b(1-b)/(1-a(1-b))^2 < 0", sp.simplify(dPi_a + b * (1 - b) / (1 - a * (1 - b))**2) == 0)
check("dPi/dbeta = -(1-a)/(1-a(1-b))^2 < 0", sp.simplify(dPi_b + (1 - a) / (1 - a * (1 - b))**2) == 0)
check("Pi -> 1 as beta -> 0 (p.800: 'as large as we like')", sp.limit(Pi, b, 0) == 1)

# ---------------------------------------------------------------------------
# (4) independent choices
# ---------------------------------------------------------------------------
print("\n(4) independent choices")
for al, be in ((Fr(1, 2), Fr(1, 3)), (Fr(9, 10), Fr(1, 10))):
    check(f"alpha={al}, beta={be}: Pr(no one correct) = (1-ab)^N -> 0 while Pi stays",
          (1 - al * be)**500 < Fr(1, 10**6) and (1 - al) * (1 - be) / (1 - al * (1 - be)) > Fr(1, 10))
print("NOTE independent choices: each agent correct w.p. alpha*beta, so the fraction correct -> alpha*beta (p.809)")

# ---------------------------------------------------------------------------
# (5) D* formula
# ---------------------------------------------------------------------------
print("\n(5) D* formula, p.810")
n = sp.symbols("n", integer=True, positive=True)
t = a * b
formula = 1 - (1 - t)**(n - 1) - (n - 1) * (1 - t)**(n - 2) * t
for nn in range(2, 9):
    atleast2 = sum(sp.binomial(nn - 1, j) * t**j * (1 - t)**(nn - 1 - j) for j in range(2, nn))
    check(f"n={nn}: formula = Pr(Bin(n-1, ab) >= 2) (at least two HAVE the true signal)",
          sp.expand(formula.subs(n, nn) - atleast2) == 0)

# ---------------------------------------------------------------------------
# (6) welfare normalisation
# ---------------------------------------------------------------------------
print("\n(6) welfare comparison, pp.810-811")
N_, z, eps, nE = sp.symbols("N z epsilon n_eps", positive=True)
lb = z * (N_ - nE) * (1 - eps) / N_
check("z[N - n(eps)](1-eps)/N -> z(1-eps) (per capita), not N(1-eps) as printed",
      sp.limit(lb, N_, sp.oo) == z * (1 - eps))
print("NOTE the printed upper bound zN[1-Pi] is a total, the lower bound a per-capita amount;"
      " the comparison goes through per capita: z(1-eps) > z(1-Pi) once eps < Pi")

print("\nAll checks passed." if ok else "\nSOME CHECKS FAILED.")
sys.exit(0 if ok else 1)
