"""
Shared model definitions for PAPER_B_MANUSCRIPT.tex
("Order-Sensitivity of Belief and Decision Statistics under Jeffrey Conditioning").

Conventions (Section 2.1 of the manuscript):
  * Two binary attributes (A, B) in {0,1}^2.  A belief is a 2x2 joint law Q,
    with row index i = value of A and column index j = value of B.
  * The prior P is parametrised by its marginals P(A=0)=alpha, P(B=0)=beta and
    the prior covariance c:

        P = [[ alpha*beta + c        , alpha*(1-beta) - c     ],
             [ (1-alpha)*beta - c    , (1-alpha)*(1-beta) + c ]]

  * A soft cue on A delivers a credence q = (q0, q1) on A's partition;
    a soft cue on B delivers r = (r0, r1).  q1 = 1-q0, r1 = 1-r0.
  * Jeffrey step on A:  (J_A Q)(i,j) = q_i * Q(i,j) / Q(A=i).
  * Bayes-factor benchmark: PB(i,j) prop. P(i,j) * (q_i/P(A=i)) * (r_j/P(B=j)).
  * assoc(Q) = Q00*Q11 - Q01*Q10   (Definition 1).
  * Z := alpha*beta*(1-alpha)*(1-beta).
"""

import sympy as sp

# ---------------------------------------------------------------- symbols ---
alpha, beta, c = sp.symbols('alpha beta c', real=True)
q0, r0 = sp.symbols('q0 r0', real=True)
lam = sp.Symbol('lamda', real=True)          # mixing weight lambda in [0,1]

q1 = 1 - q0
r1 = 1 - r0
q = [q0, q1]
r = [r0, r1]

Z = alpha * beta * (1 - alpha) * (1 - beta)

# The open cube on which all genericity statements are made.
GENERIC = [(alpha, sp.Rational(1, 3)), (beta, sp.Rational(1, 4)),
           (q0, sp.Rational(2, 5)), (r0, sp.Rational(5, 7))]


# ------------------------------------------------------------- the model ---
def prior(cc=c):
    """The prior P of Section 2.1, as a function of the covariance."""
    return sp.Matrix([[alpha * beta + cc,       alpha * (1 - beta) - cc],
                      [(1 - alpha) * beta - cc, (1 - alpha) * (1 - beta) + cc]])


def _cancel_mat(Q):
    """Put every entry over a common denominator in lowest terms.

    Composing Jeffrey steps builds nested fractions whose un-cancelled form
    blows up; cancelling eagerly after each step keeps every expression small
    and makes equality testing a cheap `cancel`.
    """
    return sp.Matrix(2, 2, lambda i, j: sp.cancel(sp.together(Q[i, j])))


def marg_A(Q):
    """[Q(A=0), Q(A=1)] -- row sums."""
    return [Q[0, 0] + Q[0, 1], Q[1, 0] + Q[1, 1]]


def marg_B(Q):
    """[Q(B=0), Q(B=1)] -- column sums."""
    return [Q[0, 0] + Q[1, 0], Q[0, 1] + Q[1, 1]]


def jeffrey_A(Q, qq=None):
    """Jeffrey step on A's partition: reset A-marginal to q, hold B|A fixed."""
    qq = q if qq is None else qq
    mA = [sp.cancel(x) for x in marg_A(Q)]
    return _cancel_mat(sp.Matrix(2, 2, lambda i, j: qq[i] * Q[i, j] / mA[i]))


def jeffrey_B(Q, rr=None):
    """Jeffrey step on B's partition: reset B-marginal to r, hold A|B fixed."""
    rr = r if rr is None else rr
    mB = [sp.cancel(x) for x in marg_B(Q)]
    return _cancel_mat(sp.Matrix(2, 2, lambda i, j: rr[j] * Q[i, j] / mB[j]))


def bayes(Q, qq=None, rr=None):
    """
    Bayes-factor benchmark P^B: one update on the combined Bayes-factor content
    of both cues, the likelihoods being matched against the *prior* marginals
    of Q.  Sequence-invariant by construction (Wagner 2002).
    """
    qq = q if qq is None else qq
    rr = r if rr is None else rr
    mA = [sp.cancel(x) for x in marg_A(Q)]
    mB = [sp.cancel(x) for x in marg_B(Q)]
    W = sp.Matrix(2, 2, lambda i, j: Q[i, j] * (qq[i] / mA[i]) * (rr[j] / mB[j]))
    return _cancel_mat(W / sp.cancel(sum(W)))


def bayes_single_A(Q, qq=None):
    """Benchmark update on the A-cue alone."""
    qq = q if qq is None else qq
    mA = [sp.cancel(x) for x in marg_A(Q)]
    W = sp.Matrix(2, 2, lambda i, j: Q[i, j] * (qq[i] / mA[i]))
    return _cancel_mat(W / sp.cancel(sum(W)))


def assoc(Q):
    """Cross-attribute association (Definition 1)."""
    return Q[0, 0] * Q[1, 1] - Q[0, 1] * Q[1, 0]


def odds_ratio(Q):
    return (Q[0, 0] * Q[1, 1]) / (Q[0, 1] * Q[1, 0])


# ------------------------------------------------- the three posteriors ----
def posteriors(cc=c):
    """(P^J_AB, P^J_BA, P^B) at covariance cc."""
    P = prior(cc)
    PJ_AB = jeffrey_B(jeffrey_A(P))     # A read first, then B
    PJ_BA = jeffrey_A(jeffrey_B(P))     # B read first, then A
    PB = bayes(P)
    return PJ_AB, PJ_BA, PB


def mean_belief(PJ_AB, PJ_BA):
    """Pbar_lambda = lambda * P^J_AB + (1-lambda) * P^J_BA."""
    return lam * PJ_AB + (1 - lam) * PJ_BA


# --------------------------------------------- route directions R1, R2 -----
R1 = sp.Matrix([[q0, -q0], [q1, -q1]])          # q (x) (1,-1)
R2 = sp.Matrix([[r0, r1], [-r0, -r1]])          # (1,-1) (x) r
J = sp.ones(2, 2)

kappa = (alpha - q0) * r0 * (1 - r0) / Z
kappa_p = (beta - r0) * q0 * (1 - q0) / Z

grad_assoc_indep = sp.Matrix([[q1 * r1, -q1 * r0],
                              [-q0 * r1,  q0 * r0]])   # grad assoc at q (x) r

indep = sp.Matrix(2, 2, lambda i, j: q[i] * r[j])      # q (x) r


# ------------------------------------------------------------ utilities ----
# Every quantity in this model is a *rational function* of (alpha,beta,c,q0,r0),
# so `cancel(together(.))` is a decision procedure for equality and is far
# cheaper than sympy's generic `simplify`.  We use it throughout.

def simp(e):
    return sp.cancel(sp.together(e))


def zero(e):
    """Decide whether a rational expression is identically zero."""
    return sp.cancel(sp.together(e)) == 0


def taylor_coeff(expr, k):
    """Coefficient of c^k in the Taylor expansion of expr about c=0."""
    e = sp.together(expr)
    for _ in range(k):
        e = sp.cancel(sp.diff(e, c))
    return sp.cancel(e.subs(c, 0) / sp.factorial(k))


def mat_taylor_coeff(Mx, k):
    return sp.Matrix(2, 2, lambda i, j: taylor_coeff(Mx[i, j], k))


def order_in_c(expr, upto=3):
    """
    Leading order of expr in c at c=0.  Returns (k, coeff) with expr =
    coeff*c^k + higher and coeff != 0; returns (None, 0) if expr vanishes to
    order `upto`.
    """
    for k in range(0, upto + 1):
        coeff = taylor_coeff(expr, k)
        if coeff != 0:
            return k, coeff
    return None, sp.Integer(0)


def order_in_c_at_generic(expr, upto=4):
    """
    Leading order in c of `expr` after fixing (alpha,beta,q0,r0) at the generic
    rational point GENERIC.  This is the computational meaning of the paper's
    "Theta(c) generically": the leading coefficient is nonzero off a
    lower-dimensional set, witnessed here at one exact rational point.
    Substituting first keeps everything univariate and cheap.
    """
    e = sp.cancel(sp.together(expr).subs(GENERIC))
    for k in range(0, upto + 1):
        d = e
        for _ in range(k):
            d = sp.cancel(sp.diff(d, c))
        coeff = sp.cancel(d.subs(c, 0) / sp.factorial(k))
        if coeff != 0:
            return k, coeff
    return None, sp.Integer(0)


def frob(X, Y):
    """Entrywise (Frobenius) inner product <X, Y>."""
    return sp.cancel(sum(X[i, j] * Y[i, j] for i in range(2) for j in range(2)))


def at_generic(expr):
    """Evaluate at the generic numeric point GENERIC (exact rationals)."""
    return sp.cancel(sp.together(expr).subs(GENERIC))


def nonzero_at_generic(expr):
    return at_generic(expr) != 0


# --------------------------------------------------------- test harness ----
class Check:
    def __init__(self, title):
        self.title = title
        self.failures = []
        self.n = 0
        print("\n" + "=" * 78 + "\n" + title + "\n" + "=" * 78)

    def __call__(self, name, ok, detail=""):
        self.n += 1
        status = "PASS" if ok else "FAIL"
        print(f"  [{status}] {name}" + (f"\n         {detail}" if detail and not ok else ""))
        if not ok:
            self.failures.append(name)
        return ok

    def eq(self, name, lhs, rhs):
        d = simp(lhs - rhs)
        return self(name, d == 0, f"difference = {d}")

    def ne(self, name, expr, detail="nonzero at the generic point"):
        """Assert expr is not identically zero, witnessed at GENERIC."""
        v = at_generic(expr)
        return self(name, v != 0, f"value at generic point = {v}")

    def mat_eq(self, name, X, Y):
        D = sp.Matrix(X) - sp.Matrix(Y)
        d = [simp(e) for e in D]
        return self(name, all(e == 0 for e in d), f"difference = {d}")

    def done(self):
        print(f"\n  -> {self.n - len(self.failures)}/{self.n} checks passed")
        if self.failures:
            print("  -> FAILURES: " + ", ".join(self.failures))
        return len(self.failures) == 0
