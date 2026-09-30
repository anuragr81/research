"""
Lemma SEP (separability of attribute-local composites), Section 5.2.1,
proof in Appendix A.2.

    For any number N of binary attributes and any joint prior P on {0,1}^N,
    any finite sequence of attribute-local updates produces

        P'(x) = P(x) * prod_{a=1..N} g_a(x_a)

    for single-attribute functions g_a.  Equivalently, log(P'/P) contains no
    interaction terms at any level.

The appendix proof uses exactly one property of a Jeffrey step: the factor it
multiplies by,

        h_t(x_a) = q_{x_a} / P_{t-1}(A_a = x_a),

is a function of the single coordinate x_a -- state-dependent only through a
normaliser that is itself a function of x_a alone.  The verification mirrors
that structure:

  Part 1  (N = 2)      fully concrete: real marginals, fully symbolic prior and
                       symbolic credences, exact rational-function algebra.
  Part 2  (N = 2..6)   the appendix's own abstraction: each step's normaliser
                       is carried as an opaque symbol M[t][v] depending only on
                       the updated coordinate.  This is what makes the argument
                       work at every N, and it is checked at every N up to 6 --
                       both the product form and every interaction contrast.
  Part 3  (N = 3)      the converse half of the appendix: vanishing pairwise
                       (and higher) contrasts force additivity.
  Part 4               a scope check: dropping attribute-locality
                       (Assumption 3) breaks the lemma -- a joint-event cue
                       manufactures association out of independence.

The induction over the update sequence itself is formalised in Lean; see
../lean/JeffreyOrder/LemmaSEP.lean.
"""
import itertools
import random
import sympy as sp

from jeffrey_core import Check


def cells(N):
    return list(itertools.product([0, 1], repeat=N))


def marginal(P, a, val, N):
    return sum(P[x] for x in cells(N) if x[a] == val)


# --------------------------------------------------------------- contrasts --
def contrasts(phi, N, combine):
    """
    Delta_S phi for every S with |S| >= 2: the alternating combination over the
    sub-cube spanned by S, with the remaining coordinates held fixed
    (Appendix A.2).
    """
    out = {}
    for k in range(2, N + 1):
        for S in itertools.combinations(range(N), k):
            rest = [a for a in range(N) if a not in S]
            for restvals in itertools.product([0, 1], repeat=len(rest)):
                terms = []
                for signs in itertools.product([0, 1], repeat=k):
                    x = [0] * N
                    for a, val in zip(S, signs):
                        x[a] = val
                    for a, val in zip(rest, restvals):
                        x[a] = val
                    terms.append(((-1) ** (k - sum(signs)), phi[tuple(x)]))
                out[(S, restvals)] = combine(terms)
    return out


def additive_contrasts(phi, N):
    return contrasts(phi, N, lambda ts: sp.expand(sum(s * v for s, v in ts)))


def multiplicative_contrasts(ratio, N):
    """
    "Delta_S log(P'/P) = 0" is equivalent to the alternating *product* of P'/P
    over the sub-cube being 1.  Working multiplicatively keeps everything
    rational, where `cancel` is a decision procedure; sympy cannot combine logs
    of unfactored polynomials, but it can cancel their quotients.
    """
    def combine(ts):
        num = sp.prod([v for s, v in ts if s > 0])
        den = sp.prod([v for s, v in ts if s < 0])
        return sp.cancel(sp.together(num / den))
    return contrasts(ratio, N, combine)


# ------------------------------------------------ Part 1: concrete, N = 2 ---
def part1_concrete_N2(ck):
    N = 2
    w = {x: sp.Symbol("w" + "".join(map(str, x)), positive=True) for x in cells(N)}
    Zt = sum(w.values())
    P = {x: w[x] / Zt for x in cells(N)}

    seq = [0, 1, 0, 1, 1]                       # A, B, A, B, B
    u = sp.symbols('u0 u1 u2 u3 u4', positive=True)

    cur = dict(P)
    g = {a: {0: sp.Integer(1), 1: sp.Integer(1)} for a in range(N)}
    for t, a in enumerate(seq):
        tgt = {0: u[t], 1: 1 - u[t]}
        m = {v: sp.cancel(sp.together(marginal(cur, a, v, N))) for v in (0, 1)}
        for v in (0, 1):
            g[a][v] = sp.cancel(sp.together(g[a][v] * tgt[v] / m[v]))
        cur = {x: sp.cancel(sp.together(cur[x] * tgt[x[a]] / m[x[a]])) for x in cells(N)}

    rebuilt = {x: sp.cancel(sp.together(P[x] * g[0][x[0]] * g[1][x[1]])) for x in cells(N)}
    ck(f"N=2 concrete, sequence {seq}: P'(x) = P(x) g_0(x_0) g_1(x_1) exactly, "
       "for symbolic prior and symbolic credences",
       all(sp.cancel(sp.together(rebuilt[x] - cur[x])) == 0 for x in cells(N)))

    ratio = {x: sp.cancel(sp.together(cur[x] / P[x])) for x in cells(N)}
    inter = multiplicative_contrasts(ratio, N)
    bad = {k: v for k, v in inter.items() if v != 1}
    ck("N=2 concrete: the pairwise contrast of log(P'/P) vanishes", not bad, f"{bad}")

    ck("N=2 concrete: P' is a probability distribution", sp.cancel(sum(cur.values())) == 1)


# --------------------------------- Part 2: the appendix argument, any N -----
def part2_abstract(ck, Nmax=6):
    """
    Each attribute-local step multiplies by h_t(x_{a_t}) = tgt_{x_a} / M_{t,x_a}.
    The appendix uses only that h_t is a function of the single coordinate
    x_{a_t}; carrying the normaliser as an opaque symbol makes exactly that
    assumption and no other, so the check below is the appendix's induction
    evaluated at each N.
    """
    random.seed(20260901)
    for N in range(2, Nmax + 1):
        w = {x: sp.Symbol(f"w{N}_" + "".join(map(str, x)), positive=True)
             for x in cells(N)}
        P = {x: w[x] for x in cells(N)}          # unnormalised; ratios are what matter

        T = N + 3
        seq = [random.randrange(N) for _ in range(T)]
        # ensure every attribute is touched at least once
        for a in range(N):
            if a not in seq:
                seq[a] = a

        cur = dict(P)
        g = {a: {0: sp.Integer(1), 1: sp.Integer(1)} for a in range(N)}
        for t, a in enumerate(seq):
            tgt = {0: sp.Symbol(f"t{N}_{t}", positive=True), 1: sp.Symbol(f"s{N}_{t}", positive=True)}
            M = {v: sp.Symbol(f"M{N}_{t}_{v}", positive=True) for v in (0, 1)}
            h = {v: tgt[v] / M[v] for v in (0, 1)}       # depends on x_a alone
            for v in (0, 1):
                g[a][v] = g[a][v] * h[v]
            cur = {x: cur[x] * h[x[a]] for x in cells(N)}

        rebuilt = {x: P[x] * sp.prod([g[a][x[a]] for a in range(N)]) for x in cells(N)}
        ok = all(sp.cancel(sp.together(rebuilt[x] - cur[x])) == 0 for x in cells(N))
        ck(f"N={N} abstract, sequence {seq}: P'(x) = P(x) * prod_a g_a(x_a)", ok)

        ratio = {x: sp.cancel(sp.together(cur[x] / P[x])) for x in cells(N)}
        inter = multiplicative_contrasts(ratio, N)
        bad = {k: v for k, v in inter.items() if v != 1}
        ck(f"N={N} abstract: all {len(inter)} interaction contrasts of log(P'/P) "
           "vanish (|S| >= 2, every fixing of the other coordinates)",
           not bad, f"non-unit: {list(bad)[:3]}")


# ----------------------------- Part 2b: the abstraction is faithful ---------
def part2b_faithful(ck):
    """
    Confirm on an exact numeric instance (N = 3, rational prior and rational
    credences) that a real Jeffrey step's normaliser really is a function of
    the updated coordinate alone -- i.e. that the abstraction of Part 2 is not
    assuming away the content.
    """
    N = 3
    random.seed(7)
    w = {x: sp.Rational(random.randrange(1, 40), random.randrange(1, 17)) for x in cells(N)}
    Zt = sum(w.values())
    P = {x: w[x] / Zt for x in cells(N)}

    seq = [0, 2, 1, 0, 2]
    cur = dict(P)
    g = {a: {0: sp.Integer(1), 1: sp.Integer(1)} for a in range(N)}
    for t, a in enumerate(seq):
        u = sp.Rational(random.randrange(1, 10), 11)
        tgt = {0: u, 1: 1 - u}
        m = {v: marginal(cur, a, v, N) for v in (0, 1)}
        # the factor by which each cell is multiplied
        fac = {x: sp.cancel(tgt[x[a]] / m[x[a]]) for x in cells(N)}
        same = all(fac[x] == fac[y] for x in cells(N) for y in cells(N) if x[a] == y[a])
        ck(f"N=3 numeric step {t} on attribute {a}: the Jeffrey factor depends on "
           f"x_{a} alone", same)
        for v in (0, 1):
            g[a][v] = sp.cancel(g[a][v] * tgt[v] / m[v])
        cur = {x: sp.cancel(cur[x] * fac[x]) for x in cells(N)}

    rebuilt = {x: sp.cancel(P[x] * sp.prod([g[a][x[a]] for a in range(N)])) for x in cells(N)}
    ck("N=3 numeric: P'(x) = P(x) * prod_a g_a(x_a) exactly",
       all(sp.cancel(rebuilt[x] - cur[x]) == 0 for x in cells(N)))
    ck("N=3 numeric: P' is a probability distribution", sp.cancel(sum(cur.values())) == 1)

    ratio = {x: sp.cancel(cur[x] / P[x]) for x in cells(N)}
    inter = multiplicative_contrasts(ratio, N)
    bad = {k: v for k, v in inter.items() if v != 1}
    ck(f"N=3 numeric: all {len(inter)} interaction contrasts vanish", not bad, f"{bad}")


# ------------------------------------ Part 3: the converse (additivity) -----
def part3_converse(ck, N=3):
    vals = {x: sp.Symbol("f_" + "".join(map(str, x))) for x in cells(N)}
    eqs = [sp.Eq(v, 0) for v in additive_contrasts(vals, N).values()]
    sol = sp.solve(eqs, list(vals.values()), dict=True)
    ck(f"converse (N={N}): the vanishing-contrast system has a unique solution family",
       len(sol) == 1, f"{sol}")
    if not sol:
        return
    phi = {x: vals[x].subs(sol[0]) for x in cells(N)}
    lam0 = phi[tuple([0] * N)]
    lama = {a: phi[tuple(1 if b == a else 0 for b in range(N))] - lam0 for a in range(N)}
    recon = {x: lam0 + sum(lama[a] for a in range(N) if x[a] == 1) for x in cells(N)}
    ck(f"converse (N={N}): phi = lambda_0 + sum_a lambda_a(x_a) reproduces phi at "
       "every cell (the appendix's telescope run backwards)",
       all(sp.expand(recon[x] - phi[x]) == 0 for x in cells(N)))
    # the solution family has exactly N+1 free parameters, as additivity requires
    free = len(set().union(*[set(e.free_symbols) for e in phi.values()]))
    ck(f"converse (N={N}): the additive family has N+1 = {N + 1} free parameters",
       free == N + 1, f"free parameters = {free}")


# ------------------------- Part 4: attribute-locality is doing real work ----
def part4_scope(ck):
    a_, b_, w_ = sp.symbols('alpha_ beta_ w_', positive=True)
    Pi = {(0, 0): a_ * b_, (0, 1): a_ * (1 - b_),
          (1, 0): (1 - a_) * b_, (1, 1): (1 - a_) * (1 - b_)}
    ck("scope: an independent prior has assoc = 0",
       sp.expand(Pi[(0, 0)] * Pi[(1, 1)] - Pi[(0, 1)] * Pi[(1, 0)]) == 0)

    # A cue on the joint event {A = B} -- excluded by Assumption 3.
    W = {x: Pi[x] * (w_ if x[0] == x[1] else 1) for x in cells(2)}
    Zt = sum(W.values())
    Q = {x: W[x] / Zt for x in cells(2)}
    assoc_post = sp.cancel(sp.together(Q[(0, 0)] * Q[(1, 1)] - Q[(0, 1)] * Q[(1, 0)]))
    val = sp.cancel(assoc_post.subs({a_: sp.Rational(1, 3), b_: sp.Rational(1, 4), w_: 2}))
    ck("scope: a NON-local (joint-event) cue manufactures association from "
       "independence, so Assumption 3 is what Lemma SEP needs", val != 0,
       f"assoc after joint-event reweighting = {val}")
    # and it is genuinely non-separable: the pairwise contrast of log(Q/P) is not 1
    ratio = {x: sp.cancel(sp.together(Q[x] / Pi[x])) for x in cells(2)}
    inter = multiplicative_contrasts(ratio, 2)
    ck("scope: the joint-event reweighting has a NON-vanishing interaction contrast",
       all(sp.cancel(v - 1) != 0 for v in inter.values()), f"{inter}")


def main():
    ck = Check("Lemma SEP -- separability of attribute-local composites")
    part1_concrete_N2(ck)
    part2_abstract(ck)
    part2b_faithful(ck)
    part3_converse(ck)
    part4_scope(ck)
    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
