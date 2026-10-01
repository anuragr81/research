#!/usr/bin/env python3
"""
Soft versus hard cues on the worked example of Setup (alpha = beta = 1/2,
c = 1/20, credential q0 = 1/5, letter r0 = 7/10).

A HARD cue is a proposition with a likelihood fixed independently of the
evaluator's belief; conditioning on two hard cues commutes (E then F is
conditioning on E and F) and, with the likelihood ratios matched to the prior
marginals, equals the benchmark P^B.  A SOFT cue delivers a credence; the
factor it implies is matched to the marginal in force when it arrives, so the
same delivered credence implies a different factor in the two sequences.  That
is the whole of the sequence effect, and the difference between the two
readings of a cue appears only when the cue meets a belief other than the prior
(on one cue the readings agree, Proposition IMM).

Every number printed in the corresponding manuscript passage is asserted here.
"""
import sympy as sp
from jeffrey_core import Check

R = sp.Rational


def cond(Q, lik, attr):
    """Bayes conditioning on a proposition with fixed likelihoods lik[k] for the
    cells of attribute attr (row = A, column = B)."""
    W = sp.Matrix(2, 2, lambda i, j: Q[i, j] * (lik[i] if attr == 'A' else lik[j]))
    return W / sum(W)


def jeff(Q, target, attr):
    m = [Q[0, 0] + Q[0, 1], Q[1, 0] + Q[1, 1]] if attr == 'A' else [Q[0, 0] + Q[1, 0], Q[0, 1] + Q[1, 1]]
    return sp.Matrix(2, 2, lambda i, j: Q[i, j] * (target[i] / m[i] if attr == 'A' else target[j] / m[j]))


def mA(Q): return [Q[0, 0] + Q[0, 1], Q[1, 0] + Q[1, 1]]
def mB(Q): return [Q[0, 0] + Q[1, 0], Q[0, 1] + Q[1, 1]]
def odds(m): return m[0] / m[1]


def main():
    ck = Check("Soft versus hard cues on the worked example")
    a = b = R(1, 2); c = R(1, 20)
    q = [R(1, 5), R(4, 5)]; r = [R(7, 10), R(3, 10)]
    P = sp.Matrix([[a * b + c, a * (1 - b) - c], [(1 - a) * b - c, (1 - a) * (1 - b) + c]])

    # ---- soft cues: the two Jeffrey sequences and the benchmark
    PA = jeff(P, q, 'A'); AB = jeff(PA, r, 'B')
    PBl = jeff(P, r, 'B'); BA = jeff(PBl, q, 'A')
    lA = [q[0] / a, q[1] / (1 - a)]; lB = [r[0] / b, r[1] / (1 - b)]
    PB = cond(cond(P, lA, 'A'), lB, 'B')
    ck("soft, credential first: A-marginal ends at 18/77 = .234", sp.simplify(mA(AB)[0] - R(18, 77)) == 0)
    ck("soft, letter first: A-marginal ends at 1/5 = .200 (the credential's credence, read last)", sp.simplify(mA(BA)[0] - R(1, 5)) == 0)
    ck("after the credential alone the B-marginal is 11/25 = .44", sp.simplify(mB(PA)[0] - R(11, 25)) == 0)

    # ---- the factor a soft cue implies depends on the marginal it meets
    f_first = odds(r) / odds(mB(P))     # letter read first, against beta = 1/2
    f_second = odds(r) / odds(mB(PA))   # letter read second, against 11/25
    ck("letter's implied factor when read first is 7/3", sp.simplify(f_first - R(7, 3)) == 0)
    ck("letter's implied factor when read second is 98/33", sp.simplify(f_second - R(98, 33)) == 0)
    ck("the two implied factors differ (the sequence effect, in one number)", f_first != f_second)
    ck("...and coincide at c = 0, where the credential leaves the B-marginal alone",
       sp.simplify(odds(r) / odds(mB(jeff(P.subs(c, 0) if False else sp.Matrix([[a*b, a*(1-b)], [(1-a)*b, (1-a)*(1-b)]]), q, 'A'))) - R(7, 3)) == 0)

    # ---- hard cues: fixed likelihood ratios 1/4 (credential) and 7/3 (letter)
    hA = [R(1, 4), 1]; hB = [R(7, 3), 1]
    EF = cond(cond(P, hA, 'A'), hB, 'B'); FE = cond(cond(P, hB, 'B'), hA, 'A')
    ck("hard cues commute: E then F equals F then E, every cell", sp.simplify(EF - FE) == sp.zeros(2, 2))
    ck("hard cues with prior-matched ratios reproduce the benchmark P^B", sp.simplify(EF - PB) == sp.zeros(2, 2))
    ck("hard A-marginal is 27/119 = .227 in either sequence", sp.simplify(mA(EF)[0] - R(27, 119)) == 0)
    ck("on ONE cue the two readings agree (Proposition IMM): hard credential = soft credential",
       sp.simplify(cond(P, hA, 'A') - PA) == sp.zeros(2, 2))

    # ---- symbolic: the sequence effect is exactly the dependence of the implied factor
    # on the belief it meets.  Prior P(A=0)=al, P(B=0)=be, association cs; delivered
    # credences x0 on A=0 and y0 on B=0; all cells, cues and the c-dependent marginals interior.
    al, be, cs, x0, y0 = sp.symbols('alpha beta c q0 r0')
    Ps = sp.Matrix([[al * be + cs, al * (1 - be) - cs], [(1 - al) * be - cs, (1 - al) * (1 - be) + cs]])
    xs = [x0, 1 - x0]; ys = [y0, 1 - y0]
    sA = jeff(Ps, xs, 'A'); sB = jeff(Ps, ys, 'B')
    D = (jeff(sA, ys, 'B') - jeff(sB, xs, 'A')).applyfunc(sp.factor)
    ck("symbolic: an earlier credential moves the B-marginal the letter meets by c(q0-alpha)/(alpha(1-alpha))",
       sp.simplify(mB(sA)[0] - be - cs * (x0 - al) / (al * (1 - al))) == 0)
    ck("symbolic: an earlier letter moves the A-marginal the credential meets by c(r0-beta)/(beta(1-beta))",
       sp.simplify(mA(sB)[0] - al - cs * (y0 - be) / (be * (1 - be))) == 0)
    # every cell of the sequence effect is c times a factor that vanishes only at q0=alpha, r0=beta
    cores = []
    for e in D:
        keep = [f for f, _ in sp.factor_list(sp.numer(sp.together(e)))[1]
                if f.free_symbols & {x0, y0} and f not in (x0, y0, x0 - 1, y0 - 1)]
        cores.append(sp.Mul(*keep))
    ck("symbolic: every cell of P^J_AB - P^J_BA carries the factor c",
       all(sp.simplify(e.subs(cs, 0)) == 0 for e in D))
    ck("symbolic: with c != 0 the sequence effect vanishes iff q0 = alpha and r0 = beta, "
       "i.e. iff neither cue changes the factor the other implies",
       sp.solve(cores, [x0, y0], dict=True) == [{x0: al, y0: be}]
       and all(sp.simplify(e.subs({x0: al, y0: be})) == 0 for e in D))
    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
