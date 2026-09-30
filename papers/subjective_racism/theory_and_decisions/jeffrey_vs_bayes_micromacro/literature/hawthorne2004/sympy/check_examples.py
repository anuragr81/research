"""
Hawthorne (2004), "Three Models of Sequential Belief Updating on Uncertain
Evidence", J. Phil. Logic 33, 89-123 -- the medical diagnosis example.
Pages are the journal pagination.

Companion to lean/Literature/Hawthorne.lean (the `med_*` theorems).  All
arithmetic is exact (sympy Rationals).

  (1) the prior (pp. 97-98): Q[C] = .5, Q[E|C] = Q[F|C] = .95,
      Q[E|~C] = Q[F|~C] = .05, E and F independent given C and given ~C;
  (2) Amnestic updating (p. 98): Q_f[~F] = .90 and Q_e[E] = .90 give
      Q_f[C] = .14, Q_fe[C] = .68, Q_e[C] = .86, Q_ef[C] = .32;
  (3) the explicit overwrite on two distinct bases, Q_e[E] = Q_fe[E] = .90;
  (4) the p. 97 commutation criterion fails: each update moves the other's
      basis marginal;
  (5) note 14: likelihoods .99 give .83 and .17;
  (6) the Likelihood-Ratio example (p. 107): LR[f,F,~F] = .50 and
      LR[e,E,~E] = 2 give Q_f[C] = .35 and Q_fe[C] = Q_ef[C] = .50;
  (7) the Bayes-factor reading of the amnestic .90 reports is 1/9 and 9, not
      .50 and 2 (audit D8(a)); with 1/9 and 9 the result is also 1/2;
  (8) the NL Extended Sequential Update Formula (p. 102) with factors taken
      against the prior: its normalising denominator is 301/625, not 1.
Exit 0 iff all checks pass.
"""
import itertools
import sys

import sympy as sp

R = sp.Rational
ok = True


def check(name, cond, detail=""):
    global ok
    print(f"  [{'PASS' if cond else 'FAIL'}] {name}")
    if detail:
        print(f"         {detail}")
    if not cond:
        ok = False


# Atoms (c, e, f): C cancer, E x-ray mass image, F cancer-like cells.
IDX = {"C": 0, "E": 1, "F": 2}


def prior(lik):
    P = {}
    for c, e, f in itertools.product((1, 0), repeat=3):
        pe = lik if e == c else 1 - lik
        pf = lik if f == c else 1 - lik
        P[(c, e, f)] = R(1, 2) * pe * pf
    return P


def marg(P, var, val=1):
    return sum(v for k, v in P.items() if k[IDX[var]] == val)


def jeffrey(P, var, target1):
    """Amnestic (Basic Jeffrey) update of basis {var, ~var} to Q[var] = target1."""
    m1, m0 = marg(P, var, 1), marg(P, var, 0)
    return {k: v * ((target1 / m1) if k[IDX[var]] == 1 else ((1 - target1) / m0))
            for k, v in P.items()}


def reweight(P, var, w):
    """Factor update on {var, ~var}: weights w[1] on var, w[0] on ~var, normalised."""
    u = {k: v * w[k[IDX[var]]] for k, v in P.items()}
    Z = sum(u.values())
    return {k: v / Z for k, v in u.items()}


def factor(P, var, lr):
    """Factor update with LR[., var, ~var] = lr."""
    return reweight(P, var, {1: lr, 0: 1})


def LR(P, Pnew, var):
    """LR[Q, e, var, ~var] = NL(var) / NL(~var) (p. 104)."""
    return (marg(Pnew, var, 1) / marg(P, var, 1)) / (marg(Pnew, var, 0) / marg(P, var, 0))


def rounds_to(x, printed, places=2):
    return round(float(x), places) == printed


P = prior(R(95, 100))

print("(1) the prior")
check("joint sums to 1", sum(P.values()) == 1)
check("Q[C] = 1/2", marg(P, "C") == R(1, 2))
check("Q[E] = Q[F] = 1/2", marg(P, "E") == R(1, 2) and marg(P, "F") == R(1, 2))

print("(2) Amnestic updating, p. 98")
Qf = jeffrey(P, "F", R(1, 10))          # Q_f[~F] = .90
Qfe = jeffrey(Qf, "E", R(9, 10))         # Q_fe[E] = .90
Qe = jeffrey(P, "E", R(9, 10))
Qef = jeffrey(Qe, "F", R(1, 10))
check("Q_f[C] = 7/50 (.14)", marg(Qf, "C") == R(7, 50))
check("Q_fe[C] = 24689/36256, rounds to .68",
      marg(Qfe, "C") == R(24689, 36256) and rounds_to(marg(Qfe, "C"), 0.68),
      f"{float(marg(Qfe, 'C')):.4f}")
check("Q_e[C] = 43/50 (.86)", marg(Qe, "C") == R(43, 50))
check("Q_ef[C] = 11567/36256, rounds to .32",
      marg(Qef, "C") == R(11567, 36256) and rounds_to(marg(Qef, "C"), 0.32),
      f"{float(marg(Qef, 'C')):.4f}")
check("the two '.18' swings: .68 - .5 and .5 - .32 both round to .18",
      rounds_to(marg(Qfe, "C") - R(1, 2), 0.18) and rounds_to(R(1, 2) - marg(Qef, "C"), 0.18))

print("(3) the overwrite on two distinct bases, p. 98")
check("Q_e[E] = Q_fe[E] = 9/10", marg(Qe, "E") == R(9, 10) and marg(Qfe, "E") == R(9, 10))
check("Q_ef[F] = Q_f[F] = 1/10 (the other order overwrites F)",
      marg(Qef, "F") == R(1, 10) and marg(Qf, "F") == R(1, 10))

print("(4) the p. 97 commutation criterion fails here")
check("the F-update moves Q[E] from 1/2 to 22/125", marg(Qf, "E") == R(22, 125))
check("the E-update moves Q[F] from 1/2 to 103/125", marg(Qe, "F") == R(103, 125))
check("hence the orders differ on C", marg(Qfe, "C") != marg(Qef, "C"))

print("(5) note 14: likelihoods .99")
P99 = prior(R(99, 100))
fe99 = marg(jeffrey(jeffrey(P99, "F", R(1, 10)), "E", R(9, 10)), "C")
ef99 = marg(jeffrey(jeffrey(P99, "E", R(9, 10)), "F", R(1, 10)), "C")
check("Q_fe[C] = 1477317/1778144, rounds to .83",
      fe99 == R(1477317, 1778144) and rounds_to(fe99, 0.83), f"{float(fe99):.4f}")
check("Q_ef[C] = 300827/1778144, rounds to .17",
      ef99 == R(300827, 1778144) and rounds_to(ef99, 0.17), f"{float(ef99):.4f}")

print("(6) the Likelihood-Ratio example, p. 107: LR .50 and 2")
Lf = factor(P, "F", R(1, 2))
Lfe = factor(Lf, "E", 2)
Le = factor(P, "E", 2)
Lef = factor(Le, "F", R(1, 2))
check("the technician's factor is LR[f,F,~F] = 1/2", LR(P, Lf, "F") == R(1, 2))
check("the radiologist's factor is LR[e,E,~E] = 2", LR(P, Le, "E") == 2)
check("Q_f[C] = 7/20 (.35)", marg(Lf, "C") == R(7, 20))
check("Q_fe[C] = 1/2 (.50)", marg(Lfe, "C") == R(1, 2))
check("Q_ef = Q_fe on every atom", all(Lfe[k] == Lef[k] for k in P))
check("LR .50 against Q[F] = 1/2 gives Q_f[~F] = 2/3, not the .90 credence",
      marg(Lf, "F", 0) == R(2, 3))

print("(7) the Bayes-factor reading of the amnestic .90 reports")
check("LR implicit in Q_f[~F] = .90 against Q[F] = 1/2 is 1/9", LR(P, Qf, "F") == R(1, 9))
check("LR implicit in Q_e[E] = .90 against Q[E] = 1/2 is 9", LR(P, Qe, "E") == 9)
check("so the LR example does not reuse the amnestic reports (1/9, 9 vs 1/2, 2)",
      (LR(P, Qf, "F"), LR(P, Qe, "E")) != (R(1, 2), 2))
N_fe = marg(factor(factor(P, "F", R(1, 9)), "E", 9), "C")
N_ef = marg(factor(factor(P, "E", 9), "F", R(1, 9)), "C")
check("with factors 1/9 and 9: Q_fe[C] = Q_ef[C] = 1/2", N_fe == R(1, 2) and N_ef == R(1, 2))

print("(8) NL Extended Sequential Update Formula (p. 102): the denominator")
nlE = {1: R(9, 10) / marg(P, "E", 1), 0: R(1, 10) / marg(P, "E", 0)}
nlF = {1: R(1, 10) / marg(P, "F", 1), 0: R(9, 10) / marg(P, "F", 0)}
unnorm = {k: v * nlE[k[1]] * nlF[k[2]] for k, v in P.items()}
den = sum(unnorm.values())
check("denominator = 301/625, not 1", den == R(301, 625))
check("unnormalised C-mass = 301/1250", marg(unnorm, "C") == R(301, 1250))
check("normalised: Q[C] = 1/2", marg(unnorm, "C") / den == R(1, 2))
A = reweight(reweight(P, "E", nlE), "F", nlF)
B = reweight(reweight(P, "F", nlF), "E", nlE)
check("sequential NL updates with fixed factors equal the one-shot formula, both orders",
      all(A[k] == unnorm[k] / den and B[k] == A[k] for k in P))

print()
print("ALL PASS" if ok else "SOME CHECKS FAILED")
sys.exit(0 if ok else 1)
