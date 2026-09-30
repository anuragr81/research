"""
Proposition IMM (exact immunity), manuscript Section 4.1.

  (a) On a single cue, the Jeffrey posterior equals the matched Bayesian
      posterior identically, for every prior and every impression.
  (b) With two cues but c = 0 (independent attributes),
      P^J_AB = P^J_BA = P^B = q (x) r exactly.

Both are exact identities -- no expansion in c is involved.
"""
import sympy as sp
from jeffrey_core import *


def main():
    ck = Check("Proposition IMM -- exact immunity")

    P = prior()

    # ---- (a) single cue on A: Jeffrey step == matched Bayesian update -------
    ck.mat_eq("(a) J_A P == Bayes(P | A-cue) identically in (alpha,beta,c,q0)",
              jeffrey_A(P), bayes_single_A(P))

    # the normaliser of the matched likelihood is exactly 1 (proof of A1)
    mA = marg_A(P)
    ck.eq("(a) normaliser Z_A = sum_ij P(i,j) q_i / P(A=i) = 1",
          sum(P[i, j] * q[i] / mA[i] for i in range(2) for j in range(2)), 1)

    # ...and symmetrically for a single cue on B
    mB = marg_B(P)
    W = sp.Matrix(2, 2, lambda i, j: P[i, j] * r[j] / mB[j])
    ck.mat_eq("(a) J_B P == Bayes(P | B-cue) identically", jeffrey_B(P), W / sum(W))

    # ---- (b) two cues at c = 0 --------------------------------------------
    PJ_AB0, PJ_BA0, PB0 = [M.subs(c, 0) for M in posteriors()]
    ck.mat_eq("(b) P^J_AB|_{c=0} == q (x) r", PJ_AB0, indep)
    ck.mat_eq("(b) P^J_BA|_{c=0} == q (x) r", PJ_BA0, indep)
    ck.mat_eq("(b) P^B|_{c=0}    == q (x) r", PB0, indep)

    # the prior factorises at c = 0 (the step the proof turns on)
    ck.mat_eq("(b) P|_{c=0} factorises as P(A=i)P(B=j)",
              prior(0), sp.Matrix(2, 2, lambda i, j: marg_A(prior(0))[i] * marg_B(prior(0))[j]))

    # each posterior is a probability distribution
    for nm, M in [("P^J_AB", posteriors()[0]), ("P^J_BA", posteriors()[1]), ("P^B", posteriors()[2])]:
        ck.eq(f"normalisation: sum({nm}) = 1", sum(M), 1)

    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
