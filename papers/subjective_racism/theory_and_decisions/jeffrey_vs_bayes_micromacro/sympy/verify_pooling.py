"""
The pooling rule is immaterial at first order (Section 5 remark).

The population mean P̄_λ = λ P^J_AB + (1-λ) P^J_BA is a linear pool of the two
posteriors.  A geometric pool, cell-wise (P^J_AB)^λ (P^J_BA)^(1-λ) renormalised,
differs from it only at second order in c, because the two posteriors coincide
at c = 0 (Proposition IMM) and two tables that differ by O(c) have linear and
geometric means that differ by O(c^2).

Rows (exact rationals at the generic prior; λ = 1/2 and λ = 2/5):
  (1) at c = 0 the two posteriors coincide, so both pools equal q (x) r
  (2) the c^0 and c^1 coefficients of (linear pool - geometric pool) vanish, cell by cell
  (3) the c^2 coefficient is nonzero in some cell (so the difference is second order,
      not smaller)
  (4) the association of the two pools therefore differs only at second order too
"""
import sympy as sp
from jeffrey_core import *


def geo_pool(X, Y, w):
    W = sp.Matrix(2, 2, lambda i, j: X[i, j] ** w * Y[i, j] ** (1 - w))
    S = sum(W)
    return W.applyfunc(lambda e: e / S)


def coeffs(e, n):
    s = sp.series(e, c, 0, n + 1).removeO()
    return [sp.simplify(s.coeff(c, k)) for k in range(n + 1)]


def main():
    ck = Check("Pooling rule -- linear and geometric pools of the two posteriors differ at second order")
    PJ_AB, PJ_BA, _ = posteriors()
    AB = PJ_AB.applyfunc(lambda e: sp.cancel(e.subs(GENERIC)))
    BA = PJ_BA.applyfunc(lambda e: sp.cancel(e.subs(GENERIC)))
    I0 = sp.Matrix(2, 2, lambda i, j: q[i] * r[j]).subs(GENERIC)
    ck.mat_eq("(1) both posteriors equal q (x) r at c=0", AB.subs(c, 0), BA.subs(c, 0))
    ck.mat_eq("(1) ...and that table is q (x) r", AB.subs(c, 0), I0)
    for w in (sp.Rational(1, 2), sp.Rational(2, 5)):
        lin = w * AB + (1 - w) * BA
        geo = geo_pool(AB, BA, w)
        diff = lin - geo
        cs = [[coeffs(diff[i, j], 2) for j in range(2)] for i in range(2)]
        ck(f"(2) lambda={w}: c^0 coefficient of linear - geometric pool is zero in every cell",
           all(cs[i][j][0] == 0 for i in range(2) for j in range(2)), f"{[[cs[i][j][0] for j in range(2)] for i in range(2)]}")
        ck(f"(2) lambda={w}: c^1 coefficient is zero in every cell",
           all(cs[i][j][1] == 0 for i in range(2) for j in range(2)), f"{[[cs[i][j][1] for j in range(2)] for i in range(2)]}")
        ck(f"(3) lambda={w}: c^2 coefficient is nonzero in some cell",
           any(cs[i][j][2] != 0 for i in range(2) for j in range(2)), f"{[[cs[i][j][2] for j in range(2)] for i in range(2)]}")
        a_lin = lin[0, 0] * lin[1, 1] - lin[0, 1] * lin[1, 0]
        a_geo = geo[0, 0] * geo[1, 1] - geo[0, 1] * geo[1, 0]
        ca = coeffs(a_lin - a_geo, 2)
        ck(f"(4) lambda={w}: association of the two pools agrees to first order",
           ca[0] == 0 and ca[1] == 0, f"{ca}")
    return ck.done()


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
