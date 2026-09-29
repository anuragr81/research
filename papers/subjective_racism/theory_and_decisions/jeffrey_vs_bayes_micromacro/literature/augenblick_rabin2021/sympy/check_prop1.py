"""
Augenblick & Rabin (2021), QJE 136(2) -- Proposition 1 and Table 1.

Checks, in exact rational arithmetic:
  (1) the paper's Table 1 numbers for the symmetric noisy-signal DGP
      (gamma = 3/4, theta_0 = 1/2, two periods), row by row;
  (2) Proposition 1, EM = ER, on that DGP;
  (3) the displayed one-period identity
          m_{t,t+1} - r_{t,t+1} = (2*theta_t - 1)*(theta_t - theta_{t+1})
      symbolically, for a free belief pair -- the identity formalized in Lean as
      `excess_step`;
  (4) Proposition 1 symbolically for a general two-period binary DGP, so the
      result is not an artefact of the particular gamma.
"""
import sympy as sp


def unc(t):
    return (1 - t) * t


def movement(stream):
    return sum((stream[i + 1] - stream[i]) ** 2 for i in range(len(stream) - 1))


def reduction(stream):
    return sum(unc(stream[i]) - unc(stream[i + 1]) for i in range(len(stream) - 1))


def posterior(prior, lik1, lik0):
    """P(state=1 | evidence) from likelihoods under each state."""
    return sp.nsimplify(prior * lik1 / (prior * lik1 + (1 - prior) * lik0))


def main():
    ok = True

    # ---------- (1) and (2): Table 1, symmetric noisy signal --------------
    g = sp.Rational(3, 4)          # precision gamma = Pr(h|H) = Pr(l|L)
    th0 = sp.Rational(1, 2)
    # per-signal likelihoods: Pr(s | state=1), Pr(s | state=0)
    lik = {'h': (g, 1 - g), 'l': (1 - g, g)}

    print("Table 1, symmetric noisy-signal DGP, gamma = 3/4, two periods")
    print(f"{'signals':<10}{'P(H_t)':<10}{'stream':<28}{'m':<10}{'r':<10}{'m-r':<10}")

    # the paper's published values, read off Table 1
    published = {
        ('l', 'l'): (sp.Rational(5, 16), sp.Rational(17, 200), sp.Rational(4, 25)),
        ('l', 'h'): (sp.Rational(3, 16), sp.Rational(1, 8), sp.Integer(0)),
    }

    EM = 0
    ER = 0
    for s1 in ('h', 'l'):
        for s2 in ('h', 'l'):
            # joint likelihood of the pair under each state (iid given state)
            L1 = lik[s1][0] * lik[s2][0]
            L0 = lik[s1][1] * lik[s2][1]
            p = sp.nsimplify(th0 * L1 + (1 - th0) * L0)      # P(history)
            t1 = posterior(th0, lik[s1][0], lik[s1][1])
            t2 = posterior(th0, L1, L0)
            stream = [th0, t1, t2]
            m, r = sp.nsimplify(movement(stream)), sp.nsimplify(reduction(stream))
            EM += p * m
            ER += p * r
            print(f"{s1+','+s2:<10}{str(p):<10}{str(stream):<28}"
                  f"{str(m):<10}{str(r):<10}{str(sp.nsimplify(m-r)):<10}")
            if (s1, s2) in published:
                pp, pm, pr = published[(s1, s2)]
                for name, got, want in (("P", p, pp), ("m", m, pm), ("r", r, pr)):
                    if sp.simplify(got - want) != 0:
                        ok = False
                        print(f"    MISMATCH vs published {name}: {got} != {want}")

    EM, ER = sp.nsimplify(EM), sp.nsimplify(ER)
    print(f"\nEM = {EM}   ER = {ER}")
    if sp.simplify(EM - ER) != 0:
        ok = False
        print("  FAIL: Proposition 1 violated on the Table 1 DGP")
    else:
        print("  PASS: Proposition 1, EM = ER, exactly")
    # resolving-stream corollary does NOT apply here (T=2 does not resolve),
    # so EM should equal u_0 - E[u_T], not u_0.
    Eu2 = 0
    for s1 in ('h', 'l'):
        for s2 in ('h', 'l'):
            L1, L0 = lik[s1][0]*lik[s2][0], lik[s1][1]*lik[s2][1]
            p = sp.nsimplify(th0 * L1 + (1 - th0) * L0)
            Eu2 += p * unc(posterior(th0, L1, L0))
    if sp.simplify(EM - (unc(th0) - sp.nsimplify(Eu2))) != 0:
        ok = False
        print("  FAIL: EM != u_0 - E[u_T]")
    else:
        print("  PASS: EM = u_0 - E[u_T] (telescoping form)")

    # ---------- (3) the one-period identity, symbolically ----------------
    a, b = sp.symbols('theta_t theta_tp1', real=True)
    lhs = (b - a) ** 2 - (unc(a) - unc(b))
    rhs = (2 * a - 1) * (a - b)
    print("\nOne-period identity  m - r = (2*theta_t - 1)*(theta_t - theta_{t+1})")
    d = sp.simplify(sp.expand(lhs - rhs))
    print(f"  residual = {d}")
    if d != 0:
        ok = False
        print("  FAIL")
    else:
        print("  PASS (matches Lean `excess_step`)")

    # ---------- (4) Proposition 1 for a general two-period DGP -----------
    # free prior and free per-state signal likelihoods; two binary signals.
    p0 = sp.Symbol('p0', positive=True)
    u1, u0 = sp.symbols('u1 u0', positive=True)   # Pr(h|1), Pr(h|0)
    liks = {'h': (u1, u0), 'l': (1 - u1, 1 - u0)}
    EMs = 0
    ERs = 0
    for s1 in ('h', 'l'):
        for s2 in ('h', 'l'):
            L1 = liks[s1][0] * liks[s2][0]
            L0 = liks[s1][1] * liks[s2][1]
            pr = p0 * L1 + (1 - p0) * L0
            t1 = p0 * liks[s1][0] / (p0 * liks[s1][0] + (1 - p0) * liks[s1][1])
            t2 = p0 * L1 / (p0 * L1 + (1 - p0) * L0)
            stream = [p0, t1, t2]
            EMs += pr * movement(stream)
            ERs += pr * reduction(stream)
    print("\nProposition 1 for a general two-period binary DGP (free p0, u1, u0)")
    d = sp.simplify(sp.together(EMs - ERs))
    print(f"  EM - ER = {d}")
    if d != 0:
        ok = False
        print("  FAIL")
    else:
        print("  PASS: EM = ER identically in (p0, u1, u0)")

    print("\nAll checks passed." if ok else "\nSOME CHECKS FAILED.")
    return ok


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
