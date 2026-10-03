"""
Barberis (2012), "A model of casino gambling", Management Science 58(1),
35-51 (journal pages).

Checks:
  (1) Figure 2's exit strategy (p.42): gamble until T = 5 or node (3,1);
      accumulated winnings $30, $10, -$10, -$30, -$50 with probabilities
      7, 9, 10, 5, 1 in 32 (all 32 paths enumerated).
  (2) Path independence (fn. 13, p.42): the action under a plan is a function
      of the node; paths with equal numbers of wins and losses reach the same
      node, so the sequence of outcomes cannot affect it.
  (3) The appendix's exit probabilities (p.50): under the strategy "exit on a
      first-round loss, when winnings return to zero, or at T", the
      probability of leaving at node (T, j) is 2^-T [C(T-1, j-1) - C(T-1, j-2)];
      checked against brute-force enumeration for T = 3, ..., 12.
  (4) Corollary 1 (p.45): condition (12) at (alpha, delta, lambda) =
      (0.88, 0.65, 2.25) first holds at T = 26 (50-digit arithmetic).
  (5) Condition (8) (p.41) on a grid of alpha, delta in (0,1) (the Lean record
      proves it for all such values).
Exit status 0 iff every check passes.
"""
import itertools
from math import comb

import mpmath as mp

OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


def w(P, d):
    P = mp.mpf(P)
    if P == 0:
        return mp.mpf(0)
    return P**d / (P**d + (1 - P)**d)**(1 / d)


def main():
    mp.mp.dps = 50

    print("(1) Figure 2's exit strategy")
    counts = {}
    for path in itertools.product((1, -1), repeat=5):
        out = 3 if path[:3] == (1, 1, 1) else sum(path)
        counts[out] = counts.get(out, 0) + 1
    check(f"distribution {dict(sorted(counts.items(), reverse=True))} = {{3:7, 1:9, -1:10, -3:5, -5:1}} (units of $10)",
          counts == {3: 7, 1: 9, -1: 10, -3: 5, -5: 1})

    print("(2) Path independence")
    node = lambda path: (len(path), path.count(-1) + 1)
    ok = all(node(p) == node(tuple(reversed(p))) for p in itertools.product((1, -1), repeat=6))
    check("every path and its reversal reach the same node (T = 6, all 64 paths)", ok)

    print("(3) Exit probabilities by the reflection principle")
    ok = True
    for T in range(3, 13):
        exits = {}
        for path in itertools.product((1, -1), repeat=T):
            if path[0] == -1:
                continue
            s, stopped = 0, False
            for t, x in enumerate(path, 1):
                s += x
                if t > 1 and s == 0:
                    stopped = True
                    break
            if not stopped:
                j = path.count(-1) + 1
                exits[j] = exits.get(j, 0) + 1
        for j in range(1, T - (T + 1) // 2 + 1):
            c2 = comb(T - 1, j - 2) if j >= 2 else 0
            ok &= exits.get(j, 0) == comb(T - 1, j - 1) - c2
    check("2^T P(exit at (T, j)) = C(T-1, j-1) - C(T-1, j-2) for T = 3..12, all positive nodes", ok)

    print("(4) Corollary 1")
    a, d, lam = mp.mpf("0.88"), mp.mpf("0.65"), mp.mpf("2.25")

    def lhs(T):
        tot = mp.mpf(0)
        for j in range(1, T - (T + 1) // 2 + 1):
            c1 = comb(T - 1, j - 1)
            c2 = comb(T - 1, j - 2) if j >= 2 else 0
            tot += (T + 2 - 2 * j)**a * (w(mp.mpf(c1) / 2**T, d) - w(mp.mpf(c2) / 2**T, d))
        return tot

    holds = [T for T in range(2, 41) if lhs(T) > lam * w(mp.mpf(1) / 2, d)]
    check(f"condition (12) first holds at T = {holds[0]} (paper: 26)", holds[0] == 26)
    check("...and fails for every T from 2 to 25", all(T >= 26 for T in holds))

    print("(5) Condition (8) on a grid")
    ok = True
    for ai in range(1, 20):
        for di in range(1, 20):
            al, de = mp.mpf(ai) / 20, mp.mpf(di) / 20
            ok &= (40**al - 30**al) >= (50**al - 30**al) * w(mp.mpf(1) / 2, de)
    check("v(40) - v(30) >= (v(50) - v(30)) w(1/2) on the 19 x 19 grid of (0,1)^2", ok)
    return OK


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
