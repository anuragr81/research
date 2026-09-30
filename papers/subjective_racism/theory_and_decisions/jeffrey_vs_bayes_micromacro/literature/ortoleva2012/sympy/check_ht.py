"""
Ortoleva (2012), AER 102(6) -- the Hypothesis Testing (HT) model, as restated
in Ortoleva (2024), Annu. Rev. Econ. 16:545-570, Sections 2.2.2 and 4
(pp. 549-560). The primary is not available; everything follows the survey.

Checks, in exact rational / symbolic arithmetic. Exit status 0 iff all pass.
  (1) Theorem 1 (p. 550): the identity
        EU_pi(fAg) - EU_pi(g) = pi(A) [EU_{pi_A}(f) - EU_{pi_A}(g)]
      symbolically on 4 states; and the "only if" algebra: the relative-
      likelihood equations q_w pi_w' = q_w' pi_w on A, q = 0 off A, sum q = 1
      have the Bayes posterior as their unique solution.
  (2) The worked example of Ortoleva.lean: pi = (24/25, 3/100, 1/100),
      pi' = (1/5, 1/5, 3/5), rho = (9/10, 1/10), eps = 1/20. On A = {b, c}:
      pi(A) = 1/25 <= eps, rho^BU_A selects pi', HT posterior (0, 1/4, 3/4)
      vs Bayes (0, 3/4, 1/4); the Dynamic Consistency violation with
      f = 1_b, g = 1_c; no score ties on any nonempty event.
  (3) Random exact sweep of HT models (3-4 states, 2-4 candidate priors,
      random eps in [0, 1)): Consequentialism (posterior supported in A,
      mass 1) on every nonempty event, and Dynamic Coherence on every cycle
      of length 2 and 3 among nonempty events (Theorem 2, "if").
  (4) eps = 0: HT equals Bayes on every positive-probability event, and
      satisfies Dynamic Consistency on a grid of acts (Theorem 2, "Moreover").
  (5) For random models: Dynamic Consistency (tested on unit acts and on the
      acts of the Theorem 1 proof) fails exactly when some event with
      0 < pi(A) <= eps has a selected prior whose conditional differs from
      pi's; and HT(eps) and HT(0) then differ on that event.
"""
import itertools
import random
import sys

import sympy as sp

R = sp.Rational
OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


# ------------------------------------------------------------------ model
def prob(p, A):
    return sum((p[w] for w in A), R(0))


def bayes(p, A):
    pa = prob(p, A)
    return tuple(p[w] / pa if w in A else R(0) for w in range(len(p)))


def eu(p, f):
    return sum((p[w] * f[w] for w in range(len(p))), R(0))


def splice(f, A, g):
    return tuple(f[w] if w in A else g[w] for w in range(len(f)))


def score(P, rho, A, i):
    return prob(P[i], A) * rho[i]


def select(P, rho, A):
    """Unique maximiser of rho^BU_A; None if tied or undefined."""
    sc = [score(P, rho, A, i) for i in range(len(P))]
    m = max(sc)
    if m == 0 or sc.count(m) != 1:
        return None
    return sc.index(m)


def ht_post(P, rho, i0, eps, A):
    pi = P[i0]
    if prob(pi, A) > eps:
        return bayes(pi, A)
    return bayes(P[select(P, rho, A)], A)


def nonempty_events(n):
    return [frozenset(c) for k in range(1, n + 1) for c in itertools.combinations(range(n), k)]


def main():
    # ------------------------------------------------------------- (1)
    print("(1) Theorem 1 (p. 550)")
    n = 4
    pi = sp.symbols("p0:4", positive=True)
    f = sp.symbols("f0:4")
    g = sp.symbols("g0:4")
    A = {0, 2, 3}
    lhs = eu(pi, splice(f, A, g)) - eu(pi, g)
    q = bayes(pi, A)
    rhs = prob(pi, A) * (eu(q, f) - eu(q, g))
    check("EU(fAg) - EU(g) = pi(A) [EU_A(f) - EU_A(g)] (symbolic, 4 states)",
          sp.simplify(lhs - rhs) == 0)
    qs = sp.symbols("q0:4")
    eqs = [qs[w] * pi[w2] - qs[w2] * pi[w] for w in A for w2 in A if w < w2]
    eqs += [qs[w] for w in range(n) if w not in A] + [sum(qs) - 1]
    sol = sp.solve(eqs, qs, dict=True)
    check("relative-likelihood + support + normalisation equations have the unique "
          "solution q = Bayes", len(sol) == 1 and all(sp.simplify(sol[0][qs[w]] - q[w]) == 0
                                                     for w in range(n)))

    # ------------------------------------------------------------- (2)
    print("(2) worked example (Ortoleva.lean, ex_*)")
    P = [(R(24, 25), R(3, 100), R(1, 100)), (R(1, 5), R(1, 5), R(3, 5))]
    rho = (R(9, 10), R(1, 10))
    eps = R(1, 20)
    pi = P[0]
    check("candidate priors are beliefs; rho sums to 1; pi is the unique mode of rho",
          all(sum(p) == 1 and min(p) >= 0 for p in P) and sum(rho) == 1 and rho[0] > rho[1])
    ties = [E for E in nonempty_events(3) if score(P, rho, E, 0) == score(P, rho, E, 1)]
    check("no score ties on any nonempty event (ex_no_tie)", ties == [])
    A = frozenset({1, 2})
    check("pi(A) = 1/25 <= eps (ex_prob_A, ex_unexpected)", prob(pi, A) == R(1, 25) and prob(pi, A) <= eps)
    check("scores on A: 9/250 for pi < 2/25 for pi' (ex_sel_A)",
          score(P, rho, A, 0) == R(9, 250) and score(P, rho, A, 1) == R(2, 25)
          and select(P, rho, A) == 1)
    check("Bayes posterior (0, 3/4, 1/4) (ex_bayes_A)", bayes(pi, A) == (0, R(3, 4), R(1, 4)))
    check("HT posterior (0, 1/4, 3/4) (ex_ht_A)", ht_post(P, rho, 0, eps, A) == (0, R(1, 4), R(3, 4)))
    B = frozenset({0, 1})
    check("likely news {a,b}: pi = 99/100 > eps and HT = Bayes (ex_likely)",
          prob(pi, B) == R(99, 100) and ht_post(P, rho, 0, eps, B) == bayes(pi, B))
    f1 = (0, 1, 0)
    g1 = (0, 0, 1)
    post = ht_post(P, rho, 0, eps, A)
    check("DC violated: EU_pi(fAg) = 3/100 >= EU_pi(g) = 1/100 but EU_A(f) = 1/4 < EU_A(g) = 3/4",
          eu(pi, splice(f1, A, g1)) == R(3, 100) and eu(pi, g1) == R(1, 100)
          and eu(post, f1) == R(1, 4) and eu(post, g1) == R(3, 4))
    # which events trigger the change of prior in the example
    trig = [sorted(E) for E in nonempty_events(3)
            if prob(pi, E) <= eps and ht_post(P, rho, 0, eps, E) != bayes(pi, E)]
    print("    events where HT departs from Bayes:", trig)
    check("in the example HT departs from Bayes only on {b, c}", trig == [[1, 2]])

    # ------------------------------------------------------------- (3)-(5)
    print("(3)-(5) random exact sweep")
    rng = random.Random(20260930)
    n_models = 0
    cons_ok = coh_ok = True
    n_cycles = 0
    eps0_ok = True
    dc_pred_ok = True
    n_dc_fail = 0
    while n_models < 300:
        n = rng.choice([3, 4])
        k = rng.choice([2, 3, 4])
        P = []
        for _ in range(k):
            w = [rng.randint(0, 6) for _ in range(n)]
            if sum(w) == 0:
                w[0] = 1
            P.append(tuple(R(x, sum(w)) for x in w))
        # guarantee `full`: one candidate with full support
        w = [rng.randint(1, 6) for _ in range(n)]
        P.append(tuple(R(x, sum(w)) for x in w))
        k += 1
        r = [rng.randint(1, 20) for _ in range(k)]
        rho = tuple(R(x, sum(r)) for x in r)
        if sorted(rho)[-1] == sorted(rho)[-2]:
            continue
        i0 = rho.index(max(rho))
        events = nonempty_events(n)
        if any(select(P, rho, E) is None for E in events):
            continue  # fn. 18: unique maximiser required
        eps = R(rng.randint(0, 19), 20)
        n_models += 1
        post = {E: ht_post(P, rho, i0, eps, E) for E in events}
        # Consequentialism
        for E in events:
            q = post[E]
            if prob(q, E) != 1 or any(q[w] != 0 for w in range(n) if w not in E):
                cons_ok = False
        # Dynamic Coherence, cycles of length 2 and 3
        for L in (2, 3):
            for cyc in itertools.product(events, repeat=L):
                if all(prob(post[cyc[i]], cyc[(i + 1) % L]) == 1 for i in range(L)):
                    n_cycles += 1
                    if post[cyc[0]] != post[cyc[-1]]:
                        coh_ok = False
        # eps = 0 is Bayes on positive events
        pi = P[i0]
        for E in events:
            if prob(pi, E) > 0 and ht_post(P, rho, i0, R(0), E) != bayes(pi, E):
                eps0_ok = False
        # DC fails iff a triggered event has a different conditional
        pred_fail = any(0 < prob(pi, E) <= eps and post[E] != bayes(pi, E) for E in events)
        # test DC on the acts used in the proof of Theorem 1 (Lean: thm1_only_if):
        # f = pi(w) 1_{w'}, g = pi(w') 1_w for w, w' in A, plus unit acts
        dc_fail = False
        for E in events:
            if prob(pi, E) == 0:
                continue
            acts = []
            for a in range(n):
                for b in range(n):
                    acts.append((tuple(R(1) if x == a else R(0) for x in range(n)),
                                 tuple(R(1) if x == b else R(0) for x in range(n))))
                    if a in E and b in E:
                        acts.append((tuple(pi[a] if x == b else R(0) for x in range(n)),
                                     tuple(pi[b] if x == a else R(0) for x in range(n))))
            for fa, gb in acts:
                lhs = eu(pi, gb) <= eu(pi, splice(fa, E, gb))
                rhs = eu(post[E], gb) <= eu(post[E], fa)
                if lhs != rhs:
                    dc_fail = True
        if dc_fail:
            n_dc_fail += 1
        if dc_fail != pred_fail:
            dc_pred_ok = False
        if pred_fail:
            # HT(eps) and HT(0) differ somewhere: eps is not minimal at 0
            if all(post[E] == ht_post(P, rho, i0, R(0), E) for E in events):
                dc_pred_ok = False
    print(f"    {n_models} models, {n_cycles} coherence cycles tested, "
          f"{n_dc_fail} models violate Dynamic Consistency")
    check("Consequentialism: every HT posterior is supported in A with mass 1", cons_ok)
    check("Dynamic Coherence holds on every tested cycle (Theorem 2, 'if')", coh_ok and n_cycles > 0)
    check("eps = 0: HT = Bayes on every positive-probability event", eps0_ok)
    check("DC fails exactly when a triggered event (0 < pi(A) <= eps) changes the "
          "conditional; then HT(eps) != HT(0)", dc_pred_ok and n_dc_fail > 0)

    print("\nAll checks passed." if OK else "\nSOME CHECKS FAILED.")
    return OK


if __name__ == "__main__":
    sys.exit(0 if main() else 1)
