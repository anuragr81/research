"""
Shmaya & Yariv, "Experiments on Decisions Under Uncertainty" (AER 2016 106(7)).

Exact-rational checks of the two theorems on a small tree (S = {0,1}, N = 2,
A = {0,1,2}), complementing the Lean development:

  (1) THEOREM 2 ("anything goes").  For *every* prefix-invariant observation map
      sigma on the tree -- all of them are enumerated -- the construction used in
      Lean (`alphaOf`: alpha reads sigma off the observed prefix, uniform weights)
      satisfies Definition 3: each conditioning event has positive weight, and
      sigma(s) is the unique argmax of the conditional law of alpha.

  (2) THEOREM 1, necessity.  A *restricted* conjectured experiment has nu
      independent of (alpha, varsigma), so the assessment at an instance s is
      P(alpha = a | varsigma|_{|s|} = s), and the parent's assessment is a convex
      combination of its children's.  Over many random restricted conjectures we
      compute the induced sigma and check the no-reversal condition of Theorem 1:
      if sigma(s^x) = a for every x, then sigma(s) = a.

  (3) The convex-combination identity underlying (2) is verified directly:
      P(alpha=a | prefix=s) == sum_x P(prefix=s^x | prefix=s) * P(alpha=a | prefix=s^x).
      This is the Lean lemma `argmax_of_convex_combination`'s hypothesis.

  (4) A reversing sigma is exhibited, and shown to be explainable by the
      unrestricted construction of (1) while violating the Theorem 1 condition --
      i.e. no restricted conjecture can produce it.
"""
from fractions import Fraction as F
from itertools import product
import random

S = [0, 1]        # signals
A = [0, 1, 2]     # alternatives
N = 2             # number of available signals


def instances():
    """All instances (nodes): prefixes of length 0..N."""
    out = []
    for n in range(N + 1):
        for s in product(S, repeat=n):
            out.append(s)
    return out


# ---------------------------------------------------------------- Theorem 2 ---
def check_theorem2():
    """Every sigma is explained by the unrestricted construction."""
    nodes = instances()
    inner = [s for s in nodes if len(s) < N]
    n_sigma = len(A) ** len(nodes)
    print(f"(1) Theorem 2: enumerating all {n_sigma} observation maps "
          f"on {len(nodes)} nodes")
    bad = 0
    for assign in product(A, repeat=len(nodes)):
        sigma = dict(zip(nodes, assign))
        # The construction: sample space = nodes, uniform weight 1, alpha = sigma.
        # Conditioning event of node s is {s} itself (nu pins the length, and the
        # first |s| signals pin the rest), so:
        for s in nodes:
            event = [w for w in nodes if len(w) == len(s) and w[:len(s)] == s]
            assert event == [s], f"event of {s} is {event}, expected [{s}]"
            wt_event = F(len(event))
            if wt_event <= 0:
                bad += 1
                continue
            joint = {a: F(sum(1 for w in event if sigma[w] == a)) for a in A}
            best = max(joint, key=lambda a: joint[a])
            uniq = sum(1 for a in A if joint[a] == joint[best]) == 1
            if best != sigma[s] or not uniq:
                bad += 1
    print(f"    violations: {bad}")
    return bad == 0


# ---------------------------------------------------------------- Theorem 1 ---
def restricted_sigma(joint):
    """
    sigma induced by a restricted conjectured experiment.

    `joint[(a, x)]` is P(alpha = a, varsigma = x) for a full realization x.
    Because nu is independent of (alpha, varsigma), conditioning on
    {nu = n, varsigma|n = s} is conditioning on {varsigma|n = s}.  Returns the
    argmax assessment at each node (None where tied).
    """
    sigma = {}
    for s in instances():
        n = len(s)
        tot = {a: F(0) for a in A}
        for (a, x), p in joint.items():
            if x[:n] == s:
                tot[a] += p
        best = max(tot, key=lambda a: tot[a])
        ties = sum(1 for a in A if tot[a] == tot[best])
        sigma[s] = best if ties == 1 else None
    return sigma


def assessment(joint, s, a):
    """P(alpha = a | varsigma|_{|s|} = s), as an exact Fraction (None if cond. prob undefined)."""
    n = len(s)
    num = sum(p for (aa, x), p in joint.items() if x[:n] == s and aa == a)
    den = sum(p for (aa, x), p in joint.items() if x[:n] == s)
    return None if den == 0 else F(num, 1) / den


def random_joint(rng):
    """A random strictly-positive joint law over (alpha, full realization)."""
    xs = list(product(S, repeat=N))
    raw = {(a, x): F(rng.randint(1, 12)) for a in A for x in xs}
    tot = sum(raw.values())
    return {k: v / tot for k, v in raw.items()}


def check_theorem1(trials=400, seed=0):
    """Necessity: a restricted conjecture's sigma never reverses."""
    rng = random.Random(seed)
    print(f"(2) Theorem 1 necessity: {trials} random restricted conjectures")
    reversals = 0
    for _ in range(trials):
        joint = random_joint(rng)
        sigma = restricted_sigma(joint)
        for s in [t for t in instances() if len(t) < N]:
            kids = [s + (x,) for x in S]
            vals = {sigma[k] for k in kids}
            if len(vals) == 1 and None not in vals:
                a = vals.pop()
                if sigma[s] != a:
                    reversals += 1
    print(f"    reversals found: {reversals}  (Theorem 1 predicts 0)")
    return reversals == 0


def check_convex_combination(trials=200, seed=1):
    """The convex-combination identity the necessity proof rests on."""
    rng = random.Random(seed)
    print(f"(3) convex-combination identity: {trials} random conjectures")
    bad = 0
    for _ in range(trials):
        joint = random_joint(rng)
        for s in [t for t in instances() if len(t) < N]:
            n = len(s)
            den = sum(p for (_, x), p in joint.items() if x[:n] == s)
            if den == 0:
                continue
            for a in A:
                parent = assessment(joint, s, a)
                tot = F(0)
                for x in S:
                    k = s + (x,)
                    wk = sum(p for (_, xx), p in joint.items() if xx[:len(k)] == k)
                    ch = assessment(joint, k, a)
                    if ch is not None:
                        tot += (F(wk, 1) / den) * ch
                if parent != tot:
                    bad += 1
    print(f"    mismatches: {bad}")
    return bad == 0


def check_reversing_example():
    """A reversing sigma: explainable unrestricted, impossible restricted."""
    print("(4) a reversing observation map")
    sigma = {s: 1 for s in instances()}
    sigma[()] = 0                    # both children of the root say 1, root says 0
    kids_agree = all(sigma[(x,)] == 1 for x in S)
    violates = kids_agree and sigma[()] != 1
    print(f"    sigma(root)={sigma[()]}, sigma(children)="
          f"{[sigma[(x,)] for x in S]}")
    print(f"    violates Theorem 1's condition: {violates}")
    # explainable by the unrestricted construction, since events are singletons
    ok_unrestricted = True
    for s in instances():
        event = [w for w in instances() if len(w) == len(s) and w[:len(s)] == s]
        joint = {a: sum(1 for w in event if sigma[w] == a) for a in A}
        best = max(joint, key=lambda a: joint[a])
        if best != sigma[s] or sum(1 for a in A if joint[a] == joint[best]) != 1:
            ok_unrestricted = False
    print(f"    explained by the unrestricted construction: {ok_unrestricted}")
    return violates and ok_unrestricted


def main():
    results = [check_theorem2(), check_theorem1(), check_convex_combination(),
               check_reversing_example()]
    print("\nAll checks passed." if all(results) else "\nSOME CHECKS FAILED.")
    return all(results)


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
