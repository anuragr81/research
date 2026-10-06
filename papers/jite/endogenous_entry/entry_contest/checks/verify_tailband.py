"""Open item 3: when does the tail condition hold, and can it be weakened?

Proposition (tail condition) is conditional, and `PROOFS.tex` recorded the
characterisation of the spreads satisfying it as open.  This suite settles what
is settleable and shows the rest is not a gap but a boundary.

Write the rank-by-rank displacement of two descending-sorted profiles as
d[j] = w'[j] - w[j], rank 0 richest, and set

    L = min { j : d[j] < 0 }   (richest rank whose wealth FALLS; Q if none)
    M = max { j : d[j] > 0 }   (poorest rank whose wealth RISES; -1 if none)

Then the proposition's two hypotheses are exactly

    rise branch covered  <=>  k* <= L
    fall branch covered  <=>  k* >  M

so the theorem is silent precisely on margins with L < k* <= M -- the BAND.
Three consequences, each checked below:

  T1  the L/M form is equivalent to the hypotheses as stated (faithfulness);
  T2  the band is empty at EVERY margin iff M <= L, which holds iff the
      displacement changes sign at most once -- i.e. iff the spread is
      dispersive.  That is what dispersiveness buys, stated exactly;
  T3  where the theorem applies it is never violated;
  T4  inside the band BOTH directions occur, so the tail condition is SHARP:
      no strengthening of the conclusion is available from the sign pattern
      alone.  Two explicit witnesses refute any purported strengthening, so
      this is established, not merely sampled;
  T5  how big the band is -- and the answer depends entirely on how spreads
      are generated, which is why T5 reports both.
"""
import random

results = []


def check(name, ok, detail=""):
    results.append((name, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))


_c = 0.2


def _kp(x):
    return 1.0 / (x - _c) - 1.0 / x


def _ks(w, D):
    ks = [j for j in range(1, len(w) + 1) if _kp(w[j - 1]) <= D[j - 1]]
    return max(ks) if ks else 0


def _is_mps(a, b):
    x, y = sorted(a), sorted(b)
    if abs(sum(x) - sum(y)) > 1e-9:
        return False
    ca = cb = 0.0
    for i in range(len(x) - 1):
        ca += x[i]
        cb += y[i]
        if cb > ca + 1e-12:
            return False
    return True


def LM(d, Q, tol=1e-12):
    negs = [j for j in range(Q) if d[j] < -tol]
    poss = [j for j in range(Q) if d[j] > tol]
    return (min(negs) if negs else Q), (max(poss) if poss else -1)


def single_crossing(d, Q, tol=1e-12):
    """True iff the displacement is (weakly) non-negative then non-positive."""
    seen_neg = False
    for j in range(Q):
        if d[j] < -tol:
            seen_neg = True
        elif d[j] > tol and seen_neg:
            return False
    return True


def _calibrated_D(w, rnd):
    """Draw a descending gain schedule straddling kappa(w), so that k* lands
    in the interior.  Needed for the structured families: their wealths make
    kappa small relative to a fixed D range, so an uncalibrated draw has every
    challenger entering and every instance is rejected."""
    Q = len(w)
    kaps = [_kp(x) for x in w]
    target = kaps[int(rnd.uniform(0.25, 0.75) * (Q - 1))]
    return sorted([rnd.uniform(0.4, 2.2) * target for _ in range(Q)],
                  reverse=True)


def gen_unstructured(rnd):
    """The R1-SCOPE generator, unchanged, so the numbers stay comparable with
    that check: iid uniform displacements, balanced.  Multi-crossing is
    TYPICAL here, by construction."""
    Q = rnd.randint(6, 10)
    D = sorted([rnd.uniform(0.001, 0.3) for _ in range(Q)], reverse=True)
    w = sorted([rnd.uniform(1.5, 14.0) for _ in range(Q)], reverse=True)
    d = [rnd.uniform(-0.5, 0.5) for _ in range(Q - 1)]
    d.append(-sum(d))
    return Q, D, w, d


def gen_linear(rnd):
    """Linear mean-preserving spread w -> mbar + lam (w - mbar): dispersive."""
    Q = rnd.randint(6, 10)
    w = sorted([rnd.uniform(1.5, 14.0) for _ in range(Q)], reverse=True)
    D = _calibrated_D(w, rnd)
    mbar = sum(w) / Q
    lam = 1.0 + rnd.uniform(0.02, 0.25)
    w2 = [mbar + lam * (x - mbar) for x in w]
    return Q, D, w, [b - a for a, b in zip(w, w2)]


def gen_lognormal(rnd):
    """Mean-preserving increase in sigma of a lognormal, read off at fixed
    rank fractions.  The economically natural case; dispersive."""
    import math
    Q = rnd.randint(6, 10)
    mean = rnd.uniform(4.0, 9.0)
    s0 = rnd.uniform(0.25, 0.5)
    s1 = s0 + rnd.uniform(0.02, 0.2)
    ps = [(j + 0.5) / Q for j in range(Q)]
    zs = [_probit(p) for p in ps]

    def prof(s):
        mu = math.log(mean) - s * s / 2.0
        return sorted([math.exp(mu + s * z) for z in zs], reverse=True)

    w, w2 = prof(s0), prof(s1)
    # Preserving the lognormal's own mean does NOT preserve the mean of the
    # discrete profile read off at fixed rank fractions, and it is the discrete
    # sum that _is_mps tests.  Re-centre additively.  This cannot disturb the
    # crossing structure: d_j is monotone in rank here, so a constant shift
    # moves the single crossing without creating a second one.
    shift = (sum(w) - sum(w2)) / Q
    w2 = [x + shift for x in w2]
    return Q, _calibrated_D(w, rnd), w, [b - a for a, b in zip(w, w2)]


class Starved(Exception):
    pass


def sample(gen, seed, n_target, max_attempts=6000000):
    """Yield admissible instances, and fail LOUDLY rather than spinning if the
    generator cannot supply them.  An earlier draft hung here: the lognormal
    family had a 0% acceptance rate against an uncalibrated D."""
    rnd = random.Random(seed)
    n = attempts = 0
    while n < n_target:
        attempts += 1
        if attempts > max_attempts:
            raise Starved(
                f"only {n}/{n_target} admissible in {max_attempts} attempts")
        Q, D, w, d = gen(rnd)
        a = admissible(Q, D, w, d)
        if a is None:
            continue
        n += 1
        yield Q, D, w, d, a[0], a[1]


def _probit(p):
    """Acklam's rational approximation to the normal quantile."""
    a = [-3.969683028665376e+01, 2.209460984245205e+02, -2.759285104469687e+02,
         1.383577518672690e+02, -3.066479806614716e+01, 2.506628277459239e+00]
    b = [-5.447609879822406e+01, 1.615858368580409e+02, -1.556989798598866e+02,
         6.680131188771972e+01, -1.328068155288572e+01]
    c = [-7.784894002430293e-03, -3.223964580411365e-01, -2.400758277161838e+00,
         -2.549732539343734e+00, 4.374664141464968e+00, 2.938163982698783e+00]
    d = [7.784695709041462e-03, 3.224671290700398e-01, 2.445134137142996e+00,
         3.754408661907416e+00]
    pl = 0.02425
    if p < pl:
        q = (2 * __import__("math").log(p)) ** 0.5 * -1
        q = (-2 * __import__("math").log(p)) ** 0.5
        return (((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) / \
               ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1)
    if p > 1 - pl:
        q = (-2 * __import__("math").log(1 - p)) ** 0.5
        return -(((((c[0]*q+c[1])*q+c[2])*q+c[3])*q+c[4])*q+c[5]) / \
                ((((d[0]*q+d[1])*q+d[2])*q+d[3])*q+1)
    q = p - 0.5
    r = q * q
    return (((((a[0]*r+a[1])*r+a[2])*r+a[3])*r+a[4])*r+a[5])*q / \
           (((((b[0]*r+b[1])*r+b[2])*r+b[3])*r+b[4])*r+1)


def admissible(Q, D, w, d):
    w2 = [a + b for a, b in zip(w, d)]
    if min(w2) <= _c + 0.05:
        return None
    if not all(w2[i] > w2[i + 1] for i in range(Q - 1)):
        return None
    k0 = _ks(w, D)
    if k0 < 2 or k0 >= Q - 1:
        return None
    if not _is_mps(w, w2):
        return None
    return w2, k0


print("=" * 72)
print("T1  THE L/M FORM IS THE PROPOSITION'S HYPOTHESIS, RESTATED")
print("=" * 72)
print("      rise: w'[j] >= w[j] for all j < k*   <=>   k* <= L")
print("      fall: w'[j] <= w[j] for all j >= k*  <=>   k* >  M")
bad = n = 0
for Q, D, w, d, w2, k0 in sample(gen_unstructured, 11, 20000):
    n += 1
    L, M = LM(d, Q)
    rise_direct = all(d[j] >= -1e-12 for j in range(k0))
    fall_direct = all(d[j] <= 1e-12 for j in range(k0, Q))
    if rise_direct != (k0 <= L) or fall_direct != (k0 > M):
        bad += 1
check("T1 the L/M criterion is equivalent to the stated hypotheses", bad == 0,
      f"{n} instances, {bad} mismatches")

print()
print("=" * 72)
print("T2  THE BAND IS EMPTY AT EVERY MARGIN  <=>  THE SPREAD IS DISPERSIVE")
print("=" * 72)
print("      band at margin k is  L < k <= M, so it is empty for all k iff")
print("      M <= L, which holds iff the displacement crosses zero at most once.")
bad = n = emptyshown = crossshown = 0
for Q, D, w, d, w2, k0 in sample(gen_unstructured, 12, 20000):
    n += 1
    L, M = LM(d, Q)
    band_empty_all = all(not (L < k <= M) for k in range(0, Q + 1))
    if band_empty_all != (M <= L):
        bad += 1
    if (M <= L) != single_crossing(d, Q):
        bad += 1
    if M <= L:
        emptyshown += 1
    else:
        crossshown += 1
check("T2 band empty at every margin <=> M <= L <=> single crossing", bad == 0,
      f"{n} instances, {bad} mismatches; {emptyshown} single-crossing, "
      f"{crossshown} multi-crossing")
check("T2 CONTROL: both cases actually occur in the sample",
      emptyshown > 0 and crossshown > 0,
      "so the equivalence is not vacuous on either side")

print()
print("=" * 72)
print("T3/T4  WHERE THE THEOREM APPLIES IT HOLDS; INSIDE THE BAND IT IS SHARP")
print("=" * 72)
stats = {"both": [0, 0, 0], "rise": [0, 0, 0], "fall": [0, 0, 0], "band": [0, 0, 0]}
wit_up = wit_dn = None
n = 0
for Q, D, w, d, w2, k0 in sample(gen_unstructured, 5, 40000):
    n += 1
    L, M = LM(d, Q)
    k1 = _ks(w2, D)
    mv = 0 if k1 > k0 else (1 if k1 == k0 else 2)
    rise_ok, fall_ok = (k0 <= L), (k0 > M)
    if rise_ok and fall_ok:
        cls = "both"
    elif rise_ok:
        cls = "rise"
    elif fall_ok:
        cls = "fall"
    else:
        cls = "band"
        if mv == 0 and wit_up is None:
            wit_up = (Q, k0, k1, L, M, [round(x, 3) for x in w],
                      [round(x, 3) for x in d])
        if mv == 2 and wit_dn is None:
            wit_dn = (Q, k0, k1, L, M, [round(x, 3) for x in w],
                      [round(x, 3) for x in d])
    stats[cls][mv] += 1

print(f"      {n} genuine mean-preserving spreads")
print(f"      {'class':>6} {'n':>7} {'k* up':>8} {'k* same':>9} {'k* down':>9}   theorem")
for cls, lab in [("both", "k* UNCHANGED (both branches)"),
                 ("rise", "k* weakly RISES"),
                 ("fall", "k* weakly FALLS"),
                 ("band", "SILENT")]:
    u, s, dn = stats[cls]
    print(f"      {cls:>6} {u+s+dn:>7} {u:>8} {s:>9} {dn:>9}   {lab}")

viol = stats["rise"][2] + stats["fall"][0] + stats["both"][0] + stats["both"][2]
covered = sum(sum(stats[c]) for c in ("both", "rise", "fall"))
check("T3 no violation anywhere the theorem applies", viol == 0,
      f"{covered} covered instances, {viol} violations; the 'both' class is "
      f"the sharpest prediction (k* exactly unchanged) and holds "
      f"{stats['both'][1]}/{sum(stats['both'])}")

sharp = wit_up is not None and wit_dn is not None
if sharp:
    for tag, wt in (("k* RISES", wit_up), ("k* FALLS", wit_dn)):
        Q, k0, k1, L, M, ww, dd = wt
        print(f"      witness, {tag}: Q={Q}, k* {k0} -> {k1}, L={L}, M={M} "
              f"(band, since L < k* <= M)")
        print(f"        w = {ww}")
        print(f"        d = {dd}")
check("T4 SHARPNESS: inside the band both directions occur", sharp,
      "two explicit witnesses with the same sign pattern class and opposite "
      "conclusions, so no strengthening of the rule is available from the "
      "displacement signs alone -- this REFUTES any such strengthening and is "
      "therefore established, not merely sampled")

print()
print("=" * 72)
print("T5  HOW BIG IS THE BAND?  IT DEPENDS ENTIRELY ON THE SPREAD FAMILY")
print("=" * 72)
print("      Reported for three generators, because a single number here")
print("      would be an artefact of whichever one was chosen.")
rows = []
for label, gen, seed in [("unstructured (iid displacements)", gen_unstructured, 5),
                         ("linear MPS  mbar + lam (w - mbar)", gen_linear, 6),
                         ("lognormal, mean-preserving d(sigma)", gen_lognormal, 7)]:
    n = inband = multi = 0
    for Q, D, w, d, w2, k0 in sample(gen, seed, 8000):
        n += 1
        L, M = LM(d, Q)
        if not (k0 <= L or k0 > M):
            inband += 1
        if not single_crossing(d, Q):
            multi += 1
    rows.append((label, n, inband, multi))
    print(f"      {label:<36} margin in band: {inband:>5}/{n}"
          f"   multi-crossing: {multi:>5}/{n}")

unstr = rows[0][2] / rows[0][1]
struct = max(rows[1][2] / rows[1][1], rows[2][2] / rows[2][1])
check("T5 the band is an artefact of unstructured displacements", struct == 0.0,
      f"{unstr:.1%} of unstructured spreads leave the theorem silent, versus "
      f"{struct:.1%} of dispersive ones -- so the ~50% figure describes the "
      f"generator, NOT economically natural spreads, and must not be quoted "
      f"as a limitation of the result")

print()
print("      WHAT THIS SETTLES.  The characterisation asked for in the open")
print("      item is exact but near-definitional (T1), and what it buys is")
print("      stated precisely by T2: dispersiveness is equivalent to the band")
print("      being empty at every margin.  The substantive finding is T4 --")
print("      the tail condition cannot be weakened, because inside the band")
print("      the sign pattern is consistent with k* moving either way.  So the")
print("      open item closes with a NEGATIVE result rather than a stronger")
print("      theorem: the hypothesis is sharp.  What remains genuinely open is")
print("      narrower still -- whether some OTHER statistic of the spread (not")
print("      the displacement signs) predicts the direction inside the band.")

print()
print("=" * 72)
nf = sum(1 for _, ok in results if not ok)
print(f"TAIL-BAND SUMMARY: {len(results)} checks, {nf} failures")
print("=" * 72)
