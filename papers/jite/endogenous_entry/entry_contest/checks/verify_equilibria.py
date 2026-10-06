import itertools
import random

res = []


def check(n, ok, d=""):
    res.append((n, ok))
    print(f"[{'PASS' if ok else 'FAIL'}] {n}" + (f"   {d}" if d else ""))


def nash_sets(Delta, kap):
    """Every pure-strategy entrant set, by exhaustive enumeration.

    Agents are indexed in increasing cost order.  In a set of size k a member
    faces k-1 rivals and stays iff kap[i] <= Delta[k-1]; an outsider would face
    k rivals on joining and stays out iff kap[i] > Delta[k].
    """
    Q = len(kap)
    out = []
    for r in range(Q + 1):
        for S in itertools.combinations(range(Q), r):
            S = set(S)
            k = len(S)
            ok = True
            for i in range(Q):
                if i in S:
                    if not (kap[i] <= Delta[k - 1]):
                        ok = False
                        break
                else:
                    if not (kap[i] > Delta[k]):
                        ok = False
                        break
            if ok:
                out.append(frozenset(S))
    return out


def kstar_of(Delta, kap):
    """Largest k with kap[j-1] <= Delta[j-1] for all j <= k (single crossing)."""
    ks = 0
    for j in range(1, len(kap) + 1):
        if kap[j - 1] <= Delta[j - 1]:
            ks = j
        else:
            break
    return ks


print("=" * 72)
print("P5-inv.  THE EQUILIBRIUM COUNT IS UNIQUE ACROSS ALL PURE EQUILIBRIA")
print("=" * 72)
print("  Added 2 Sep 2026 after SOUNDNESS_20260902.md found that the prose")
print("  claim 'identities pinned down by the wealth ordering' is FALSE.")
print("  The Lean development proves prefix structure WITHIN the assortative")
print("  schedule; nothing there quantifies over arbitrary entrant sets, so")
print("  nothing there could have failed.  This suite enumerates entrant sets")
print("  directly.")
print()

print("E1  The published counterexample: identities are NOT pinned")
print("-" * 72)
Delta_ce = [12, 10, 1]
kap_ce = [2, 5, 8]
eqs_ce = nash_sets(Delta_ce, kap_ce)
sets_ce = sorted(sorted(s) for s in eqs_ce)
counts_ce = {len(s) for s in eqs_ce}
richest_absent = [s for s in eqs_ce if len(s) > 0 and 0 not in s]
print(f"      Delta = {Delta_ce}   (strictly antitone, as P3-cor requires)")
print(f"      kappa = {kap_ce}   (increasing in index = decreasing in wealth)")
print(f"      equilibria: {sets_ce}")
print(f"      equilibrium with the richest agent ABSENT: "
      f"{sorted(richest_absent[0]) if richest_absent else 'none'}")
check("E1 multiple equilibria with distinct identity sets",
      len(eqs_ce) == 3 and len(richest_absent) == 1,
      "so 'identities pinned by the wealth ordering' is false")

print()
print("E2  ... yet the COUNT is the same in every one of them")
print("-" * 72)
print(f"      counts across the three equilibria: {sorted(counts_ce)}")
print(f"      k* = {kstar_of(Delta_ce, kap_ce)}")
check("E2 count is invariant on the counterexample",
      counts_ce == {kstar_of(Delta_ce, kap_ce)},
      "this is what P7, P8, P9, P9-gen, P9-str and P-MU are about")

print()
print("E3  Randomised: count uniqueness, prefix existence, multiplicity")
print("-" * 72)
random.seed(20260902)
N_INST = 20000
count_ok = prefix_ok = multi = richest_out = no_eq = 0
cond_holds = cond_unique = 0
for _ in range(N_INST):
    Q = random.randint(3, 6)
    D = sorted([random.random() for _ in range(Q + 1)], reverse=True)
    K = sorted([random.random() for _ in range(Q)])
    eqs = nash_sets(D, K)
    ks = kstar_of(D, K)
    if not eqs:
        no_eq += 1
        continue
    if {len(S) for S in eqs} == {ks}:
        count_ok += 1
    if frozenset(range(ks)) in eqs:
        prefix_ok += 1
    if len(eqs) > 1:
        multi += 1
    if any(len(S) > 0 and 0 not in S for S in eqs):
        richest_out += 1
    if ks == 0 or ks == Q or K[ks] > D[ks - 1]:
        cond_holds += 1
        if len(eqs) == 1:
            cond_unique += 1
tot = N_INST - no_eq
print(f"      instances {N_INST}, Q in 3..6, seed 20260902")
print(f"      no pure equilibrium exists      : {no_eq}")
print(f"      count unique and equal to k*    : {count_ok} / {tot}")
print(f"      assortative prefix is an eq     : {prefix_ok} / {tot}")
print(f"      multiple equilibria (identities): {multi}  ({100*multi/tot:.1f}%)")
print(f"        ... incl. richest absent      : {richest_out}")
check("E3 the count equals k* in every equilibrium of every instance",
      count_ok == tot and prefix_ok == tot and no_eq == 0,
      "existence and count-invariance both hold throughout")

print()
print("E4  Identity multiplicity is common, so E1 is not a knife edge")
print("-" * 72)
check("E4 multiplicity occurs in a substantial fraction of instances",
      multi > tot // 20,
      f"{multi} of {tot} instances ({100*multi/tot:.1f}%) have >1 equilibrium")

print()
print("E5  Sufficient condition for identity uniqueness")
print("-" * 72)
print("      If kappa_(k*+1) > Delta(k*-1) -- the first excluded agent would")
print("      not enter even at the most favourable slot -- the assortative")
print("      equilibrium is the ONLY one (Lean: members_below_kstar).")
print(f"      condition holds in {cond_holds} instances; "
      f"equilibrium unique in {cond_unique} of those")
check("E5 the condition implies a unique equilibrium, without exception",
      cond_holds > 0 and cond_unique == cond_holds)

print()
print("E6  CONTROL: drop antitonicity of Delta and count uniqueness FAILS")
print("-" * 72)
print("      P3-cor gives a strictly antitone Delta.  If that is removed the")
print("      conclusion does not survive, so E2/E3 are testing the hypothesis")
print("      rather than restating a tautology.")
random.seed(7)
broken = 0
trials = 4000
for _ in range(trials):
    Q = random.randint(3, 5)
    D = [random.random() for _ in range(Q + 1)]  # NOT sorted: antitonicity gone
    K = sorted([random.random() for _ in range(Q)])
    eqs = nash_sets(D, K)
    if len({len(S) for S in eqs}) > 1:
        broken += 1
print(f"      instances with equilibria of DIFFERENT sizes: {broken} / {trials}")
check("E6 antitonicity of Delta is load-bearing", broken > 0,
      "without it, equilibria of different sizes coexist")

print()
print("=" * 72)
print("N4.  ASSORTATIVE ENTRY IS THE CHEAPEST EQUILIBRIUM")
print("=" * 72)
print("      Every equilibrium has count k* (E2/E3). Delta depends on the")
print("      count alone, so aggregate surplus differs across equilibria only")
print("      through the total entry cost actually paid.")

def gain_anon(S):
    return 40.0 * len(S)


def gain_id(S):
    return 40.0 * len(S) + 0.5 * sum(S)


random.seed(31337)
surplus_id_gap = 0
id_pairs = 0
not_eq = 0
not_min = 0
surplus_mismatch = 0
inst = 0
multi = 0
for _ in range(6000):
    Q = random.randint(3, 7)
    D = sorted([random.uniform(0, 20) for _ in range(Q)], reverse=True)
    kap = sorted([random.uniform(0, 20) for _ in range(Q)])
    eqs = []
    for r in range(Q + 1):
        for S in itertools.combinations(range(Q), r):
            k = len(S)
            ok = all(kap[i] <= D[k - 1] for i in S)
            if k < Q:
                ok = ok and all(kap[j] > D[k] for j in range(Q) if j not in S)
            if ok:
                eqs.append(S)
    if not eqs:
        continue
    ks = [j for j in range(1, Q + 1) if kap[j - 1] <= D[j - 1]]
    kstar = max(ks) if ks else 0
    assort = tuple(range(kstar))
    inst += 1
    if len(eqs) > 1:
        multi += 1
    if assort not in eqs:
        not_eq += 1
    costs = {S: sum(kap[i] for i in S) for S in eqs}
    if abs(costs[assort] - min(costs.values())) > 1e-12:
        not_min += 1
    for S in eqs:
        lhs = (gain_anon(S) - costs[S]) - (gain_anon(assort) - costs[assort])
        rhs = costs[assort] - costs[S]
        if abs(lhs - rhs) > 1e-12:
            surplus_mismatch += 1
        lhs_id = (gain_id(S) - costs[S]) - (gain_id(assort) - costs[assort])
        if abs(lhs_id - rhs) > 1e-12:
            surplus_id_gap += 1
        id_pairs += 1

print(f"      instances: {inst}   with identity multiplicity: {multi}")
check("N4 the assortative set is always an equilibrium", not_eq == 0,
      f"{inst} instances")
check("N4 it minimises total realised entry cost among all equilibria",
      not_min == 0, f"{inst} instances, {multi} with a genuine choice")
check("N4 surplus gap equals cost gap under an ANONYMOUS gain schedule",
      surplus_mismatch == 0,
      f"{id_pairs} equilibrium pairs, gain a function of the count alone")
check("N4 CONTROL: with an identity-DEPENDENT gain it does not",
      surplus_id_gap > 0,
      f"{surplus_id_gap} of {id_pairs} pairs break once gain depends on who "
      f"entered, so anonymity is the load-bearing hypothesis")

worse = 0
random.seed(4)
for _ in range(4000):
    Q = random.randint(3, 7)
    D = sorted([random.uniform(0, 20) for _ in range(Q)], reverse=True)
    kap = sorted([random.uniform(0, 20) for _ in range(Q)])
    ks = [j for j in range(1, Q + 1) if kap[j - 1] <= D[j - 1]]
    kstar = max(ks) if ks else 0
    if kstar == 0 or kstar >= Q:
        continue
    assort = sum(kap[i] for i in range(kstar))
    other = sum(kap[i] for i in list(range(kstar - 1)) + [kstar])
    if other > assort:
        worse += 1
print(f"      CONTROL: a non-assortative count-k* set is strictly costlier in")
print(f"      {worse} cases, so the selection is not vacuous")
check("N4 CONTROL: non-assortative sets are strictly costlier", worse > 0,
      f"{worse} cases")

print()
print("=" * 72)
print("N4-WELFARE.  WHAT THE COST RANKING IS, AND WHAT IT IS NOT")
print("=" * 72)
print("      The selection ranks equilibria by total kappa, a sum of UTILITY")
print("      increments across agents. In RESOURCE terms every equilibrium")
print("      costs k* times c and they are indistinguishable, so the ranking")
print("      carries no resource-efficiency content; and because kappa falls")
print("      in wealth, it systematically favours the richest entrants.")

random.seed(20260903)
res_tie = 0
res_diff = 0
util_diff = 0
rich_pref = 0
rich_tie = 0
multi_w = 0
for _ in range(6000):
    Q = random.randint(3, 7)
    D = sorted([random.uniform(0, 20) for _ in range(Q)], reverse=True)
    kap = sorted([random.uniform(0, 20) for _ in range(Q)])
    eqs = []
    for r in range(Q + 1):
        for S in itertools.combinations(range(Q), r):
            k = len(S)
            ok = all(kap[i] <= D[k - 1] for i in S)
            if k < Q:
                ok = ok and all(kap[j] > D[k] for j in range(Q) if j not in S)
            if ok:
                eqs.append(S)
    if len(eqs) < 2:
        continue
    multi_w += 1
    sizes = {len(S) for S in eqs}
    if len(sizes) == 1:
        res_tie += 1
    else:
        res_diff += 1
    ucosts = {S: sum(kap[i] for i in S) for S in eqs}
    if max(ucosts.values()) - min(ucosts.values()) > 1e-12:
        util_diff += 1
    ks = [j for j in range(1, Q + 1) if kap[j - 1] <= D[j - 1]]
    kstar = max(ks) if ks else 0
    assort = tuple(range(kstar))
    idx = {S: sum(S) for S in eqs}
    others = [v for S, v in idx.items() if S != assort]
    if others and idx.get(assort, 0) < min(others):
        rich_pref += 1
    elif others and idx.get(assort, 0) == min(others):
        rich_tie += 1

print(f"      instances with genuine multiplicity: {multi_w}")
print(f"      all equilibria share the same size (hence the same resource")
print(f"        cost k*c): {res_tie}; differing sizes: {res_diff}")
print(f"      instances where total kappa DOES differ across equilibria:"
      f" {util_diff}")
print(f"      assortative set has the strictly lowest index sum, i.e. the")
print(f"        wealthiest members: {rich_pref}  (ties: {rich_tie})")
check("N4-WELFARE resource cost is identical across all equilibria",
      res_diff == 0, f"{multi_w} multiplicity instances, all same size")
check("N4-WELFARE so the ranking is driven entirely by the utility metric",
      util_diff > 0, f"{util_diff} of {multi_w} have a strict kappa gap")
check("N4-WELFARE and its direction is to favour the wealthiest entrants",
      rich_pref > 0 and rich_pref + rich_tie == multi_w,
      f"{rich_pref} strict, {rich_tie} tied, {multi_w} total")

print()
print("=" * 72)
nf = sum(1 for _, ok in res if not ok)
print(f"SUMMARY: {len(res)} checks, {nf} failures")
print("=" * 72)
raise SystemExit(1 if nf else 0)
