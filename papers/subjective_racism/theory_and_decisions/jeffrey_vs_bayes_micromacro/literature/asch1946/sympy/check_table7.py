"""
Asch (1946), "Forming Impressions of Personality", J. Abnormal and Social
Psychology 41, 258-290 -- Experiment VI, Tables 7 and 8.

Own transcription, read from the rendered scan (PDF p.14 = journal p.271 for
Table 7; PDF p.15 = journal p.272 for Table 8), not from the text layer.

Table 7, "Choice of fitting qualities (percentages)".  Each cell is the
percentage of subjects who, on Check List I (Table 1, p.262: a forced choice
within each pair of "mostly opposite" terms), chose the listed member of the
pair.  Columns: Experiment VI, intelligent->envious (N=34) and
envious->intelligent (N=24); Experiment VII, intelligent->evasive (N=46) and
evasive->intelligent (N=53).

Checks:
  (1) every Table 7 number quoted in the project (notes/paper_review_log.md
      530-533, notes/manuscript_change_plan.md 816-818, 842-845) against the
      transcription, in the right column;
  (2) Table 8 counts sum to N, and the printed percentages against the counts;
  (3) summary statistics of the Exp VI order effect across the 18 traits,
      including an exact two-sided Fisher test per trait on counts
      reconstructed as round(p * N) -- a reconstruction, since Asch notes that
      some subjects left pairs blank (p.263), so per-cell denominators vary;
  (4) whether each cell is attainable as a rounded percentage of the nominal N.
"""
from fractions import Fraction as F
from math import comb

OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


# trait (listed member), opposite, IE (N=34), EI (N=24), IEv (N=46), EvI (N=53)
TABLE7 = [
    ("generous", "ungenerous", 24, 10, 42, 23),
    ("wise", "shrewd", 18, 17, 35, 19),
    ("happy", "unhappy", 32, 5, 51, 49),
    ("good-natured", "irritable", 18, 0, 54, 37),
    ("humorous", "humorless", 52, 21, 53, 29),
    ("sociable", "unsociable", 56, 27, 50, 48),
    ("popular", "unpopular", 35, 14, 44, 39),
    ("reliable", "unreliable", 84, 91, 96, 94),
    ("important", "insignificant", 85, 90, 77, 89),
    ("humane", "ruthless", 36, 21, 49, 46),
    ("good-looking", "unattractive", 74, 35, 59, 53),
    ("persistent", "unstable", 82, 87, 94, 100),
    ("serious", "frivolous", 97, 100, 44, 100),
    ("restrained", "talkative", 64, 9, 91, 91),
    ("altruistic", "self-centered", 6, 5, 32, 25),
    ("imaginative", "hard-headed", 26, 14, 37, 16),
    ("strong", "weak", 94, 73, 74, 96),
    ("honest", "dishonest", 80, 79, 66, 81),
]
N_IE, N_EI, N_IEV, N_EVI = 34, 24, 46, 53

# Table 8, ranking of "envious": rank -> (N, % ) for I->E and E->I
TABLE8 = {1: (5, 15, 7, 29), 2: (4, 11, 4, 17), 3: (5, 15, 5, 21),
          4: (3, 9, 2, 8), 5: (4, 11, 2, 8), 6: (13, 39, 4, 17)}

# numbers quoted by the project: trait -> (IE, EI)
QUOTED = {
    "restrained": (64, 9), "good-looking": (74, 35), "serious": (97, 100),
    "persistent": (82, 87), "reliable": (84, 91), "humorous": (52, 21),
    "good-natured": (18, 0), "important": (85, 90),
}


def fisher_two_sided(a, b, c, d):
    """Exact two-sided Fisher test for [[a, b], [c, d]] (sum of tables with
    probability <= observed)."""
    r1, r2, c1 = a + b, c + d, a + c
    n = r1 + r2
    def p(x):
        return F(comb(r1, x) * comb(r2, c1 - x), comb(n, c1))
    obs = p(a)
    lo, hi = max(0, c1 - r2), min(r1, c1)
    return float(sum(p(x) for x in range(lo, hi + 1) if p(x) <= obs))


def rnd(x):
    """Round half up, as printed tables of the period do."""
    return int(F(x) + F(1, 2))


def attainable(pct, n):
    return any(rnd(F(100 * k, n)) == pct for k in range(n + 1))


def main():
    rows = {t[0]: t for t in TABLE7}
    print("(1) Numbers quoted by the project")
    check("18 traits, in Check List I order", len(TABLE7) == 18)
    for trait, (ie, ei) in QUOTED.items():
        r = rows[trait]
        check(f"{trait:13s} I->E {ie:3d} / E->I {ei:3d}  (transcription {r[2]}/{r[3]})",
              (r[2], r[3]) == (ie, ei))

    print("(2) Table 8")
    ie_n = sum(v[0] for v in TABLE8.values())
    ei_n = sum(v[2] for v in TABLE8.values())
    check(f"I->E counts sum to 34: {ie_n}", ie_n == N_IE)
    check(f"E->I counts sum to 24: {ei_n}", ei_n == N_EI)
    check("printed percentages each sum to 100",
          sum(v[1] for v in TABLE8.values()) == 100 and sum(v[3] for v in TABLE8.values()) == 100)
    for rank, (n1, p1, n2, p2) in TABLE8.items():
        r1, r2 = rnd(F(100 * n1, N_IE)), rnd(F(100 * n2, N_EI))
        note = "" if (r1, r2) == (p1, p2) else f"   <- printed {p1}/{p2}, counts give {r1}/{r2}"
        print(f"    rank {rank}: I->E {n1}/34 = {float(F(100*n1, 34)):5.1f}%  E->I {n2}/24 = "
              f"{float(F(100*n2, 24)):5.1f}%{note}")
    print("    (I->E ranks 2, 5 and 6 are printed 11, 11, 39 where 4/34 and 13/34 round to 12 and 38;"
          " exact rounding would total 101, so Asch evidently forced the column to 100.)")
    check("modal rank: 'envious' 6th under I->E (13/34), 1st under E->I (7/24)",
          max(TABLE8, key=lambda k: TABLE8[k][0]) == 6 and max(TABLE8, key=lambda k: TABLE8[k][2]) == 1)
    print(f"    share at the modal rank: I->E {float(F(13,34)):.2f}, E->I {float(F(7,24)):.2f}")

    print("(3) Exp VI order effect across the 18 traits (d = I->E minus E->I, percentage points)")
    d = [(t[0], t[2] - t[3]) for t in TABLE7]
    pos = [x for x in d if x[1] > 0]
    neg = [x for x in d if x[1] < 0]
    zer = [x for x in d if x[1] == 0]
    mean = F(sum(x[1] for x in d), 18)
    mabs = F(sum(abs(x[1]) for x in d), 18)
    srt = sorted(abs(x[1]) for x in d)
    med = F(srt[8] + srt[9], 2)
    print(f"    positive (more favourable when intelligent comes first): {len(pos)}; "
          f"negative: {len(neg)}; zero: {len(zer)}")
    print(f"    mean d = {float(mean):.2f}, mean |d| = {float(mabs):.2f}, median |d| = {float(med):.1f}, "
          f"range {min(x[1] for x in d)} .. {max(x[1] for x in d)}")
    print("    negative d: " + ", ".join(f"{t} {v}" for t, v in neg))
    check("the four 'barely move' traits all move toward envious-first (d < 0)",
          all(dict(d)[t] < 0 for t in ("reliable", "important", "persistent", "serious")))
    check("these four are exactly the traits with d < 0", sorted(t for t, _ in neg)
          == sorted(["reliable", "important", "persistent", "serious"]))
    print(f"    one E->I subject = {100/24:.1f} points, one I->E subject = {100/34:.1f} points")
    print("    Fisher exact (two-sided) on reconstructed counts round(p*N):")
    sig = []
    for t in TABLE7:
        a = rnd(F(t[2] * N_IE, 100)); c = rnd(F(t[3] * N_EI, 100))
        pv = fisher_two_sided(a, N_IE - a, c, N_EI - c)
        flag = " *" if pv < 0.05 else ""
        if pv < 0.05:
            sig.append(t[0])
        print(f"      {t[0]:13s} {t[2]:3d} vs {t[3]:3d}  ~ {a:2d}/34 vs {c:2d}/24  p = {pv:.2g}{flag}")
    print(f"    p < .05 for {len(sig)} of 18: {', '.join(sig)}")
    check("restrained, good-looking, humorous, good-natured significant; "
          "serious, persistent, reliable, important not",
          all(x in sig for x in ("restrained", "good-looking", "humorous", "good-natured"))
          and not any(x in sig for x in ("serious", "persistent", "reliable", "important")))
    d7 = [(t[0], t[4] - t[5]) for t in TABLE7]
    print("    Exp VII (d = I->Ev minus Ev->I): positive "
          f"{sum(1 for x in d7 if x[1] > 0)}, negative {sum(1 for x in d7 if x[1] < 0)}, "
          f"zero {sum(1 for x in d7 if x[1] == 0)}; mean d = {float(F(sum(x[1] for x in d7), 18)):.2f}")
    print("      negative d: " + ", ".join(f"{t} {v}" for t, v in d7 if v < 0))

    print("(4) Attainability of each cell as round(100 k / N) for the nominal N")
    for col, n, idx in (("I->E", N_IE, 2), ("E->I", N_EI, 3), ("I->Ev", N_IEV, 4), ("Ev->I", N_EVI, 5)):
        bad = [(t[0], t[idx]) for t in TABLE7 if not attainable(t[idx], n)]
        print(f"    {col:6s} N={n}: {18-len(bad)}/18 attainable; not: "
              + (", ".join(f"{a} {b}" for a, b in bad) if bad else "none"))
    print("    (unattainable cells imply per-item non-response or a different base; "
          "Asch reports blanks for Exp I, p.263)")

    print("\nAll checks passed." if OK else "\nSOME CHECKS FAILED.")
    return OK


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
