"""
Hogarth & Einhorn (1992), Cognitive Psychology 24, 1-55 -- the counts and
percentages quoted in the text, checked against the tables they come from.

Source: the LaTeX transcription (T-p.N), with Table 1 read from the rendered page.

  (1) Table 1 (T-p.4) totals, and the head counts quoted on T-p.5:
      76 data points; 43 short-simple; primacy in 19 of 27 EoS and recency in
      16 of 16 SbS (short simple); 9 of 11 short complex recency; 14 of 16 long
      simple primacy.  Also the "19 of 27" and "all 16" repeated on T-p.12.
  (2) Table 3 (T-p.19) column totals (24 subjects per condition) and the
      percentages quoted on T-p.18 (61% and 75% recency in Experiment 3).
  (3) Other quoted rates: 19/285 = 6.7%, 7/96 = 7.3% (T-p.16); 35/47 = 75%,
      30/47 = 64% (T-p.16); 32/47 = 68% (T-p.20); 48/479 = 10% (T-p.21);
      42/57 = 74%, 36/57 = 63% (T-p.21); mean changes 15.1 and 11.0 (T-p.19).
"""
from fractions import Fraction as F

OK = True


def check(name, cond):
    global OK
    print(("  PASS  " if cond else "  FAIL  ") + name)
    if not cond:
        OK = False


def pct(n, d):
    return round(100 * n / d)


def main():
    print("(1) Table 1")
    # rows: outcome -> (Simple EoS, Simple SbS, Complex EoS, Complex SbS, Total)
    short = {"Primacy": (19, 0, 1, 0, 20), "Recency": (5, 16, 7, 2, 30), "No effect": (3, 0, 1, 0, 4)}
    long_ = {"Primacy": (12, 2, 2, 0, 16), "Recency": (2, 0, 1, 2, 5), "No effect": (0, 0, 1, 0, 1)}
    for tag, tab in (("short", short), ("long", long_)):
        for k, row in tab.items():
            check(f"{tag} {k}: row sums to printed total {row[4]}", sum(row[:4]) == row[4])
    total = sum(r[4] for r in short.values()) + sum(r[4] for r in long_.values())
    check(f"76 data points (T-p.5): {total}", total == 76)
    ss = sum(r[0] + r[1] for r in short.values())
    check(f"43 of 76 short-simple (T-p.5): {ss}", ss == 43)
    eos_ss = sum(r[0] for r in short.values())
    check(f"primacy in 19 of 27 short-simple EoS: {short['Primacy'][0]}/{eos_ss}",
          short["Primacy"][0] == 19 and eos_ss == 27)
    sbs_ss = sum(r[1] for r in short.values())
    check(f"recency in 16 of 16 short-simple SbS: {short['Recency'][1]}/{sbs_ss}",
          short["Recency"][1] == 16 and sbs_ss == 16)
    sc = sum(r[2] + r[3] for r in short.values())
    sc_rec = short["Recency"][2] + short["Recency"][3]
    check(f"9 of 11 short-complex recency: {sc_rec}/{sc}", sc_rec == 9 and sc == 11)
    ls = sum(r[0] + r[1] for r in long_.values())
    ls_pri = long_["Primacy"][0] + long_["Primacy"][1]
    check(f"14 of 16 long-simple primacy: {ls_pri}/{ls}", ls_pri == 14 and ls == 16)
    no_eff = short["No effect"][4] + long_["No effect"][4]
    print(f"    'No effect' data points: {no_eff} of 76")

    print("(2) Table 3 (surrogate individual-level order effects)")
    t3 = {  # experiment -> condition -> (primacy, recency, no effect, missing)
        1: {"SbS": (11, 10, 1, 2), "EoS": (12, 12, 0, 0)},
        2: {"SbS": (12, 11, 1, 0), "EoS": (11, 12, 1, 0)},
        3: {"SbS": (8, 14, 1, 1), "EoS": (6, 18, 0, 0)},
    }
    printed_tot = {1: (23, 22, 1, 2), 2: (23, 23, 2, 0), 3: (14, 32, 1, 1)}
    for e, d in t3.items():
        for c, row in d.items():
            check(f"Exp {e} {c}: 24 subjects", sum(row) == 24)
        tot = tuple(d["SbS"][i] + d["EoS"][i] for i in range(4))
        check(f"Exp {e} Total column {printed_tot[e]}", tot == printed_tot[e])
    s3, e3 = t3[3]["SbS"], t3[3]["EoS"]
    check(f"Exp 3 SbS recency 61% (T-p.18): {s3[1]}/{s3[0]+s3[1]+s3[2]} = {pct(s3[1], s3[0]+s3[1]+s3[2])}%",
          pct(s3[1], s3[0] + s3[1] + s3[2]) == 61)
    check(f"Exp 3 EoS recency 75%: {e3[1]}/{e3[0]+e3[1]} = {pct(e3[1], e3[0]+e3[1])}%",
          pct(e3[1], e3[0] + e3[1]) == 75)

    print("(3) Other quoted rates")
    check("19/285 = 6.7%", round(1000 * F(19, 285)) / 10 == 6.7)
    check("7/96 = 7.3%", round(1000 * F(7, 96)) / 10 == 7.3)
    # 35/47 = 74.47%: HE print 75%, which is a rounding slip (nearest integer 74).
    check("35/47 within 1 point of the printed 75%", abs(100 * F(35, 47) - 75) < 1)
    print(f"    note: 35/47 = {float(F(35, 47))*100:.2f}%, HE print 75% (nearest integer is 74)")
    check("30/47 = 64%", pct(30, 47) == 64)
    check("32/47 = 68%", pct(32, 47) == 68)
    check("48/479 = 10%", pct(48, 479) == 10)
    check("42/57 = 74%", pct(42, 57) == 74)
    check("36/57 = 63%", pct(36, 57) == 63)
    check("strong-first increase 78.1 - 63.0 = 15.1", F("78.1") - F("63.0") == F("15.1"))
    check("strong-second increase 83.7 - 72.7 = 11.0", F("83.7") - F("72.7") == F("11.0"))
    check("weak: (72.7 - 68.2) > (81.2 - 78.1)", F("72.7") - F("68.2") > F("81.2") - F("78.1"))
    check("Exp 2: (68.1 - 41.1) > (55.1 - 35.8)", F("68.1") - F("41.1") > F("55.1") - F("35.8"))
    check("Exp 2: (68.3 - 55.1) > (41.1 - 35.8)", F("68.3") - F("55.1") > F("41.1") - F("35.8"))
    check("Exp 3: (75.7 - 43.6) > (82.6 - 68.6)", F("75.7") - F("43.6") > F("82.6") - F("68.6"))
    check("Exp 3: (69.2 - 43.6) > (82.6 - 62.7)", F("69.2") - F("43.6") > F("82.6") - F("62.7"))


    print("\nAll checks passed." if OK else "\nSOME CHECKS FAILED.")
    return OK


if __name__ == "__main__":
    import sys
    sys.exit(0 if main() else 1)
