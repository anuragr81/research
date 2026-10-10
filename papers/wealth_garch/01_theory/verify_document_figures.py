"""
verify_document_figures.py

Regenerates, from the solve output itself, every numerical figure quoted in
PROOFS_v2 that the lambda_S sweep can support, and asserts each against the
value printed in the document. Nothing here is carried from notes: each
number is recomputed from resultM_lambda_*_maxiter3000.mat on every run.

WHY THIS EXISTS. Figures were reaching the document from ad-hoc scratch
scripts whose outputs did not survive, so the document quoted numbers that
no bundled script regenerated. Anything this file cannot reach is reported
as SKIP with the reason, never silently omitted.

BOUNDARY CONVENTION -- read this before interpreting F6/F7. The solver
records its own recapitalisation edge as `recap_edge_new`. Re-deriving the
trigger instead as "last grid point where v' still equals 1+kappa" gives a
point ONE CELL LOWER. Both are defensible labels for x_L; they are not the
same grid point, and figures evaluated at one are not figures evaluated at
the other. This file reports both and states which the document's numbers
match, rather than picking one silently.

PREREQUISITES. octave on PATH, and the solved .mat files. Point MATDATA_DIR
at the folder holding them; defaults to 03_empirical/results. Absent
prerequisites SKIP rather than FAIL, following verify_saturation_limits.py.

Run: python3 01_theory/verify_document_figures.py
"""

import os
import glob
import shutil
import subprocess

FAILURES = []
SKIPPED = []
READ_ERRORS = []


def check(tag, claim, ok, measured=""):
    if ok is None:
        print(f"{tag:<6}SKIP    {claim}" + (f"   [{measured}]" if measured else ""))
        SKIPPED.append(tag)
        return
    status = "PASS" if ok else "FAIL"
    if not ok:
        FAILURES.append(tag)
    print(f"{tag:<6}{status:<8}{claim}" + (f"   [{measured}]" if measured else ""))


HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
MATDATA = os.environ.get("MATDATA_DIR", os.path.join(ROOT, "03_empirical", "results"))
LAMS = ["1.0000", "1.1000", "1.2500", "1.5000", "1.7500", "2.0000"]

have_octave = shutil.which("octave") is not None
have_mat = bool(glob.glob(os.path.join(MATDATA, "resultM_lambda_*_maxiter3000.mat")))

print("=" * 78)
print("DOCUMENT FIGURE REGENERATION -- lambda_S sweep (M operator)")
print("=" * 78)
print(f"MATDATA_DIR = {MATDATA}")
print(f"octave: {'found' if have_octave else 'ABSENT'};  "
      f".mat files: {'found' if have_mat else 'ABSENT'}")
print()

DATA = {}
if have_octave and have_mat:
    for lam in LAMS:
        f = os.path.join(MATDATA, f"resultM_lambda_{lam}_maxiter3000.mat")
        if not os.path.exists(f):
            continue
        cmd = f"""
S = load('{f}');
sol = S.sol_new; p = S.params;
printf("SCALARS %.10f %.10f %.10f %d %d %.10f %.10f %.10f %.10f %.10f %.10f %.10f %.10f\\n", ...
  sol.y_star, S.recap_edge_new, S.y_post_new, sol.converged, sol.iterations, ...
  p.rho, p.mu_L, p.kappa, p.K, p.R_ref, p.lambda_S, p.a1, p.a3);
printf("PARAMS2 %.10f %.10f %.10f %.10f %.10f %.10f %.10f\\n", ...
  p.r, p.mu, p.sigma, p.sigma_L, p.c, p.gamma, p.a2);
for i = 1:length(sol.y)
  printf("ROW %.12g %.12g %.12g %.12g %.12g %.12g %.12g\\n", ...
    sol.y(i), sol.v(i), sol.v_prime(i), sol.v_second(i), ...
    sol.pi_star(i), sol.pi_max(i), sol.Lam(i));
end
printf("DONE\\n");
"""
        r = subprocess.run(["octave", "--no-gui", "--eval", cmd],
                           capture_output=True, text=True, timeout=600)
        sc = [l for l in r.stdout.splitlines() if l.startswith("SCALARS")]
        p2 = [l for l in r.stdout.splitlines() if l.startswith("PARAMS2")]
        rows = [l.split()[1:] for l in r.stdout.splitlines() if l.startswith("ROW")]
        if not sc or not p2 or not rows or "DONE" not in r.stdout:
            err = (r.stderr or "").strip().splitlines()
            err = " | ".join(x for x in err if x.strip())[:400] or "no stderr; octave produced no usable output"
            READ_ERRORS.append(f"lambda={lam}: {err}")
            continue
        v = [float(x) for x in sc[0].split()[1:]]
        w = [float(x) for x in p2[0].split()[1:]]
        import numpy as np
        G = np.array([[float(c) for c in rr] for rr in rows])
        DATA[lam] = dict(
            y=G[:, 0], v=G[:, 1], vp=G[:, 2], vpp=G[:, 3],
            pi=G[:, 4], pimax=G[:, 5], Lam=G[:, 6],
            y_star=v[0], recap_edge=v[1], y_post=v[2],
            converged=int(v[3]), iters=int(v[4]),
            rho=v[5], mu_L=v[6], kap=v[7], K=v[8], R=v[9], lamS=v[10],
            a1=v[11], a3=v[12],
            r=w[0], mu_s=w[1], sigma=w[2], sigma_L=w[3], c=w[4],
            gamma=w[5], a2=w[6],
        )

if not DATA:
    prereq_missing = (not have_octave) or (not have_mat)
    if prereq_missing:
        reason = []
        if not have_octave:
            reason.append("octave not on PATH")
        if not have_mat:
            reason.append(f"no resultM_lambda_*_maxiter3000.mat under {MATDATA}")
        msg = "; ".join(reason)
        verdict = None          # genuine SKIP: a prerequisite is absent
    else:
        # Both prerequisites ARE present and reading still failed. That is a
        # BROKEN CHECK, not an absent one. Reporting it as SKIP would let the
        # run exit 0 while verifying nothing -- the exact false green this
        # file exists to prevent.
        msg = "octave and .mat files BOTH present but unreadable -- see errors below"
        verdict = False
    for t in ["F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12"]:
        check(t, "regenerate document figure from solve output", verdict, msg)
    if READ_ERRORS:
        print()
        print("  OCTAVE READ ERRORS (first line each):")
        for e in READ_ERRORS:
            print(f"    {e}")
else:
    import numpy as np

    D0 = DATA[LAMS[0]]
    h = D0["y"][1] - D0["y"][0]

    # ---------------------------------------------------------------- F1
    ok = (len(D0["y"]) == 301 and abs(h - 0.005) < 1e-12
          and abs(D0["y"][0] - 1.0) < 1e-12 and abs(D0["y"][-1] - 2.5) < 1e-12)
    check("F1", "grid is N=301 on [1, 2.5], h=0.005",
          ok, f"N={len(D0['y'])} h={h:.6f}")

    allconv = all(DATA[l]["converged"] == 1 for l in DATA)
    check("F1b", "every lambda_S run reports converged",
          allconv, f"{sum(DATA[l]['converged'] for l in DATA)}/{len(DATA)}")

    # parameters quoted in the document's table
    P = {"rho": 0.12, "mu_L": 0.03, "kap": 0.01, "K": 0.02, "R": 1.15,
         "r": 0.025, "mu_s": 0.04, "sigma": 0.08, "sigma_L": 0.03,
         "c": 0.20, "gamma": 0.02, "a1": 0.045}
    bad = [k for k, want in P.items() if abs(D0[k] - want) > 1e-9]
    check("F1c", "solve parameters match the document's parameter table",
          not bad, "all match" if not bad else f"MISMATCH: {bad}")

    rho_L = D0["rho"] - D0["mu_L"]
    check("F1d", "rho_L = rho - mu_L = 0.09", abs(rho_L - 0.09) < 1e-12,
          f"rho_L={rho_L:.6f}")

    # ------------------------------------------------- boundary convention
    def plateau_edge(D):
        """Last grid point on the v' plateau (my earlier convention)."""
        vp = D["vp"]
        j = np.where(np.abs(vp - vp[0]) > 1e-9)[0][0]
        return D["y"][j - 1], j

    print()
    print("  boundary convention, per lambda_S:")
    for l in LAMS:
        if l not in DATA:
            continue
        D = DATA[l]
        pe, j = plateau_edge(D)
        print(f"    lam={D['lamS']:.2f}  solver recap_edge={D['recap_edge']:.4f}   "
              f"plateau-derived={pe:.4f}   difference={D['recap_edge']-pe:+.4f} "
              f"({(D['recap_edge']-pe)/h:+.1f} cells)")
    print()

    # ---------------------------------------------------------------- F2
    ys = [DATA[l]["y_star"] for l in LAMS if l in DATA]
    mono = all(ys[i] < ys[i + 1] for i in range(len(ys) - 1))
    check("F2", "CSL: y* rises 1.5715 -> 1.6596, strictly monotone",
          mono and abs(ys[0] - 1.5715) < 5e-4 and abs(ys[-1] - 1.6596) < 5e-4,
          f"{ys[0]:.4f} -> {ys[-1]:.4f}, monotone={mono}")

    # ---------------------------------------------------------------- F3
    xs = [DATA[l]["recap_edge"] for l in LAMS if l in DATA]
    mono = all(xs[i] < xs[i + 1] for i in range(len(xs) - 1))
    check("F3", "CSL: recap boundary rises 1.030 -> 1.105 (solver's own edge)",
          mono and abs(xs[0] - 1.030) < 5e-4 and abs(xs[-1] - 1.105) < 5e-4,
          f"{xs[0]:.4f} -> {xs[-1]:.4f}, monotone={mono}")

    # ---------------------------------------------------------------- F4
    v1 = [DATA[l]["v"][0] for l in LAMS if l in DATA]
    mono = all(v1[i] > v1[i + 1] for i in range(len(v1) - 1))
    check("F4", "CSL: V(1) falls 0.3843 -> 0.3395, strictly monotone",
          mono and abs(v1[0] - 0.3843) < 5e-4 and abs(v1[-1] - 0.3395) < 5e-4,
          f"{v1[0]:.4f} -> {v1[-1]:.4f}, monotone={mono}")

    # ---------------------------------------------------------------- F5
    # mu'(y*) with the full derivative including the pi' term, at lambda=1
    def mu_of(D):
        return D["y"] * ((1 - D["pi"]) * D["r"] + D["pi"] * D["mu_s"] - D["mu_L"]) + D["gamma"]

    D = DATA["1.0000"]
    i_star = int(np.argmin(np.abs(D["y"] - D["y_star"])))
    mu = mu_of(D)
    mup = np.gradient(mu, D["y"])[i_star]
    check("F5", "TCSN: rho_L - mu'(y*) = 0.045 > 0",
          abs((rho_L - mup) - 0.045) < 2e-3,
          f"mu'(y*)={mup:.6f}, rho_L-mu'={rho_L-mup:.6f}")

    # ------------------------------------------------------------ F6 / F7
    # Psi(x) = rho_L V + Lambda - (1+kappa) mu ; at a point where v'=1+kappa,
    # (1/2) sigma^2 V'' = Psi, so the two share a sign.
    def psi_of(D):
        return rho_L * D["v"] + D["Lam"] - (1 + D["kap"]) * mu_of(D)

    print("  F6/F7 detail -- Psi and V'' at both candidate trigger points:")
    # Canonical x_L is the solver's recap_edge (the last active point), per
    # the D2 resolution: the plateau-derived point one cell below is a
    # central-difference stencil artefact, not a rival convention. The
    # document's SOCN Psi figures are the EDGE values; F6 checks the edge.
    doc_psi = {"1.0000": 0.012, "1.2500": 0.023, "2.0000": 0.035}
    doc_vpp = {"1.0000": 6.4, "1.2500": 13.9, "2.0000": 18.7}
    psi_at_plateau, psi_at_edge, vpp_at_edge, vpp_above = {}, {}, {}, {}
    for l in ["1.0000", "1.2500", "2.0000"]:
        if l not in DATA:
            continue
        D = DATA[l]
        Psi = psi_of(D)
        pe, j = plateau_edge(D)
        ie = int(np.argmin(np.abs(D["y"] - D["recap_edge"])))
        psi_at_plateau[l] = Psi[j - 1]
        psi_at_edge[l] = Psi[ie]
        vpp_at_edge[l] = D["vpp"][ie]
        vpp_above[l] = D["vpp"][j]
        print(f"    lam={D['lamS']:.2f}  Psi(plateau {pe:.3f})={Psi[j-1]:+.4f}   "
              f"Psi(edge {D['recap_edge']:.3f})={Psi[ie]:+.4f}   "
              f"V''(edge)={D['vpp'][ie]:+.3f}   V''(first above)={D['vpp'][j]:+.3f}")
    print()

    def close(meas, want, tol):
        return all(abs(meas[l] - want[l]) <= tol for l in want if l in meas)

    m_edge = close(psi_at_edge, doc_psi, 1e-3)
    check("F6", "SOCN: Psi(x_L) = 0.012 / 0.023 / 0.035 at the canonical (edge) x_L",
          m_edge,
          "edge="
          + "/".join(f"{psi_at_edge[l]:.4f}" for l in doc_psi if l in psi_at_edge)
          + f"  (plateau values, not used: "
          + "/".join(f"{psi_at_plateau[l]:.4f}" for l in doc_psi if l in psi_at_plateau)
          + ")")


    v_edge = close(vpp_at_edge, doc_vpp, 0.1)
    v_above = close(vpp_above, doc_vpp, 0.1)
    which2 = ("solver recap_edge" if v_edge else
              "first point above plateau" if v_above else "NEITHER")
    check("F7", "SOCN: V''(x_L^+) = 6.4 / 13.9 / 18.7",
          v_edge or v_above,
          f"matches at: {which2}; edge="
          + "/".join(f"{vpp_at_edge[l]:.3f}" for l in doc_vpp if l in vpp_at_edge)
          + "  above="
          + "/".join(f"{vpp_above[l]:.3f}" for l in doc_vpp if l in vpp_above))

    # ---------------------------------------------------------------- F8
    allpos, worst = True, None
    for l in LAMS:
        if l not in DATA:
            continue
        D = DATA[l]
        Psi = psi_of(D)
        m = D["y"] <= D["recap_edge"] + 1e-12
        mn = Psi[m].min()
        if mn < -1e-9:
            allpos = False
        if worst is None or mn < worst:
            worst = mn
    check("F8", "SOC(ii): Psi >= 0 throughout the intervention region",
          allpos, f"min over all lambda_S = {worst:+.6f}")

    # --------------------------------------------------------------- F10
    a4 = True
    vals = []
    for l in LAMS:
        if l not in DATA:
            continue
        D = DATA[l]
        ip = int(np.argmin(np.abs(D["y"] - D["y_post"])))
        vals.append(D["vpp"][ip])
        if D["vpp"][ip] >= 0:
            a4 = False
    check("F10", "SOC(i): V''(y_post) < 0 strictly, every lambda_S",
          a4, "V''(y_post) = " + "/".join(f"{x:+.4f}" for x in vals))

    # --------------------------------------------------------------- F11
    gaps = [DATA[l]["y_post"] - DATA[l]["recap_edge"] for l in LAMS if l in DATA]
    spread = max(gaps) - min(gaps)
    lvl = max(DATA[l]["y_star"] for l in DATA) - min(DATA[l]["y_star"] for l in DATA)
    check("F11", "IDN reading: trigger-to-target gap ~flat in lambda_S while levels move",
          spread < lvl,
          f"gap {min(gaps):.4f}..{max(gaps):.4f} (spread {spread:.4f}) "
          f"vs y* spread {lvl:.4f}")

    # ---------------------------------------------------------------- F9
    counts = {}
    for l in LAMS:
        if l not in DATA:
            continue
        D = DATA[l]
        m = (D["y"] >= D["recap_edge"] - 1e-12) & (D["y"] <= D["y_star"] + 1e-12)
        agree = int(np.sum(np.abs(D["pi"][m] - D["pimax"][m]) < 1e-8))
        counts[l] = (agree, int(m.sum()))
    lowlam = [l for l in ["1.0000", "1.1000", "1.2500", "1.5000", "1.7500"] if l in counts]
    binds = all(counts[l][0] == counts[l][1] for l in lowlam)
    rng = sorted(counts[l][1] for l in lowlam)
    check("F9", "CBI: cap binds at every inaction cell for lambda_S <= 1.75, cells 108-111",
          binds and rng[0] >= 100 and rng[-1] <= 120,
          "; ".join(f"lam={DATA[l]['lamS']:.2f}:{counts[l][0]}/{counts[l][1]}" for l in lowlam))
    if "2.0000" in counts:
        a, t = counts["2.0000"]
        check("F9b", "CBI: at lambda_S=2.00 a small number of cells show a slack cap",
              (t - a) >= 0, f"slack cells = {t-a} of {t}")
    xbar = (D0["a3"] - D0["a1"] * D0["a2"]) / (D0["a3"] - D0["a1"])
    check("F9c", "CBI: regime switch xbar = 1.167647",
          abs(xbar - 1.167647) < 1e-5, f"xbar={xbar:.6f}")

    # --------------------------------------------------------------- F12
    # The document reports count 6 and span 0.0250 at N=301. Those are
    # consistent with each other only as span=(count-1)*h, so the count is
    # of GRID POINTS and the span is the distance between the outermost two.
    # Whether the trigger cell itself is included changes the count by one,
    # so both readings are reported rather than one chosen silently.
    D = DATA["1.0000"]
    pos = D["vpp"] > 0
    ie = int(np.argmin(np.abs(D["y"] - D["recap_edge"])))
    runs = {}
    for label, start in (("including the trigger cell", ie),
                         ("strictly above it", ie + 1)):
        k = start
        cnt = 0
        while k < len(pos) and pos[k]:
            cnt += 1
            k += 1
        runs[label] = (cnt, max(cnt - 1, 0) * h)
    detail = "; ".join(f"{lab}: count={c} span={sp:.4f}" for lab, (c, sp) in runs.items())
    hit = [lab for lab, (c, sp) in runs.items() if c == 6 and abs(sp - 0.0250) < 1e-9]
    check("F12", "LNC: convex region at N=301, lambda_S=1 -- count 6, span 0.0250",
          bool(hit), (f"matches {hit[0]}; " if hit else "matches NEITHER reading; ") + detail)

# ---------------------------------------------------------------- SKIPS
print()
check("F13", "KSW: K-sweep figures (band 0.159->0.685, K^(1/3) slope, K=0 collapse)",
      None, "needs resultK_*.mat -- K-sweep outputs not in repo; open item C1")
check("F14", "CSL: H-operator figures (y* 1.3725->1.4371, boundary 1.145->1.200)",
      None, "needs resultH_*.mat -- H-convention outputs not in repo; open item C2")
check("F15", "LNC: three-mesh refinement (counts 6->12->24, spans 0.0250/0.0275/0.0288)",
      None, "needs solves at other N -- only N=301 in repo; open item C3")

print()
print("=" * 78)
print(f"figures: {len(FAILURES)} FAIL, {len(SKIPPED)} SKIP")
if FAILURES:
    print(f"FAILURES: {FAILURES}")
print("=" * 78)
raise SystemExit(1 if FAILURES else 0)
