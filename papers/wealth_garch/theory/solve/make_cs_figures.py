"""Draws the figures of the comparative-statics section of wealth_garch_theory.tex from
results_cs.json (run_cs.py) and results_q7.json (run_q7_safety.py), into
theory/figures/cs_*.pdf, and writes results_cs_frontier.md, the comparison of the two
levers at equal safety on both value axes.

    python3 make_cs_figures.py
"""
import json
import os

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
# CS_OUT redirects the figures and results_cs_frontier.md, for the check that redraws them
# (MS-11 of checks/verify_manuscript.py).
OUT = os.environ.get("CS_OUT")
FIG = OUT or os.path.join(os.path.dirname(HERE), "figures")
MD = OUT or HERE
META = {"CreationDate": None, "ModDate": None}
plt.rcParams.update({"font.family": "serif", "font.size": 9, "axes.spines.top": False, "axes.spines.right": False,
                     "figure.dpi": 150, "lines.linewidth": 1.4, "lines.markersize": 4})
COLS = ["#1b4f72", "#b9770e", "#7b241c", "#196f3d"]
NAMES = {"y_star": "dividend barrier $y^*$", "x_L": "recapitalisation trigger $x_L$", "y_post": "injection target $y_{post}$"}


def load():
    cs = json.load(open(os.path.join(HERE, "results_cs.json")))
    q7 = json.load(open(os.path.join(HERE, "results_q7.json")))
    return cs, q7


def series(cs, tag, xkey, group):
    """Rows of block tag, grouped by the value of group, sorted by xkey."""
    out = {}
    for t, kw, o in cs["thresholds"]:
        if t != tag:
            continue
        p = dict(cs["base"], **kw)
        out.setdefault(p[group], []).append((p[xkey], o))
    return {g: sorted(v, key=lambda a: a[0]) for g, v in sorted(out.items())}


def three_panels(groups, xlabel, glabel, fname, xlog=False):
    fig, ax = plt.subplots(1, 3, figsize=(7.0, 2.6))
    for j, key in enumerate(("y_star", "x_L", "y_post")):
        for i, (g, rows) in enumerate(groups.items()):
            xs = np.array([r[0] for r in rows])
            ys = np.array([r[1][key] for r in rows])
            inner = np.array([r[1]["interior"] for r in rows])
            ax[j].plot(xs, ys, color=COLS[i], label=f"{glabel} = {g:g}")
            ax[j].plot(xs[inner], ys[inner], "o", color=COLS[i])
            ax[j].plot(xs[~inner], ys[~inner], "o", mfc="white", color=COLS[i])
        if key == "x_L":
            ax[j].axhline(1.0, color="0.6", lw=0.6, ls=":")
        ax[j].set_title(NAMES[key], fontsize=9)
        ax[j].set_xlabel(xlabel)
        if xlog:
            ax[j].set_xscale("log")
    h, l = ax[0].get_legend_handles_labels()
    fig.legend(h, l, loc="lower center", ncol=len(l), frameon=False, fontsize=7)
    fig.tight_layout(rect=(0, 0.08, 1, 1))
    fig.savefig(os.path.join(FIG, fname), metadata=META)
    plt.close(fig)


def fig_floor(cs):
    groups = series(cs, "C", "a1", "lam")
    dets = {}
    for t, kw, o in cs["determinants"]:
        if t == "C" and "det" in o:
            p = dict(cs["base"], **kw)
            dets.setdefault(p["lam"], []).append((p["a1"], o["det"]))
    fig, ax = plt.subplots(1, 3, figsize=(7.0, 2.6))
    for i, (g, rows) in enumerate(groups.items()):
        xs = [r[0] for r in rows]
        ax[0].plot(xs, [r[1]["y_star"] for r in rows], "o-", color=COLS[i], label=f"$\\lambda_S$ = {g:g}")
        ax[1].plot(xs, [r[1]["x_L"] for r in rows], "o-", color=COLS[i])
        d = sorted(dets.get(g, []))
        ax[2].plot([a for a, _ in d], [v for _, v in d], "o-", color=COLS[i])
    ax[0].set_title(NAMES["y_star"], fontsize=9)
    ax[1].set_title(NAMES["x_L"], fontsize=9)
    ax[2].set_title("determinant of M3", fontsize=9)
    ax[2].axhline(0, color="0.6", lw=0.6, ls=":")
    for a in ax:
        a.set_xlabel("solvency floor $a_1$")
    h, l = ax[0].get_legend_handles_labels()
    fig.legend(h, l, loc="lower center", ncol=len(l), frameon=False, fontsize=7)
    fig.tight_layout(rect=(0, 0.08, 1, 1))
    fig.savefig(os.path.join(FIG, "cs_floor.pdf"), metadata=META)
    plt.close(fig)


def fig_risk(cs):
    fig, ax = plt.subplots(1, 2, figsize=(7.0, 2.6))
    for i, (lam, o) in enumerate(sorted(cs["profiles"].items(), key=lambda a: float(a[0]))):
        x = np.array(o["x"])
        r = np.array(o["risk"])
        for a in ax:
            a.plot(x, r, color=COLS[i], label=f"$\\lambda_S$ = {float(lam):g}")
        ax[0].plot([o["x_L"]], [np.interp(o["x_L"], x, r)], "o", color=COLS[i])
    ax[0].set_xlim(1.0, 1.2)
    ax[1].set_ylim(-1.0, 1.2)
    for a in ax:
        a.axhline(0, color="0.6", lw=0.6, ls=":")
        a.axvline(cs["base"]["R"], color="0.6", lw=0.6, ls="--")
        a.set_xlabel("assets-to-liabilities ratio $x$")
    ax[0].set_title("$-V''/V'$ near the trigger", fontsize=9)
    ax[1].set_title("$-V''/V'$ over the inaction region", fontsize=9)
    h, l = ax[0].get_legend_handles_labels()
    fig.legend(h, l, loc="lower center", ncol=len(l), frameon=False, fontsize=7)
    fig.tight_layout(rect=(0, 0.08, 1, 1))
    fig.savefig(os.path.join(FIG, "cs_risk.pdf"), metadata=META)
    plt.close(fig)


def fig_frontier(q7):
    res = {tuple(k): v for k, v in q7["points"]}
    lams = sorted({k[0] for k in res if k[1] == 0.05 and k[2] == 0.045})
    a1s = sorted({k[2] for k in res if k[0] == 1.0 and k[1] == 0.05})
    fig, ax = plt.subplots(1, 2, figsize=(7.0, 2.8))
    for j, (val, lab) in enumerate((("C", "cash value $C(1.2)$"), ("v12", "value function $V(1.2)$"))):
        tl = [res[(l, 0.05, 0.045)]["time_deficit"] for l in lams]
        ax[j].plot(tl, [res[(l, 0.05, 0.045)][val] for l in lams], "o-", color=COLS[0], label="asymmetry parameter, $\\lambda_S$ 1 to 3")
        for l, t in zip(lams, tl):
            if l in (1.0, 1.5, 3.0):
                ax[j].annotate(f"{l:g}", (t, res[(l, 0.05, 0.045)][val]), textcoords="offset points", xytext=(3, 3), fontsize=7)
        ta = [res[(1.0, 0.05, a)]["time_deficit"] for a in a1s]
        ax[j].plot(ta, [res[(1.0, 0.05, a)][val] for a in a1s], "s-", color=COLS[1], label="solvency floor, $a_1$ 0.045 to 0.12")
        for a, t in zip(a1s, ta):
            ax[j].annotate(f"{a:g}", (t, res[(1.0, 0.05, a)][val]), textcoords="offset points", xytext=(-4, -10), ha="right", fontsize=7)
        ax[j].set_xlabel("expected years in deficit, five years from $x=1.2$")
        ax[j].set_title(lab, fontsize=9)
    h, l = ax[0].get_legend_handles_labels()
    fig.legend(h, l, loc="lower center", ncol=2, frameon=False, fontsize=7)
    fig.tight_layout(rect=(0, 0.08, 1, 1))
    fig.savefig(os.path.join(FIG, "cs_frontier.pdf"), metadata=META)
    plt.close(fig)
    # the two levers at equal safety, on both value axes
    td = np.array([res[(l, 0.05, 0.045)]["time_deficit"] for l in lams])
    o = np.argsort(td)
    L = ["# Results: the two levers at equal safety (generated)", "",
         "Generated by `make_cs_figures.py` from `results_q7.json`. For each point of the solvency-floor path "
         "(lambda_S = 1, K = 0.05), the asymmetry parameter on the path a1 = 0.045, K = 0.05 with the same expected "
         "time in deficit over five years, by linear interpolation, and the cash value C(1.2) and value function "
         "v(1.2) of each.", "",
         "| a1 | time in deficit | lambda_S giving it | C by the floor | C by lambda_S | V by the floor | V by lambda_S |",
         "|---|---|---|---|---|---|---|"]
    for a in a1s:
        r = res[(1.0, 0.05, a)]
        t = r["time_deficit"]
        if td.min() <= t <= td.max():
            f = lambda key: float(np.interp(t, td[o], np.array([res[(l, 0.05, 0.045)][key] for l in lams])[o]))
            lam_eq = float(np.interp(t, td[o], np.array(lams)[o]))
            L.append(f"| {a} | {t:.3f} | {lam_eq:.3f} | {r['C']:.4f} | {f('C'):.4f} | {r['v12']:.4f} | {f('v12'):.4f} |")
        else:
            L.append(f"| {a} | {t:.3f} | outside the lambda_S path | {r['C']:.4f} | | {r['v12']:.4f} | |")
    open(os.path.join(MD, "results_cs_frontier.md"), "w").write("\n".join(L) + "\n")


def main():
    cs, q7 = load()
    three_panels(series(cs, "A", "lam", "K"), "asymmetry parameter $\\lambda_S$", "$K$", "cs_lambda.pdf")
    three_panels(series(cs, "B", "K", "lam"), "fixed issuance cost $K$", "$\\lambda_S$", "cs_K.pdf", xlog=True)
    fig_floor(cs)
    fig_risk(cs)
    fig_frontier(q7)
    print("figures written to", FIG)


if __name__ == "__main__":
    main()
