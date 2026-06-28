"""
Regenerate ONLY fig_composed_map.pdf, adding (i) shaded basins of attraction
and (ii) a cobweb trajectory, so the attractor-with-basin structure is visible
rather than only described. Parameters match the live figure / caption:
mu=0.05, delta=0.5, s_g=1, F=(1-c)^2; panel (a) trap (gamma_t=0.05),
panel (b) post-bifurcation (gamma_t=0.30). Same fixed-point markers and
annotations as before; nothing else is touched.
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import rcParams
from matplotlib.patches import Patch
from matplotlib.lines import Line2D

rcParams['font.family'] = 'serif'
rcParams['font.size'] = 10
rcParams['axes.labelsize'] = 11
rcParams['axes.titlesize'] = 11
rcParams['legend.fontsize'] = 8
rcParams['mathtext.fontset'] = 'cm'

MU, DELTA, S_G = 0.05, 0.5, 1.0
TRAP_FILL = '#e8a0a0'   # warm: trap basin
INT_FILL = '#9fc0e0'    # cool: integration basin
COBWEB = '#16557a'      # dark teal-blue cobweb

def F(c):
    return (1.0 - c) ** 2

def H(c, g):
    return MU + c * (1 - MU + g - S_G * DELTA * F(c)) - g * c ** 2

def fixed_points(g, n=400):
    cg = np.linspace(0.001, 0.999, n)
    gg = lambda c: H(c, g) - c
    out = []
    for i in range(len(cg) - 1):
        if gg(cg[i]) * gg(cg[i + 1]) < 0:
            a, b = cg[i], cg[i + 1]
            for _ in range(100):
                m = 0.5 * (a + b)
                if gg(a) * gg(m) < 0:
                    b = m
                else:
                    a = m
            out.append(0.5 * (a + b))
    out.append(1.0)
    return sorted(set(round(x, 6) for x in out))

def stable(c, g, eps=1e-5):
    return abs((H(c + eps, g) - H(c - eps, g)) / (2 * eps)) < 1

def cobweb(ax, c0, g, n=60, color=COBWEB, lw=0.9):
    """Staircase: vertical (c,c)->(c,H(c)), horizontal (c,H(c))->(H(c),H(c))."""
    c = c0
    ax.plot([c0], [c0], marker='o', ms=4.5, color=color, zorder=6)
    for _ in range(n):
        hc = H(c, g)
        ax.plot([c, c], [c, hc], color=color, lw=lw, alpha=0.95, zorder=4)
        ax.plot([c, hc], [hc, hc], color=color, lw=lw, alpha=0.95, zorder=4)
        if abs(hc - c) < 5e-4:
            break
        c = hc

def mark_fps(ax, fps, g, annotate=False):
    for fp in fps:
        st = stable(fp, g)
        ax.plot(fp, fp, marker=('o' if st else 's'), markersize=8,
                markerfacecolor=('black' if st else 'white'),
                markeredgecolor='black', markeredgewidth=1.3,
                linestyle='None', zorder=7)
    if annotate and len(fps) >= 3:
        lo, mid, hi = fps[0], fps[1], fps[2]
        ax.annotate(r'$\bar{c}_g^{\mathrm{low}}$', xy=(lo, lo),
                    xytext=(lo + 0.06, lo - 0.08), fontsize=9)
        ax.annotate(r'$\bar{c}_g^{\mathrm{mid}}$', xy=(mid, mid),
                    xytext=(mid - 0.02, mid + 0.06), fontsize=9)
        ax.annotate(r'$\bar{c}_g^{\mathrm{high}}$', xy=(hi, hi),
                    xytext=(hi - 0.17, hi - 0.08), fontsize=9)

fig, axes = plt.subplots(1, 2, figsize=(8.5, 3.7))
c = np.linspace(0, 1, 400)
C0 = 0.55  # same starting standing in both panels

# ---- Panel (a): trap regime ----
ga = 0.05
ax = axes[0]
fps_a = fixed_points(ga)
c_mid = fps_a[1]
ax.axvspan(0, c_mid, color=TRAP_FILL, alpha=0.30, lw=0, zorder=0)
ax.axvspan(c_mid, 1, color=INT_FILL, alpha=0.30, lw=0, zorder=0)
ax.axvline(c_mid, color='0.35', ls=':', lw=1.0, zorder=1)
ax.plot(c, H(c, ga), 'k-', lw=1.6, zorder=3)
ax.plot(c, c, 'k--', lw=0.9, alpha=0.6, zorder=2)
cobweb(ax, C0, ga)
mark_fps(ax, fps_a, ga, annotate=True)
ax.set_xlim(0, 1); ax.set_ylim(0, 1)
ax.set_xlabel(r'$\bar{c}_g^t$'); ax.set_ylabel(r'$\bar{c}_g^{t+1}$')
ax.set_title(r'(a) Trap regime ($\tilde{\gamma} = 0.05$)')
ax.set_aspect('equal'); ax.grid(True, alpha=0.25)

# ---- Panel (b): post-bifurcation ----
gb = 0.30
ax = axes[1]
fps_b = fixed_points(gb)
ax.axvspan(0, 1, color=INT_FILL, alpha=0.30, lw=0, zorder=0)
ax.plot(c, H(c, gb), 'k-', lw=1.6, zorder=3)
ax.plot(c, c, 'k--', lw=0.9, alpha=0.6, zorder=2)
cobweb(ax, C0, gb)
mark_fps(ax, fps_b, gb)
ax.set_xlim(0, 1); ax.set_ylim(0, 1)
ax.set_xlabel(r'$\bar{c}_g^t$'); ax.set_ylabel(r'$\bar{c}_g^{t+1}$')
ax.set_title(r'(b) Post-bifurcation ($\tilde{\gamma} = 0.30$)')
ax.set_aspect('equal'); ax.grid(True, alpha=0.25)

legend_elems = [
    Line2D([0], [0], color='k', lw=1.6, label=r'$\mathcal{H}(\bar{c}_g)$'),
    Line2D([0], [0], color='k', lw=0.9, ls='--', alpha=0.6, label='45-degree line'),
    Line2D([0], [0], color=COBWEB, lw=0.9, marker='o', ms=4.5,
           label=r'trajectory from $\bar{c}_g^0 = 0.55$'),
    Line2D([0], [0], marker='o', color='none', markerfacecolor='black',
           markeredgecolor='black', ms=8, label='attractor'),
    Line2D([0], [0], marker='s', color='none', markerfacecolor='white',
           markeredgecolor='black', ms=8, label='repeller (tipping point)'),
    Patch(facecolor=TRAP_FILL, alpha=0.30, label='basin of low attractor'),
    Patch(facecolor=INT_FILL, alpha=0.30, label='basin of integration'),
]
fig.legend(handles=legend_elems, loc='lower center', ncol=4,
           frameon=True, framealpha=0.95, bbox_to_anchor=(0.5, -0.10))

plt.tight_layout()
plt.savefig('/home/claude/work/figures/fig_composed_map.pdf',
            bbox_inches='tight', dpi=300)
plt.close()
print('trap fps:', [round(x, 3) for x in fps_a], '| mid =', round(c_mid, 3))
print('post fps:', [round(x, 3) for x in fps_b])
print('saved fig_composed_map.pdf')
