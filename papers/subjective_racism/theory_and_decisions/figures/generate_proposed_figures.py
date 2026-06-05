"""
Generate two additional figures for the discrimination paper:

Figure A (proposed): Cobweb diagram — composed map with iteration trajectories
  overlaid, showing convergence from multiple initial conditions to the
  appropriate attractor. Trap regime only (where dynamics are interesting).

Figure B (proposed): Trajectory time-series — c̄_g^t vs generation t for the
  same initial conditions, showing convergence rates and the closure trap as a
  path that stays low forever.

Style: matches generate_figures.py (serif, same parameters).
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import rcParams

rcParams['font.family'] = 'serif'
rcParams['font.size'] = 10
rcParams['axes.labelsize'] = 11
rcParams['axes.titlesize'] = 11
rcParams['legend.fontsize'] = 9
rcParams['mathtext.fontset'] = 'cm'

MU = 0.05
DELTA = 0.5
S_G = 1.0
GAMMA_TRAP = 0.05  # same as Figure 1 panel (a)

def F(c):
    return (1.0 - c) ** 2

def H(c, gamma_tilde):
    return MU + c * (1 - MU + gamma_tilde - S_G * DELTA * F(c)) - gamma_tilde * c ** 2

def fixed_points(gamma_tilde, n_seeds=400):
    c_grid = np.linspace(0.001, 0.999, n_seeds)
    g = lambda c: H(c, gamma_tilde) - c
    fps = []
    for i in range(len(c_grid) - 1):
        if g(c_grid[i]) * g(c_grid[i + 1]) < 0:
            a, b = c_grid[i], c_grid[i + 1]
            for _ in range(100):
                m = 0.5 * (a + b)
                if g(a) * g(m) < 0:
                    b = m
                else:
                    a = m
            fps.append(0.5 * (a + b))
    fps.append(1.0)
    return sorted(set(round(f, 6) for f in fps))

def stability(c_star, gamma_tilde, eps=1e-5):
    h_prime = (H(c_star + eps, gamma_tilde) - H(c_star - eps, gamma_tilde)) / (2 * eps)
    return abs(h_prime) < 1

def cobweb_path(x0, gamma_tilde, n_iter=80):
    """Return list of (x, y) points tracing the cobweb iteration."""
    pts = [(x0, 0.0)]
    x = x0
    for _ in range(n_iter):
        y = H(x, gamma_tilde)
        pts.append((x, y))   # vertical: up to map
        pts.append((y, y))   # horizontal: across to 45° line
        if abs(y - x) < 1e-6:
            break
        x = y
    return pts

# Find fixed points in trap regime
fps_trap = fixed_points(GAMMA_TRAP)
print(f"Trap-regime fixed points: {[round(f,4) for f in fps_trap]}")
fp_low, fp_mid, fp_high = fps_trap[0], fps_trap[1], fps_trap[2]
print(f"  low (stable)   = {fp_low:.4f}")
print(f"  mid (unstable) = {fp_mid:.4f}")
print(f"  high (stable)  = {fp_high:.4f}")

# Initial conditions chosen to demonstrate basins
INITIAL_CONDITIONS = [
    (0.05, '#1F4E79'),  # below low fixed point → converges up to fp_low
    (0.30, '#1F4E79'),  # between low and mid → converges down to fp_low
    (fp_mid + 0.05, '#A0522D'),  # just above tipping → converges to high
    (0.85, '#A0522D'),  # well above → converges to high
]

# ============================================================================
# Figure A: Cobweb diagram with trajectories overlaid
# ============================================================================

fig, ax = plt.subplots(figsize=(5.5, 5.0))

c = np.linspace(0, 1, 400)
H_c = H(c, GAMMA_TRAP)

# Map curve
ax.plot(c, H_c, 'k-', linewidth=1.8, zorder=3)
# 45-degree line
ax.plot(c, c, 'k--', linewidth=0.9, alpha=0.5, zorder=2)

# Cobweb trajectories
for x0, color in INITIAL_CONDITIONS:
    path = cobweb_path(x0, GAMMA_TRAP, n_iter=60)
    xs = [p[0] for p in path]
    ys = [p[1] for p in path]
    ax.plot(xs, ys, '-', color=color, linewidth=0.9, alpha=0.75, zorder=4)
    # Starting marker
    ax.plot(x0, 0, marker='v', markersize=7,
            markerfacecolor=color, markeredgecolor=color, zorder=6)

# Fixed points
for fp in fps_trap:
    stab = stability(fp, GAMMA_TRAP)
    marker = 'o' if stab else 's'
    facecolor = 'black' if stab else 'white'
    ax.plot(fp, fp, marker=marker, markersize=9,
            markerfacecolor=facecolor, markeredgecolor='black',
            markeredgewidth=1.4, zorder=7)

# Labels
ax.annotate(r'$\bar{c}_g^{\mathrm{low}}$',
            xy=(fp_low, fp_low), xytext=(fp_low + 0.06, fp_low - 0.08),
            fontsize=10)
ax.annotate(r'$\bar{c}_g^{\mathrm{mid}}$',
            xy=(fp_mid, fp_mid), xytext=(fp_mid - 0.14, fp_mid + 0.05),
            fontsize=10)
ax.annotate(r'$\bar{c}_g^{\mathrm{high}}$',
            xy=(fp_high, fp_high), xytext=(fp_high - 0.22, fp_high - 0.05),
            fontsize=10)

# Legend (custom)
from matplotlib.lines import Line2D
legend_handles = [
    Line2D([0], [0], color='k', linewidth=1.8, label=r'composed map $\mathcal{H}$'),
    Line2D([0], [0], color='k', linewidth=0.9, linestyle='--', alpha=0.5, label='45-degree line'),
    Line2D([0], [0], color='#1F4E79', linewidth=0.9, label='trajectory $\\to$ low'),
    Line2D([0], [0], color='#A0522D', linewidth=0.9, label='trajectory $\\to$ high'),
]
ax.legend(handles=legend_handles, loc='lower right', framealpha=0.95, fontsize=8.5)

ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.set_xlabel(r'$\bar{c}_g^t$')
ax.set_ylabel(r'$\bar{c}_g^{t+1}$')
ax.set_title(r'Cobweb iteration in the trap regime ($\tilde{\gamma} = 0.05$)')
ax.set_aspect('equal')
ax.grid(True, alpha=0.25)

plt.tight_layout()
plt.savefig('/home/claude/work/figures/fig_cobweb.pdf', bbox_inches='tight', dpi=300)
plt.savefig('/home/claude/work/figures/fig_cobweb.png', bbox_inches='tight', dpi=200)
plt.close()
print("Saved fig_cobweb.pdf and fig_cobweb.png")

# ============================================================================
# Figure B: Trajectory time-series
# ============================================================================

fig, ax = plt.subplots(figsize=(6.5, 3.8))

N_GENERATIONS = 30
t_axis = np.arange(N_GENERATIONS + 1)

for x0, color in INITIAL_CONDITIONS:
    trajectory = [x0]
    x = x0
    for _ in range(N_GENERATIONS):
        x = H(x, GAMMA_TRAP)
        trajectory.append(x)
    ax.plot(t_axis, trajectory, '-', color=color, linewidth=1.4,
            marker='o', markersize=3.5, alpha=0.85,
            label=r'$\bar{c}_g^0 = $' + f'{x0:.2f}')

# Reference lines at attractors
ax.axhline(fp_low, color='gray', linestyle=':', linewidth=0.9, alpha=0.7)
ax.axhline(fp_high, color='gray', linestyle=':', linewidth=0.9, alpha=0.7)
ax.axhline(fp_mid, color='red', linestyle=':', linewidth=0.9, alpha=0.5)

# Annotate attractors
ax.text(N_GENERATIONS + 0.3, fp_low, r'$\bar{c}_g^{\mathrm{low}}$', fontsize=10, va='center')
ax.text(N_GENERATIONS + 0.3, fp_high, r'$\bar{c}_g^{\mathrm{high}}$', fontsize=10, va='center')
ax.text(N_GENERATIONS + 0.3, fp_mid, r'$\bar{c}_g^{\mathrm{mid}}$ (tipping)',
        fontsize=9, va='center', color='darkred')

ax.set_xlim(0, N_GENERATIONS + 4)
ax.set_ylim(-0.02, 1.05)
ax.set_xlabel(r'generation $t$')
ax.set_ylabel(r'$\bar{c}_g^t$')
ax.set_title(r'Group-prior trajectories across generations ($\tilde{\gamma} = 0.05$)')
ax.grid(True, alpha=0.25)
ax.legend(loc='center right', framealpha=0.95, fontsize=8.5)

plt.tight_layout()
plt.savefig('/home/claude/work/figures/fig_trajectory_timeseries.pdf', bbox_inches='tight', dpi=300)
plt.savefig('/home/claude/work/figures/fig_trajectory_timeseries.png', bbox_inches='tight', dpi=200)
plt.close()
print("Saved fig_trajectory_timeseries.pdf and fig_trajectory_timeseries.png")
print("Done.")
