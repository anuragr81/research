"""
Additional figure candidates:

Figure C: Bifurcation diagram swept in δ (discrimination intensity) with γ̃ fixed.
  Shows how the closure trap appears as discrimination intensifies — the
  policy-relevant comparative static.

Figure D: Two-panel cobweb (trap regime + post-bifurcation) with trajectories
  overlaid. Variant of existing Figure 2 with iteration dynamics shown.
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import rcParams
from matplotlib.lines import Line2D

rcParams['font.family'] = 'serif'
rcParams['font.size'] = 10
rcParams['axes.labelsize'] = 11
rcParams['axes.titlesize'] = 11
rcParams['legend.fontsize'] = 9
rcParams['mathtext.fontset'] = 'cm'

MU = 0.05
S_G = 1.0
GAMMA_FIXED = 0.05  # trap-regime gamma for the δ-bifurcation

def F(c):
    return (1.0 - c) ** 2

def H(c, gamma_tilde, delta):
    return MU + c * (1 - MU + gamma_tilde - S_G * delta * F(c)) - gamma_tilde * c ** 2

def fixed_points(gamma_tilde, delta, n_seeds=400):
    c_grid = np.linspace(0.001, 0.999, n_seeds)
    g = lambda c: H(c, gamma_tilde, delta) - c
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

def stability(c_star, gamma_tilde, delta, eps=1e-5):
    h_prime = (H(c_star + eps, gamma_tilde, delta) - H(c_star - eps, gamma_tilde, delta)) / (2 * eps)
    return abs(h_prime) < 1

def cobweb_path(x0, gamma_tilde, delta, n_iter=60):
    pts = [(x0, 0.0)]
    x = x0
    for _ in range(n_iter):
        y = H(x, gamma_tilde, delta)
        pts.append((x, y))
        pts.append((y, y))
        if abs(y - x) < 1e-6:
            break
        x = y
    return pts

# ============================================================================
# Figure C: Bifurcation diagram in δ
# ============================================================================

fig, ax = plt.subplots(figsize=(5.8, 4.0))

delta_range = np.linspace(0.001, 1.0, 400)
stable_low, unstable_mid, stable_high = [], [], []

for d in delta_range:
    fps = fixed_points(GAMMA_FIXED, d)
    interior = [f for f in fps if f < 0.999]
    # Boundary 1 is always present and (in trap) stable
    if len(interior) == 2:
        stable_low.append((d, interior[0]))
        unstable_mid.append((d, interior[1]))
        stable_high.append((d, 1.0))
    elif len(interior) == 0:
        stable_high.append((d, 1.0))
    elif len(interior) == 1:
        stable_low.append((d, interior[0]))
        unstable_mid.append((d, interior[0]))
        stable_high.append((d, 1.0))

# Determine bifurcation point (where low / mid emerge as δ increases)
delta_star = None
if stable_low:
    delta_star = stable_low[0][0]

# Plot
if stable_low:
    sl = np.array(stable_low)
    ax.plot(sl[:, 0], sl[:, 1], 'k-', linewidth=1.8, label='stable fixed point')
sh = np.array(stable_high)
ax.plot(sh[:, 0], sh[:, 1], 'k-', linewidth=1.8)
if unstable_mid:
    um = np.array(unstable_mid)
    ax.plot(um[:, 0], um[:, 1], 'k--', linewidth=1.5, label='unstable fixed point')

if delta_star is not None:
    ax.axvline(delta_star, color='gray', linestyle=':', linewidth=1.0, alpha=0.7)
    ax.annotate(r'$\delta^*$',
                xy=(delta_star, 0.02), xytext=(delta_star + 0.01, 0.05),
                fontsize=11)
    ax.annotate('closure trap\nemerges',
                xy=(delta_star, stable_low[0][1] if stable_low else 0.5),
                xytext=(delta_star + 0.08, 0.45),
                fontsize=9,
                arrowprops=dict(arrowstyle='->', color='gray', lw=0.8))

ax.set_xlabel(r'discrimination intensity $\delta$')
ax.set_ylabel(r'fixed point $\bar{c}_g^*$')
ax.set_xlim(0, 1.0)
ax.set_ylim(0, 1.05)
ax.set_title(r'Closure as $\delta$ increases ($\tilde{\gamma} = 0.05$, $\mu = 0.05$)')
ax.grid(True, alpha=0.25)
ax.legend(loc='center right', framealpha=0.95)

plt.tight_layout()
plt.savefig('/home/claude/work/figures/fig_bifurcation_delta.pdf', bbox_inches='tight', dpi=300)
plt.savefig('/home/claude/work/figures/fig_bifurcation_delta.png', bbox_inches='tight', dpi=200)
plt.close()
print(f"Figure C: δ* (closure emerges) ≈ {delta_star:.3f}" if delta_star else "Figure C: no trap regime in swept range")

# ============================================================================
# Figure D: Two-panel cobweb (trap + post-bifurcation), trajectories overlaid
# ============================================================================

fig, axes = plt.subplots(1, 2, figsize=(9.0, 4.4))
c = np.linspace(0, 1, 400)
DELTA_FOR_COBWEB = 0.5

# --- Panel (a): trap regime ---
gamma_a = 0.05
H_a = H(c, gamma_a, DELTA_FOR_COBWEB)
ax = axes[0]
ax.plot(c, H_a, 'k-', linewidth=1.7, zorder=3)
ax.plot(c, c, 'k--', linewidth=0.9, alpha=0.5, zorder=2)
fps_a = fixed_points(gamma_a, DELTA_FOR_COBWEB)

initial_a = [
    (0.05, '#1F4E79'),
    (0.30, '#1F4E79'),
    (0.82, '#A0522D'),
    (0.90, '#A0522D'),
]
for x0, color in initial_a:
    path = cobweb_path(x0, gamma_a, DELTA_FOR_COBWEB, n_iter=50)
    xs = [p[0] for p in path]
    ys = [p[1] for p in path]
    ax.plot(xs, ys, '-', color=color, linewidth=0.85, alpha=0.75, zorder=4)
    ax.plot(x0, 0, marker='v', markersize=6,
            markerfacecolor=color, markeredgecolor=color, zorder=6)

for fp in fps_a:
    stab = stability(fp, gamma_a, DELTA_FOR_COBWEB)
    marker = 'o' if stab else 's'
    facecolor = 'black' if stab else 'white'
    ax.plot(fp, fp, marker=marker, markersize=8,
            markerfacecolor=facecolor, markeredgecolor='black',
            markeredgewidth=1.3, zorder=7)

ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.set_xlabel(r'$\bar{c}_g^t$')
ax.set_ylabel(r'$\bar{c}_g^{t+1}$')
ax.set_title(r'(a) Trap regime ($\tilde{\gamma} = 0.05$)')
ax.set_aspect('equal')
ax.grid(True, alpha=0.25)

# --- Panel (b): post-bifurcation ---
gamma_b = 0.30
H_b = H(c, gamma_b, DELTA_FOR_COBWEB)
ax = axes[1]
ax.plot(c, H_b, 'k-', linewidth=1.7, zorder=3)
ax.plot(c, c, 'k--', linewidth=0.9, alpha=0.5, zorder=2)
fps_b = fixed_points(gamma_b, DELTA_FOR_COBWEB)

initial_b = [(0.05, '#A0522D'), (0.30, '#A0522D'), (0.60, '#A0522D'), (0.90, '#A0522D')]
for x0, color in initial_b:
    path = cobweb_path(x0, gamma_b, DELTA_FOR_COBWEB, n_iter=30)
    xs = [p[0] for p in path]
    ys = [p[1] for p in path]
    ax.plot(xs, ys, '-', color=color, linewidth=0.85, alpha=0.75, zorder=4)
    ax.plot(x0, 0, marker='v', markersize=6,
            markerfacecolor=color, markeredgecolor=color, zorder=6)

for fp in fps_b:
    stab = stability(fp, gamma_b, DELTA_FOR_COBWEB)
    marker = 'o' if stab else 's'
    facecolor = 'black' if stab else 'white'
    ax.plot(fp, fp, marker=marker, markersize=8,
            markerfacecolor=facecolor, markeredgecolor='black',
            markeredgewidth=1.3, zorder=7)

ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.set_xlabel(r'$\bar{c}_g^t$')
ax.set_ylabel(r'$\bar{c}_g^{t+1}$')
ax.set_title(r'(b) Post-bifurcation ($\tilde{\gamma} = 0.30$)')
ax.set_aspect('equal')
ax.grid(True, alpha=0.25)

# Single legend for the figure
legend_handles = [
    Line2D([0], [0], color='k', linewidth=1.7, label=r'composed map $\mathcal{H}$'),
    Line2D([0], [0], color='k', linewidth=0.9, linestyle='--', alpha=0.5, label='45-degree line'),
    Line2D([0], [0], color='#1F4E79', linewidth=0.9, label=r'cobweb $\to$ low'),
    Line2D([0], [0], color='#A0522D', linewidth=0.9, label=r'cobweb $\to$ high'),
]
fig.legend(handles=legend_handles, loc='lower center', ncol=4,
           bbox_to_anchor=(0.5, -0.02), framealpha=0.95, fontsize=9)

plt.tight_layout()
plt.subplots_adjust(bottom=0.18)
plt.savefig('/home/claude/work/figures/fig_cobweb_twopanel.pdf', bbox_inches='tight', dpi=300)
plt.savefig('/home/claude/work/figures/fig_cobweb_twopanel.png', bbox_inches='tight', dpi=200)
plt.close()
print(f"Figure D: trap-regime fps = {[round(f,3) for f in fps_a]}")
print(f"Figure D: post-bifurcation fps = {[round(f,3) for f in fps_b]}")
print("Done.")
