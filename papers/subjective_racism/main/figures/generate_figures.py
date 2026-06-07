"""
Generate illustrative figures for the discrimination paper.

Figure 1: Composed map H(c̄_g; s_g) vs 45-degree line, two panels showing
  the closure-trap regime and the post-bifurcation regime.

Figure 2: Bifurcation diagram in γ̃, showing the saddle-node where the
  low fixed point and the tipping point merge.

Functional form for illustration: F(c̄_g) = (1 - c̄_g)^2 satisfies the
model's qualitative requirements (continuous, F(1) = 0, decreasing).

Parameters: μ = 0.05, δ = 0.5, s_g = 1.
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import rcParams

# Match LaTeX serif typography
rcParams['font.family'] = 'serif'
rcParams['font.size'] = 10
rcParams['axes.labelsize'] = 11
rcParams['axes.titlesize'] = 11
rcParams['legend.fontsize'] = 9
rcParams['mathtext.fontset'] = 'cm'

# Model parameters (held fixed across both figures)
MU = 0.05
DELTA = 0.5
S_G = 1.0

def F(c):
    """Discrimination rate function (illustrative form)."""
    return (1.0 - c) ** 2

def H(c, gamma_tilde):
    """Composed intergenerational map H(c̄_g; s_g)."""
    return MU + c * (1 - MU + gamma_tilde - S_G * DELTA * F(c)) - gamma_tilde * c ** 2

def fixed_points(gamma_tilde, n_seeds=200):
    """Find fixed points of H by scanning and Newton-refining."""
    c_grid = np.linspace(0.001, 0.999, n_seeds)
    g = lambda c: H(c, gamma_tilde) - c
    fps = []
    for i in range(len(c_grid) - 1):
        if g(c_grid[i]) * g(c_grid[i + 1]) < 0:
            # Bisection
            a, b = c_grid[i], c_grid[i + 1]
            for _ in range(100):
                m = 0.5 * (a + b)
                if g(a) * g(m) < 0:
                    b = m
                else:
                    a = m
            fps.append(0.5 * (a + b))
    # c = 1 is always a fixed point (boundary)
    fps.append(1.0)
    fps = sorted(set(round(f, 6) for f in fps))
    return fps

def stability(c_star, gamma_tilde, eps=1e-5):
    """Local stability: |H'(c*)| < 1 ⇒ stable."""
    h_prime = (H(c_star + eps, gamma_tilde) - H(c_star - eps, gamma_tilde)) / (2 * eps)
    return abs(h_prime) < 1


# ============================================================================
# Figure 1: H vs 45-degree line, two panels
# ============================================================================

fig, axes = plt.subplots(1, 2, figsize=(8.5, 3.6))

c = np.linspace(0, 1, 400)

# Panel A: trap regime
gamma_a = 0.05
H_a = H(c, gamma_a)
ax = axes[0]
ax.plot(c, H_a, 'k-', linewidth=1.6, label=r'$\mathcal{H}(\bar{c}_g)$')
ax.plot(c, c, 'k--', linewidth=0.9, alpha=0.6, label='45-degree line')
fps_a = fixed_points(gamma_a)
for fp in fps_a:
    stab = stability(fp, gamma_a)
    marker = 'o' if stab else 's'
    facecolor = 'black' if stab else 'white'
    ax.plot(fp, fp, marker=marker, markersize=8,
            markerfacecolor=facecolor, markeredgecolor='black',
            markeredgewidth=1.3, zorder=5)
ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.set_xlabel(r'$\bar{c}_g^t$')
ax.set_ylabel(r'$\bar{c}_g^{t+1}$')
ax.set_title(r'(a) Trap regime ($\tilde{\gamma} = 0.05$)')
ax.legend(loc='lower right', framealpha=0.95)
ax.set_aspect('equal')
ax.grid(True, alpha=0.25)

# Annotate fixed points in panel A
if len(fps_a) >= 3:
    fp_low, fp_mid, fp_high = fps_a[0], fps_a[1], fps_a[2]
    ax.annotate(r'$\bar{c}_g^{\mathrm{low}}$',
                xy=(fp_low, fp_low), xytext=(fp_low + 0.07, fp_low - 0.06),
                fontsize=9)
    ax.annotate(r'$\bar{c}_g^{\mathrm{mid}}$',
                xy=(fp_mid, fp_mid), xytext=(fp_mid - 0.14, fp_mid + 0.04),
                fontsize=9)
    ax.annotate(r'$\bar{c}_g^{\mathrm{high}}$',
                xy=(fp_high, fp_high), xytext=(fp_high - 0.18, fp_high - 0.07),
                fontsize=9)

# Panel B: post-bifurcation
gamma_b = 0.30
H_b = H(c, gamma_b)
ax = axes[1]
ax.plot(c, H_b, 'k-', linewidth=1.6, label=r'$\mathcal{H}(\bar{c}_g)$')
ax.plot(c, c, 'k--', linewidth=0.9, alpha=0.6, label='45-degree line')
fps_b = fixed_points(gamma_b)
for fp in fps_b:
    stab = stability(fp, gamma_b)
    marker = 'o' if stab else 's'
    facecolor = 'black' if stab else 'white'
    ax.plot(fp, fp, marker=marker, markersize=8,
            markerfacecolor=facecolor, markeredgecolor='black',
            markeredgewidth=1.3, zorder=5)
ax.set_xlim(0, 1)
ax.set_ylim(0, 1)
ax.set_xlabel(r'$\bar{c}_g^t$')
ax.set_ylabel(r'$\bar{c}_g^{t+1}$')
ax.set_title(r'(b) Post-bifurcation ($\tilde{\gamma} = 0.30$)')
ax.legend(loc='lower right', framealpha=0.95)
ax.set_aspect('equal')
ax.grid(True, alpha=0.25)

plt.tight_layout()
plt.savefig('/home/claude/work/figures/fig_closure_map.pdf',
            bbox_inches='tight', dpi=300)
plt.close()
print(f"Figure 1: fixed points in trap regime: {[round(f,3) for f in fps_a]}")
print(f"Figure 1: fixed points post-bifurcation: {[round(f,3) for f in fps_b]}")


# ============================================================================
# Figure 2: Bifurcation diagram
# ============================================================================

fig, ax = plt.subplots(figsize=(5.5, 4.0))

gamma_range = np.linspace(0.001, 0.35, 350)
stable_low = []
unstable_mid = []
stable_high = []

for gt in gamma_range:
    fps = fixed_points(gt)
    # Classify
    interior_fps = [f for f in fps if f < 0.999]
    boundary_fp = 1.0  # always present
    if len(interior_fps) == 2:
        # Trap regime: low stable, mid unstable
        stable_low.append((gt, interior_fps[0]))
        unstable_mid.append((gt, interior_fps[1]))
        stable_high.append((gt, boundary_fp))
    elif len(interior_fps) == 0:
        # Post-bifurcation: only high
        stable_high.append((gt, boundary_fp))
    elif len(interior_fps) == 1:
        # Saddle-node point itself
        stable_low.append((gt, interior_fps[0]))
        unstable_mid.append((gt, interior_fps[0]))
        stable_high.append((gt, boundary_fp))

# Plot stable branches (solid)
if stable_low:
    sl = np.array(stable_low)
    ax.plot(sl[:, 0], sl[:, 1], 'k-', linewidth=1.6,
            label='stable fixed point')
sh = np.array(stable_high)
ax.plot(sh[:, 0], sh[:, 1], 'k-', linewidth=1.6)

# Plot unstable branch (dashed)
if unstable_mid:
    um = np.array(unstable_mid)
    ax.plot(um[:, 0], um[:, 1], 'k--', linewidth=1.4,
            label='unstable fixed point')

# Annotate saddle-node
if stable_low:
    gamma_star = stable_low[-1][0]
    ax.axvline(gamma_star, color='gray', linestyle=':', linewidth=1.0, alpha=0.7)
    ax.annotate(r'$\tilde{\gamma}^*$',
                xy=(gamma_star, 0.02), xytext=(gamma_star + 0.01, 0.04),
                fontsize=11)
    ax.annotate('saddle-node\nbifurcation',
                xy=(gamma_star, stable_low[-1][1]),
                xytext=(gamma_star + 0.04, 0.42),
                fontsize=9,
                arrowprops=dict(arrowstyle='->', color='gray', lw=0.8))

ax.set_xlabel(r'effective mobility $\tilde{\gamma}$')
ax.set_ylabel(r'fixed point $\bar{c}_g^*$')
ax.set_xlim(0, 0.35)
ax.set_ylim(0, 1.02)
ax.grid(True, alpha=0.25)
ax.legend(loc='center right', framealpha=0.95)

plt.tight_layout()
plt.savefig('/home/claude/work/figures/fig_bifurcation.pdf',
            bbox_inches='tight', dpi=300)
plt.close()
print(f"Figure 2: bifurcation γ̃* ≈ {gamma_star:.3f}")
print("Figures saved.")
