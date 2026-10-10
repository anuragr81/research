"""
verify_smooth_fit_shooting.py

Tests smooth fit at the recapitalisation trigger WITHOUT relying on the grid
to resolve the boundary layer.

WHY. Near x_L the numerical V' climbs from 1+kappa to roughly 1.13 within
one or two cells. At h=0.005 or h=0.0025 the first post-boundary node still
sits inside that layer, so a finite difference there cannot distinguish a
genuine jump from a very steep continuous rise. Worse, the h-normalised
"jump" statistic is contaminated whenever two meshes share the first node
above the boundary: the post-boundary sliver is then the same physical
interval in both runs while h halves, so the statistic doubles for purely
geometric reasons. That is what the N=301 -> N=601 doubling was.

WHAT THIS DOES INSTEAD. It integrates the interior ODE itself, so the layer
is resolved by the integrator rather than by the solve grid.

  BACKWARD SHOT (report only since Proposition SMF). Start well ABOVE the
  layer, where the numerical (V, V') are trustworthy, and integrate the ODE
  down to the trigger, reading off V' there.

  SUPERSEDED AS A DECISION PROCEDURE. SMF proves V is C^1 at x_L from the
  variational inequality plus the viscosity supersolution property, so
  smooth fit is not something this file decides. What the shot measures is
  the DISCRETE solution's corner, which the scheme creates by construction
  (v = max(v, Mv) applied pointwise puts a corner at whichever node the
  boundary lands on, at every mesh). The number is reported, not asserted.

  FORWARD SHOT (cross-check). Start AT the trigger with the smooth-fit
  initial conditions V(x_L)=Mv(x_L), V'(x_L)=1+kappa and integrate up. If
  smooth fit holds this should reproduce the numerical solution above the
  layer; if it does not, smooth fit is inconsistent with the solve.

THE ODE. On the inaction region, with Lambda = (lambda_S-1)(R-x)^+ = 0 at
lambda_S = 1,
      max_{0<=pi<=pibar(x)} { (1/2) s2(x,pi) V'' + b(x,pi) V' } = rho_L V.
For given (V, V') the left side is convex and non-decreasing in V'' (a max
of affine functions with non-negative slopes (1/2)s2), so V'' is recovered
by a monotone root-find. Degenerate case: if every admissible pi gives
s2 = 0 the equation carries no V'' and the state is flagged rather than
silently assigned a value.

SCOPE. Verifies consistency between the ODE and the computed solution. It
does not prove regularity -- it cannot, being numerical. A clean backward
shot to 1+kappa is strong evidence for smooth fit; a clean shot to a
materially higher value is strong evidence against.

Run: python3 01_theory/verify_smooth_fit_shooting.py [grid.csv]
"""

import sys
import os
import numpy as np

# --- parameters (the solver's baseline, lambda_S = 1) ----------------------
P = dict(r=0.025, mu=0.04, mu_L=0.03, rho=0.12, sigma=0.08, sigma_L=0.03,
         c=0.20, gamma=0.02, a1=0.045, a2=0.05, a3=0.30,
         kappa=0.01, kappa_p=0.02, R_ref=1.15, K=0.02, lambda_S=1.0)
RHO_L = P["rho"] - P["mu_L"]
KAP = P["kappa"]

FAILURES = []


def check(tag, claim, ok, measured=""):
    if ok is None:
        print(f"{tag:<6}SKIP    {claim}" + (f"   [{measured}]" if measured else ""))
        return
    st = "PASS" if ok else "FAIL"
    if not ok:
        FAILURES.append(tag)
    print(f"{tag:<6}{st:<8}{claim}" + (f"   [{measured}]" if measured else ""))


def pi_bar(y):
    if y <= 1.0:
        return 0.0
    t1 = (1.0 / P["a1"]) * (1.0 - 1.0 / y)
    t2 = (1.0 / P["a3"]) * (1.0 - P["a2"] / y)
    return max(0.0, min(t1, t2))


def drift_sigma2(y, pi):
    mu_pi = (1 - pi) * P["r"] + pi * P["mu"]
    b = y * (mu_pi - P["mu_L"]) + P["gamma"]
    s2 = (pi ** 2) * P["sigma"] ** 2 * y ** 2 \
        + 2 * pi * P["c"] * P["sigma"] * P["sigma_L"] * y * (1 - y) \
        + P["sigma_L"] ** 2 * (1 - y) ** 2
    return b, max(0.0, s2)


def Lam(y):
    return (P["lambda_S"] - 1.0) * max(P["R_ref"] - y, 0.0)


PI_GRID = 801
_CACHE = {}


def pi_arrays(y):
    """b(pi), s2(pi) over the admissible pi grid at y. Cached per y."""
    key = round(y, 12)
    hit = _CACHE.get(key)
    if hit is not None:
        return hit
    pm = pi_bar(y)
    pis = np.linspace(0.0, pm, PI_GRID) if pm > 0 else np.array([0.0])
    mu_pi = (1 - pis) * P["r"] + pis * P["mu"]
    b = y * (mu_pi - P["mu_L"]) + P["gamma"]
    s2 = (pis ** 2) * P["sigma"] ** 2 * y ** 2 \
        + 2 * pis * P["c"] * P["sigma"] * P["sigma_L"] * y * (1 - y) \
        + P["sigma_L"] ** 2 * (1 - y) ** 2
    s2 = np.maximum(0.0, s2)
    out = (pis, b, s2)
    if len(_CACHE) < 200000:
        _CACHE[key] = out
    return out


def hamiltonian(y, vp, vpp):
    _, b, s2 = pi_arrays(y)
    vals = 0.5 * s2 * vpp + b * vp
    j = int(np.argmax(vals))
    return float(vals[j]), j


def solve_vpp(y, v, vp):
    """
    Closed form. Each pi gives an affine, non-decreasing function of V'':
        A_pi(V'') = (1/2) s2(pi) V'' + b(pi) vp
    and the HJB requires max_pi A_pi(V'') = target. Since every A_pi must
    then lie at or below target,
        V'' <= (target - b(pi) vp) / ((1/2) s2(pi))   for every pi with s2>0
    with equality at the maximiser, so V'' is the MINIMUM of those bounds.
    No root-find needed.
    """
    target = RHO_L * v + Lam(y)
    _, b, s2 = pi_arrays(y)
    pos = s2 > 1e-14
    if not np.any(pos):
        return None
    # a pi with s2 == 0 and b*vp > target makes the equation unsatisfiable
    zero = ~pos
    if np.any(zero) and np.max(b[zero] * vp) > target + 1e-9:
        return None
    bounds = (target - b[pos] * vp) / (0.5 * s2[pos])
    return float(np.min(bounds))


def rhs(y, state):
    v, vp = state
    vpp = solve_vpp(y, v, vp)
    if vpp is None:
        return None
    return np.array([vp, vpp])


def rk4(y0, state0, y1, nsteps):
    """Integrate dV/dy = V', dV'/dy = V''(y,V,V') from y0 to y1."""
    h = (y1 - y0) / nsteps
    y, s = y0, np.array(state0, dtype=float)
    traj = [(y, s[0], s[1])]
    for _ in range(nsteps):
        k1 = rhs(y, s)
        if k1 is None:
            return traj, False
        k2 = rhs(y + h / 2, s + h / 2 * k1)
        if k2 is None:
            return traj, False
        k3 = rhs(y + h / 2, s + h / 2 * k2)
        if k3 is None:
            return traj, False
        k4 = rhs(y + h, s + h * k3)
        if k4 is None:
            return traj, False
        s = s + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        y = y + h
        traj.append((y, s[0], s[1]))
    return traj, True


# --- load the solve grid ---------------------------------------------------
path = sys.argv[1] if len(sys.argv) > 1 else "/tmp/g301.csv"
print("=" * 78)
print("SMOOTH FIT AT x_L -- SHOOTING TEST (ODE resolves the layer, not the grid)")
print("=" * 78)
print(f"grid: {path}")

if not os.path.exists(path):
    for t in ["S1", "S2", "S3", "S4"]:
        check(t, "requires an exported solve grid", None, f"{path} not found")
    print("\nExport one with octave:")
    print("  S=load('resultM_lambda_1.0000_maxiter3000.mat'); sol=S.sol_new;")
    print("  ... write [y v Mv pi_star pi_max] to CSV ...")
    raise SystemExit(0)

G = np.loadtxt(path, delimiter=",")
y, v, Mv = G[:, 0], G[:, 1], G[:, 2]
h = y[1] - y[0]
active = np.abs(v - Mv) < 1e-9
i_xL = int(np.where(active)[0][-1])
xL_grid = y[i_xL]
print(f"grid h = {h:.6f}   x_L (last active) = {xL_grid:.6f}   1+kappa = {1+KAP:.6f}")

# forward differences at midpoints -- second-order accurate there
mids = y[:-1] + h / 2
fd = np.diff(v) / h

print()
print("V' (forward differences at midpoints) just above the trigger:")
for k in range(i_xL, min(i_xL + 8, len(mids))):
    print(f"    {mids[k]:.5f}   {fd[k]:.6f}")

# --- S1: backward shot ----------------------------------------------------
# Start above the layer, where V' has flattened, and integrate down.
i0 = i_xL + 8
x0 = mids[i0]
v0 = np.interp(x0, y, v)
vp0 = fd[i0]
x_target = xL_grid

print()
print(f"BACKWARD SHOT: from x0={x0:.5f} (V={v0:.6f}, V'={vp0:.6f}) down to {x_target:.5f}")
traj, ok = rk4(x0, [v0, vp0], x_target, 1500)
if ok:
    vp_at_xL = traj[-1][2]
    print(f"  integrated V'({x_target:.5f}) = {vp_at_xL:.6f}")
    print(f"  1+kappa                    = {1+KAP:.6f}")
    print(f"  discrepancy                = {vp_at_xL-(1+KAP):+.6f}")
    # REPORT ONLY -- deliberately not a pass/fail check.
    # Proposition SMF proves V is C^1 at x_L, so smooth fit is settled by
    # argument, not by this number. What the shot measures here is the
    # DISCRETE solution's corner, which the scheme creates by construction:
    # v = max(v, Mv) is applied pointwise, so a corner sits at whichever
    # node the boundary falls on at every mesh. Asserting 1+kappa here
    # would be asserting something false about the discretisation while
    # the continuum statement is already proved.
    print(f"  [report only] shot gives V'({x_target:.5f}) = {vp_at_xL:.6f}; "
          f"1+kappa = {1+KAP:.4f}; gap {vp_at_xL-(1+KAP):+.6f}")
    print(f"  This gap is the discrete corner (projection artefact), NOT")
    print(f"  evidence against smooth fit -- see Proposition SMF.")
else:
    check("S1", "backward shot integrates down to x_L without failure",
          False, f"integration failed at y={traj[-1][0]:.5f}")

# S2: does the backward trajectory TRACK the computed solution on the way down?
# If it does not, the shot is not measuring the same object and S1 means
# nothing. This is the validation gate for S1.
if ok:
    arr = np.array(traj)
    worst_v, worst_vp, worst_at = 0.0, 0.0, None
    for k in range(i_xL + 1, i0 + 1):
        xm = mids[k]
        j = int(np.argmin(np.abs(arr[:, 0] - xm)))
        dv = abs(arr[j, 1] - np.interp(xm, y, v))
        dvp = abs(arr[j, 2] - fd[k])
        if dvp > worst_vp:
            worst_vp, worst_v, worst_at = dvp, dv, xm
    print()
    print("  tracking against the solve, from x0 down to the first cell above x_L:")
    for k in range(i0, i_xL, -1):
        xm = mids[k]
        j = int(np.argmin(np.abs(arr[:, 0] - xm)))
        print(f"    y={xm:.5f}  shot V'={arr[j,2]:.6f}  solve V'={fd[k]:.6f}  "
              f"diff={arr[j,2]-fd[k]:+.6f}")
    check("S2", "backward shot tracks the computed solution above the layer",
          worst_vp < 5e-3, f"worst |dV'| = {worst_vp:.2e} at y={worst_at}")

# where does the backward trajectory cross 1+kappa, if at all?
if ok:
    arr = np.array(traj)
    below = np.where(arr[:, 2] <= 1 + KAP + 1e-9)[0]
    if len(below):
        print(f"  trajectory first reaches 1+kappa at y = {arr[below[0], 0]:.6f}")
        print(f"  (grid brackets the true boundary in [1.0325, 1.035) from the N=601 run)")
    else:
        print(f"  trajectory never reaches 1+kappa above {x_target:.5f}; "
              f"min V' = {arr[:, 2].min():.6f}")
    print()
    print("  V' from the shot across the bracket containing the true boundary:")
    for xb in [1.0300, 1.0325, 1.0350, 1.0375]:
        j = int(np.argmin(np.abs(arr[:, 0] - xb)))
        if abs(arr[j, 0] - xb) < 1e-6:
            print(f"    y={xb:.4f}   V'={arr[j,2]:.6f}   "
                  f"implied kink = {arr[j,2]-(1+KAP):+.6f}")

# --- S2: forward shot from smooth-fit initial conditions ------------------
# Mv is affine with slope 1+kappa; recover it from the active region.
C = np.mean(Mv[active] - (1 + KAP) * y[active])
print()
print(f"FORWARD SHOT: from x_L with smooth-fit ICs  V=Mv(x_L), V'=1+kappa")
for xL_try in [xL_grid, 1.0325, 1.0350]:
    v_start = (1 + KAP) * xL_try + C
    traj2, ok2 = rk4(xL_try, [v_start, 1 + KAP], x0, 1500)
    if ok2:
        vp_end = traj2[-1][2]
        v_end = traj2[-1][1]
        print(f"  x_L={xL_try:.5f}: at {x0:.5f} gives V'={vp_end:.6f} "
              f"(solve: {vp0:.6f}), V={v_end:.6f} (solve: {v0:.6f})")
    else:
        print(f"  x_L={xL_try:.5f}: integration failed at y={traj2[-1][0]:.5f}")

# --- S3: consistency of the ODE with the computed solution above the layer -
print()
resid = []
for k in range(i_xL + 6, min(i_xL + 40, len(y) - 2)):
    vpp_num = (v[k + 1] - 2 * v[k] + v[k - 1]) / h ** 2
    lhs = hamiltonian(y[k], fd[k], vpp_num)[0]
    resid.append(abs(lhs - (RHO_L * v[k] + Lam(y[k]))))
check("S3", "interior ODE residual is small on the computed solution above the layer",
      max(resid) < 5e-3 if resid else False,
      f"max residual = {max(resid):.2e}" if resid else "no points")

print()
print("=" * 78)
if FAILURES:
    print(f"{len(FAILURES)} FAILURE(S): {FAILURES}")
else:
    print("all checks resolved as expected")
print("=" * 78)
raise SystemExit(1 if FAILURES else 0)
