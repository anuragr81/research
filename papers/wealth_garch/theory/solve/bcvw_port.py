"""Python port of the solver of Bayraktar, Chevalier, Ly Vath and Wang (2026), from the
authors' repository github.com/yuqiongwang/bank_capital_structure at commit 4a4996ab
(solve_bank_qvi.m, compute_H_operator.m, compute_pi_bar.m, drift_and_sigma2.m,
compute_y_star_analytical.m, pick_y_post.m, enforce_slope_floor.m and the local function
compute_one_sensitivity of 00_b1_sensitivity_analysis.m), written line for line from
reading the MATLAB source. The original is MIT licensed, Copyright (c) 2026 Yuqiong Wang;
the notice applies to this port. The scheme is pseudo-time stepping (dt = 0.05) with the
obstacles applied after each implicit step, 301 nodes on [1.02, 2.5], 151 exposure points,
stop at a change below 1e-6 or after 1500 steps."""
import numpy as np

def pi_bar(y, p):
    t1 = (1/p["a1"])*(1 - 1/y); t2 = (1/p["a3"])*(1 - p["a2"]/y)
    pim = np.maximum(0, np.minimum(t1, t2))
    if p.get("pi_cap") is not None: pim = np.minimum(pim, p["pi_cap"])
    return np.where(y <= 1, 0.0, pim)

def drift_s2(y, pi, p):
    b = y*((1-pi)*p["r"] + pi*p["mu"] - p["mu_L"]) + p["gamma"]
    s2 = pi**2*p["sigma"]**2*y**2 + 2*pi*p["c"]*p["sigma"]*p["sigma_L"]*y*(1-y) + p["sigma_L"]**2*(1-y)**2
    return b, np.maximum(0, s2)

def coeffs(y, pi, p, dy, rhoL):
    b, s2 = drift_s2(y, pi, p)
    A = 0.5*s2/dy**2
    Am = A + np.maximum(-b, 0)/dy; Ap = A + np.maximum(b, 0)/dy
    return Am, -(Am+Ap) - rhoL, Ap

def H(y, v, p):
    kp, k = p["kappa_p"], p["kappa"]
    base = v - y/(1-kp)
    suf = np.maximum.accumulate(base[::-1])[::-1]
    out = np.full(len(y), -1e100)
    out[:-1] = suf[1:] + ((1-k)*y[:-1] + k)/(1-kp) + k*(y[:-1]-1)
    return out

def thomas(a, b, c, d):
    n = len(b); cp = np.zeros(n); dp = np.zeros(n)
    cp[0] = c[0]/b[0] if n > 1 else 0; dp[0] = d[0]/b[0]
    for i in range(1, n):
        m = b[i] - a[i-1]*cp[i-1]
        cp[i] = c[i]/m if i < n-1 else 0; dp[i] = (d[i] - a[i-1]*dp[i-1])/m
    x = np.zeros(n); x[-1] = dp[-1]
    for i in range(n-2, -1, -1): x[i] = dp[i] - cp[i]*x[i+1]
    return x

def solve(p, mode="QVI", N=301, y_max=2.5, dt=0.05, max_iter=1500, tol=1e-6, M=151):
    ylo = p["underline_y"]; y = np.linspace(ylo, y_max, N); dy = y[1]-y[0]
    rhoL = p["rho"] - p["mu_L"]; pim = pi_bar(y, p); fr = np.linspace(0, 1, M)
    v = np.maximum(y-1, 0)
    if mode == "HJB": v[0] = ylo-1
    pis = np.zeros(N)
    for it in range(1, max_iter+1):
        vold = v.copy()
        v[-1] = 2*v[-2] - v[-3]
        if mode == "HJB": v[0] = ylo-1
        yi = y[1:-1][:, None]; PI = pim[1:-1][:, None]*fr[None, :]
        Am, Ai, Ap = coeffs(yi, PI, p, dy, rhoL)
        Lv = Am*v[:-2, None] + Ai*v[1:-1, None] + Ap*v[2:, None]
        k = np.argmax(Lv, axis=1)            # first maximiser, as the strict '>' in the loop
        pis[1:-1] = PI[np.arange(N-2), k]; pis[0] = pis[1]; pis[-1] = pis[-2]
        Am, Ai, Ap = coeffs(y[1:-1], pis[1:-1], p, dy, rhoL)
        v0 = ylo-1 if mode == "HJB" else v[0]; vN = v[-1]
        bd = 1 - dt*Ai; rhs = v[1:-1].copy()
        rhs[0] -= (-dt*Am[0])*v0; rhs[-1] -= (-dt*Ap[-1])*vN
        vin = thomas(-dt*Am[1:], bd, -dt*Ap[:-1], rhs)
        vn = v.copy(); vn[1:-1] = vin; vn[-1] = vn[-2] + dy
        vn = np.maximum(vn, y-1)
        if mode == "QVI":
            Hv = H(y, vn, p); vn = np.maximum(vn, Hv); vn[0] = max(ylo-1, Hv[0])
        else:
            vn[0] = ylo-1
        for i in range(1, N):
            if vn[i] < vn[i-1] + dy: vn[i] = vn[i-1] + dy
        if mode == "HJB": vn[0] = ylo-1
        v = vn
        err = np.max(np.abs(v - vold))
        if err < tol: break
    return dict(y=y, v=v, pi=pis, pim=pim, it=it, err=err, rhoL=rhoL)

def ystar_analytical(sol, p):
    y, v = sol["y"], sol["v"]
    mu_opt = p["r"] + sol["pim"]*(p["mu"]-p["r"]) if p["mu"] >= p["r"] else p["r"]
    f = sol["rhoL"]*v - p["gamma"] + (p["mu_L"] - mu_opt)*y
    for i in range(1, len(y)):
        if f[i-1] <= 0 <= f[i]:
            return y[i-1] + (-f[i-1]/(f[i]-f[i-1]))*(y[i]-y[i-1]) if f[i]-f[i-1] > 1e-12 else 0.5*(y[i-1]+y[i])
    return y[np.argmin(np.abs(f))]

def ystar_slope(sol):
    y, v = sol["y"], sol["v"]; dy = y[1]-y[0]; g = np.diff(v)/dy - 1
    for k in range(1, len(g)):
        if g[k-1] > 0 >= g[k]:
            return y[k] + g[k-1]/(g[k-1]-g[k])*dy
    return np.nan

def ypost(sol, p, ys):
    y, v = sol["y"], sol["v"]; dy = y[1]-y[0]
    vp = np.zeros_like(v); vp[1:-1] = (v[2:]-v[:-2])/(2*dy)
    vp[0] = (-3*v[0]+4*v[1]-v[2])/(2*dy)
    target = 1/(1-p["kappa_p"]); I = np.where(y <= ys)[0]; yI = y[I]; g = vp[I]-target
    for j in range(min(5, len(yI)-1)+1, len(yI)):
        if g[j-1] > 0 >= g[j]:
            return yI[j-1] + g[j-1]/(g[j-1]-g[j])*(yI[j]-yI[j-1])
    return np.nan

def table1_point(p, **kw):
    q = solve(p, "QVI", **kw); h = solve(p, "HJB", **kw)
    vq = np.interp(1.2, q["y"], q["v"]); vh = np.interp(1.2, h["y"], h["v"])
    ys = ystar_analytical(q, p)
    return dict(ystar=ys, ystar_slope=ystar_slope(q), ypost=ypost(q, p, ys), v12=vq, G=(vq-vh)/vh,
                itQ=q["it"], errQ=q["err"], itH=h["it"], errH=h["err"])

BASE = dict(r=0.02, mu=0.04, mu_L=0.03, rho=0.12, sigma=0.08, sigma_L=0.03, c=0.20, gamma=0.02,
            a1=0.045, a2=0.05, a3=0.30, kappa=0.01, kappa_p=0.02, pi_cap=None, underline_y=1.02)

