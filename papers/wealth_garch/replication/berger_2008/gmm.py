import re

import numpy as np


def term(spec):
    m = re.fullmatch(r"L(\d+)\.(\w+)", spec)
    return (m.group(2), int(m.group(1))) if m else (spec, 0)


def system_gmm(df, y, regressors, gmm, iv, level=True, constant=True, collapse=False, idcol="id", tcol="year"):
    ids = sorted(df[idcol].unique())
    times = sorted(df[tcol].unique())
    T, N = len(times), len(ids)
    pos = {t: k for k, t in enumerate(times)}
    names = sorted({y} | {term(r)[0] for r in regressors} | {v for v, _, _ in gmm} | {term(u)[0] for u in iv})
    cube = np.full((N, T, len(names)), np.nan)
    col = {n: k for k, n in enumerate(names)}
    rowid = {i: k for k, i in enumerate(ids)}
    for rec in df[[idcol, tcol] + names].itertuples(index=False):
        cube[rowid[rec[0]], pos[rec[1]], :] = rec[2:]

    def at(name, lag, t):
        s = t - lag
        return cube[:, s, col[name]] if s >= 0 else np.full(N, np.nan)

    max_lag = max(term(r)[1] for r in list(regressors) + list(iv))
    first_level = max([max_lag] + [a - 1 for _, a, _ in gmm]) if level else max_lag
    first_diff = first_level + 1 if level else max_lag + 1
    diff_t = list(range(first_diff, T))
    level_t = list(range(first_level, T)) if level else []
    rows_t = [("d", t) for t in diff_t] + [("l", t) for t in level_t]
    R = len(rows_t)
    k = len(regressors) + (1 if constant and level else 0)

    instr = []
    for v, a, b in gmm:
        if collapse:
            for lag in range(a, min(b, diff_t[-1]) + 1):
                instr.append(("dgmm", v, None, lag))
        else:
            for t in diff_t:
                for lag in range(a, min(b, t) + 1):
                    instr.append(("dgmm", v, t, lag))
    if level:
        for v, a, _ in gmm:
            if collapse:
                instr.append(("lgmm", v, None, a - 1))
            else:
                for t in level_t:
                    if t - (a - 1) >= 1:
                        instr.append(("lgmm", v, t, a - 1))
    for u in iv:
        instr.append(("iv", u, None, None))
    if constant and level:
        instr.append(("cons", None, None, None))
    m = len(instr)

    Y = np.zeros((N, R))
    X = np.zeros((N, R, k))
    Z = np.zeros((N, R, m))
    valid = np.zeros((N, R), dtype=bool)
    for r, (eq, t) in enumerate(rows_t):
        if eq == "d":
            yv = at(y, 0, t) - at(y, 0, t - 1)
            xs = [at(*term(s), t) - at(term(s)[0], term(s)[1], t - 1) for s in regressors]
            if constant and level:
                xs.append(np.zeros(N))
        else:
            yv = at(y, 0, t)
            xs = [at(*term(s), t) for s in regressors]
            if constant and level:
                xs.append(np.ones(N))
        xs = np.column_stack(xs)
        ok = ~np.isnan(yv) & ~np.isnan(xs).any(axis=1)
        valid[:, r] = ok
        Y[:, r] = np.where(ok, yv, 0.0)
        X[:, r, :] = np.where(ok[:, None], xs, 0.0)
        for j, (kind, v, tt, lag) in enumerate(instr):
            if kind == "dgmm" and eq == "d" and tt in (t, None):
                z = at(v, lag, t)
            elif kind == "lgmm" and eq == "l" and tt in (t, None) and t - lag >= 1:
                z = at(v, lag, t) - at(v, lag + 1, t)
            elif kind == "iv":
                z = at(*term(v), t) - at(term(v)[0], term(v)[1], t - 1) if eq == "d" else at(*term(v), t)
            elif kind == "cons" and eq == "l":
                z = np.ones(N)
            else:
                continue
            Z[:, r, j] = np.where(ok & ~np.isnan(z), z, 0.0)

    H = np.zeros((R, R))
    for r1, (e1, t1) in enumerate(rows_t):
        for r2, (e2, t2) in enumerate(rows_t):
            if e1 == "d" and e2 == "d":
                H[r1, r2] = 2.0 if t1 == t2 else (-1.0 if abs(t1 - t2) == 1 else 0.0)
            elif e1 == "l" and e2 == "l":
                H[r1, r2] = 1.0 if t1 == t2 else 0.0
            else:
                td, tl = (t1, t2) if e1 == "d" else (t2, t1)
                H[r1, r2] = 1.0 if tl == td else (-1.0 if tl == td - 1 else 0.0)

    ZX = np.einsum("nrm,nrk->mk", Z, X)
    ZY = np.einsum("nrm,nr->m", Z, Y)
    W1 = np.linalg.pinv(np.einsum("nrm,rs,nsq->mq", Z, H, Z))

    def solve(W):
        A = ZX.T @ W @ ZX
        return np.linalg.solve(A, ZX.T @ W @ ZY), np.linalg.inv(A)

    def scores(beta):
        u = (Y - np.einsum("nrk,k->nr", X, beta)) * valid
        return u, np.einsum("nrm,nr->nm", Z, u)

    b1, A1inv = solve(W1)
    u1, g1 = scores(b1)
    S1 = g1.T @ g1
    V1 = A1inv @ ZX.T @ W1 @ S1 @ W1 @ ZX @ A1inv
    W2 = np.linalg.pinv(S1)
    b2, V2 = solve(W2)
    u2, g2 = scores(b2)
    D = np.zeros((k, k))
    for c in range(k):
        xc = X[:, :, c]
        dS = -(np.einsum("nrm,nr->nm", Z, xc).T @ g1 + g1.T @ np.einsum("nrm,nr->nm", Z, xc))
        D[:, c] = -V2 @ ZX.T @ W2 @ dS @ W2 @ g2.sum(axis=0)
    Vc = V2 + D @ V2 + V2 @ D.T + D @ V1 @ D.T
    hansen = float(g2.sum(axis=0) @ W2 @ g2.sum(axis=0))
    labels = list(regressors) + (["_cons"] if constant and level else [])
    return {"labels": labels, "b1": b1, "se1": np.sqrt(np.diag(V1)), "b2": b2, "se2": np.sqrt(np.diag(Vc)),
            "nobs": int(valid[:, :len(diff_t)].sum()), "ninst": m, "ngroups": N,
            "hansen": hansen, "df": m - k}
