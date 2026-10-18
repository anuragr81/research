import numpy as np

from mmp_a1 import keep_better
from mmp_model import NEG, evaluate, optimise


def warm_start(model, V, pol):
    vd = model["par"][13]
    args = (model["eg"], model["lg"], model["wg"], model["Pw"], model["Pb"], model["gn"], model["gw"],
            model["rD"], model["rL"], model["rS"], model["par"])
    V1 = np.maximum(V, vd)
    k = max(0.0, float(np.max(V1 - evaluate(V1, pol, *args)))) / (1 - model["par"][0])
    return np.maximum(V1 - k, vd), pol


def solve_fast(model, rel=1e-6, howard=50, max_outer=400, start=None, log=print):
    vd = model["par"][13]
    failure = vd > NEG / 10
    args = (model["eg"], model["lg"], model["wg"], model["Pw"], model["Pb"], model["gn"], model["gw"],
            model["rD"], model["rL"], model["rS"], model["par"])
    if start is None:
        V = np.full((len(model["eg"]), len(model["lg"]), len(model["wg"]), model["Pb"].shape[0]), vd if failure else 0.0)
        prev = None
    else:
        V, prev = warm_start(model, *start)
    fall, full_count, check = 0.0, 0, False
    for outer in range(max_outer):
        full = prev is None or outer < 3 or outer % 10 == 0 or check
        if full:
            Vn, pol = optimise(V, *args, model["qx"], model["qn"], model["qf"], model["nmax"], model["fmax"],
                               model["fix_nf"], model["nm_iters"])
            full_count += 1
            if prev is not None:
                Vn, pol = keep_better(Vn, pol, prev, V, *args, model["nmax"], model["fmax"], model["fix_nf"],
                                      model["nm_iters"])
        else:
            Vn, pol = keep_better(np.full_like(V, -np.inf), prev.copy(), prev, V, *args, model["nmax"], model["fmax"],
                                  model["fix_nf"], model["nm_iters"])
        for _ in range(howard):
            Vn = evaluate(Vn, pol, *args)
        live = (Vn > NEG / 10) | (V > NEG / 10)
        finite = Vn[Vn > NEG / 10]
        span = float(finite.max() - (vd if failure else finite.min()))
        diff = float(np.max(np.abs(Vn - V)[live]))
        if outer > 0:
            fall = max(fall, float(np.max((V - Vn)[live])) / span)
        V, prev = Vn, pol
        log(f"outer {outer} ({'full' if full else 'light'}): max change {diff:.3e} (tolerance {rel * span:.3e})")
        if diff <= rel * span:
            if full:
                return V, pol, outer + 1, diff, True, fall, full_count
            check = True
        else:
            check = False
    return V, pol, max_outer, diff, False, fall, full_count
