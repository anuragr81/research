import numpy as np

from mmp_model import NEG, gauss_hermite_lognormal, tauchen_common

TABLE2 = {
    "small": {"w_mean": (0.08, 0.13), "w_ar": (0.14, 0.20), "w_sd": (0.10, 0.20), "g_mean": (0.90, 0.65),
              "g_sd": (3.48, 3.50), "rD": (-0.51, -0.34), "loan_spread": (1.87, 1.72), "liquid_spread": (0.92, 0.42)},
    "large": {"w_mean": (0.22, 0.37), "w_ar": (0.51, 0.70), "w_sd": (0.14, 0.28), "g_mean": (1.64, 1.60),
              "g_sd": (5.41, 5.31), "rD": (-0.58, -0.48), "loan_spread": (1.89, 1.68), "liquid_spread": (1.07, 0.56)},
}
TABLE4 = {
    "large": {"phiF": 0.0092, "phiE": 0.0705, "beta": 0.9753, "gamma": 1.3023, "c": 0.0110, "phiN": 0.4352, "cD": 2e-5},
    "small": {"phiF": 0.0803, "phiE": 0.0072, "beta": 0.9859, "gamma": 1.9054, "c": 0.0096, "phiN": 0.9005, "cD": 4e-5},
}
THETA = {"large": 0.06, "small": 0.08}
PB = np.array([[1 - 1 / 20, 1 / 20], [1 / 8, 1 - 1 / 8]])
TAU, PSI, LW, LU, OML, OMS = 0.15, 1.3, 16.66, 33.33, 1.0, 0.2


def model(size, ne=41, nl=41, nw=15, ng=5, nq=(15, 15, 10), nm_iters=60, nmax=0.5, fmax=1.5):
    t2, t4 = TABLE2[size], TABLE4[size]
    wg, Pw = tauchen_common([m / 100 for m in t2["w_mean"]], list(t2["w_ar"]), [s / 100 for s in t2["w_sd"]], nw)
    gn, gw = gauss_hermite_lognormal([m / 100 for m in t2["g_mean"]], [s / 100 for s in t2["g_sd"]], ng)
    rD = np.array(t2["rD"]) / 100
    gamma, beta = t4["gamma"], t4["beta"]
    vd = t4["cD"] ** (1 - gamma) / (1 - gamma) / (1 - beta)
    par = np.array([beta, gamma, TAU, THETA[size], t4["phiN"], PSI, t4["phiF"], t4["phiE"], t4["c"], LW, LU, OML, OMS, vd])
    return {"eg": np.linspace(0.0, 0.4, ne), "lg": np.linspace(0.0, 2.5, nl), "wg": wg, "Pw": Pw, "Pb": PB,
            "gn": gn, "gw": gw, "rD": rD, "rL": rD + np.array(t2["loan_spread"]) / 100,
            "rS": rD + np.array(t2["liquid_spread"]) / 100, "par": par,
            "qx": np.linspace(0.02, 0.98, nq[0]) ** 2, "qn": np.linspace(0.0, 1.0, nq[1]), "qf": np.linspace(0.0, 1.0, nq[2]),
            "nmax": nmax, "fmax": fmax, "fix_nf": False, "nm_iters": nm_iters}


def cake(gamma, beta, r):
    par = np.array([beta, gamma, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1e9, 1e9, 1.0, 1.0, NEG])
    return {"eg": np.linspace(0.0, 0.4, 81), "lg": np.array([0.0, 1.0]), "wg": np.array([0.0]),
            "Pw": np.ones((1, 1, 1)), "Pb": np.ones((1, 1)), "gn": np.ones((1, 1)), "gw": np.ones(1),
            "rD": np.array([r]), "rL": np.zeros(1), "rS": np.array([r]), "par": par,
            "qx": np.linspace(0.001, 0.999, 400), "qn": np.zeros(1), "qf": np.zeros(1), "nmax": 0.0, "fmax": 0.0,
            "fix_nf": True, "nm_iters": 80}
