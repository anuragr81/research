import math
import sys

from scipy.optimize import fsolve
from scipy.stats import norm


def roots(mu, sigma, rho):
    s = math.sqrt(mu ** 2 + 2 * rho * sigma ** 2)
    return (-mu + s) / sigma ** 2, (-mu - s) / sigma ** 2


def coeffs(p, m):
    return m / (p * m - p ** 2), p / (p * m - m ** 2)


def u0(mu, sigma, rho):
    p, m = roots(mu, sigma, rho)
    return 2 / (p - m) * math.log(-m / p)


def f1(x, u2, mu, sigma, rho):
    p, m = roots(mu, sigma, rho)
    a1, a2 = coeffs(p, m)
    return a1 * math.exp(-p * (u2 - x)) + a2 * math.exp(-m * (u2 - x))


def M(x, u2, mu, sigma, rho, delta, K):
    sd = sigma * math.sqrt(delta)
    b = mu / rho - K - u2
    up = (x + mu * delta) / sd
    dn = (-x + mu * delta) / sd
    first = (x + mu * delta + b) * norm.cdf(up) + sd * norm.pdf(up)
    second = (-x + mu * delta + b) * norm.cdf(dn) + sd * norm.pdf(dn)
    return math.exp(-rho * delta) * (first - math.exp(-2 * mu * x / sigma ** 2) * second)


def deriv(f, x, h=1e-7):
    return (f(x + h) - f(x - h)) / (2 * h)


def barriers(mu, sigma, rho, delta, K, guess=None):
    top = u0(mu, sigma, rho)

    def eqs(v):
        u1, u2 = v
        if not 0 < u1 < u2 < 0.2:
            return [1.0, 1.0]
        g = lambda x: M(x, u2, mu, sigma, rho, delta, K)
        h = lambda x: f1(x, u2, mu, sigma, rho)
        return [g(u1) - h(u1), deriv(g, u1) - deriv(h, u1)]

    def admissible(u1, u2):
        if not 0 < u1 < u2 < top:
            return False
        grid = [u2 * i / 200 for i in range(201)]
        return all(M(x, u2, mu, sigma, rho, delta, K) <= f1(x, u2, mu, sigma, rho) + 1e-12 for x in grid)

    found = []
    for g1 in (0.002, 0.005, 0.008, 0.011, 0.014, 0.017, 0.020):
        for g2 in (0.015, 0.020, 0.025, 0.030, 0.035):
            sol, info, ok, msg = fsolve(eqs, [g1, g2], full_output=True, xtol=1e-12)
            if ok == 1 and max(abs(e) for e in eqs(sol)) < 1e-10 and admissible(*sol):
                if not any(abs(sol[0] - f[0]) < 1e-7 and abs(sol[1] - f[1]) < 1e-7 for f in found):
                    found.append(sol)
    if len(found) != 1:
        return (float("nan"), float("nan")), False, len(found)
    return found[0], True, 1


def survive_moments(x, mu, sigma, delta):
    sd = sigma * math.sqrt(delta)
    up = (x + mu * delta) / sd
    dn = (-x + mu * delta) / sd
    w = math.exp(-2 * mu * x / sigma ** 2)
    prob = norm.cdf(up) - w * norm.cdf(dn)
    first = (x + mu * delta) * norm.cdf(up) + sd * norm.pdf(up)
    second = (-x + mu * delta) * norm.cdf(dn) + sd * norm.pdf(dn)
    return prob, first - w * second


def interval(u1, u2, mu, sigma, delta):
    k = 2 * mu * (u2 - u1) / sigma ** 2
    return (math.exp(k) - 1 - k) * sigma ** 2 / (2 * mu ** 2) + delta


TABLE2 = [(0.08, 0.10, 0.9, 2.4, 1.5, 6, 8), (0.08, 0.25, 0.8, 2.7, 1.8, 12, 18),
          (0.08, 0.50, 0.7, 2.9, 2.2, 19, 36), (0.08, 1.00, 0.6, 3.1, 2.5, 29, 68),
          (0.5, 0.10, 1.6, 3.2, 1.6, 6, 11), (0.5, 0.25, 1.3, 3.4, 2.0, 11, 28),
          (0.5, 0.50, 1.1, 3.5, 2.4, 17, 57), (0.5, 1.00, 0.9, 3.7, 2.8, 27, 122),
          (1.0, 0.10, 1.7, 3.5, 1.8, 5, 16), (1.0, 0.25, 1.4, 3.7, 2.3, 10, 44),
          (1.0, 0.50, 1.1, 3.7, 2.7, 16, 103), (1.0, 1.00, 0.6, 3.8, 3.2, 24, 291)]


def table2(mu, sigma, rho):
    print()
    print("Table 2 (mu = sigma = 1%, rho = 4%): reproduced / printed")
    print("E[s]: conditional expectation u2 - E[X_Delta | survive]; gap: u2 - u1.")
    print("Years: expected time from u2 down to u1, with (+D) and without the delay.")
    print(f"{'Delta':>5} {'K%':>5} | {'u1%':>10} | {'u2%':>10} | {'E[s]%':>5} {'gap%':>5} /pr | {'cost(gap)':>9} | {'yrs+D':>5} {'yrs':>5} /pr")
    tally = {"u1": 0, "u2": 0, "es": 0, "gap": 0, "cost": 0, "yrsD": 0, "yrs": 0}
    for delta, K, pu1, pu2, pes, pcost, pyrs in TABLE2:
        (u1, u2), ok, n = barriers(mu, sigma, rho, delta, K / 100)
        prob, ex = survive_moments(u1, mu, sigma, delta)
        es = u2 - ex / prob
        gap = u2 - u1
        cost = (K / 100) / (K / 100 + gap)
        yrs_d = interval(u1, u2, mu, sigma, delta)
        yrs = yrs_d - delta
        checks = {"u1": round(100 * u1, 1) == pu1, "u2": round(100 * u2, 1) == pu2,
                  "es": round(100 * es, 1) == pes, "gap": round(100 * gap, 1) == pes,
                  "cost": round(100 * cost) == pcost, "yrsD": round(yrs_d) == pyrs, "yrs": round(yrs) == pyrs}
        for k, v in checks.items():
            tally[k] += v
        print(f"{delta:>5} {K:>5.2f} | {100*u1:5.3f}/{pu1:<4} | {100*u2:5.3f}/{pu2:<4} | {100*es:5.2f} {100*gap:5.2f} /{pes:<4}"
              f" | {100*cost:5.1f}/{pcost:<3} | {yrs_d:6.1f} {yrs:6.1f} /{pyrs:<4}{'' if ok else '  (no unique admissible solution)'}")
    print("rows matching the printed value at its precision, of 12: "
          + ", ".join(f"{k} {v}" for k, v in tally.items()))


def main():
    mu, sigma, rho = 0.01, 0.01, 0.04
    print(f"u0 (17) at mu = sigma = 1%, rho = 4%: {100 * u0(mu, sigma, rho):.2f}%   printed 3.80% (Fig. 2)")
    (u1, u2), ok, n = barriers(mu, sigma, rho, 0.5, 0.01)
    print(f"Fig. 2, Delta = 0.5, K = 1%: u1 {100 * u1:.2f}%, u2 {100 * u2:.2f}%, converged {ok}   printed 0.90%, 3.66%")
    table2(mu, sigma, rho)


if __name__ == "__main__":
    main()
