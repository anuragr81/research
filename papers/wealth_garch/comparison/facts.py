import hashlib

from sample import ANNUAL, QUARTERLY, QUARTERS, R_REF, RATES, qflow, ratio, samples, shift

THIN, FAR = 1.0, 4.0


def observed(banks, q):
    f1, f2, eps = [], [], []
    for b in banks:
        s, sig = q.get(b["cert"], {}), b["sigma"]
        for d in QUARTERS:
            r_now, r_pre = ratio(q, b["cert"], d), ratio(q, b["cert"], shift(d, -1))
            now = None if r_now is None else r_now - R_REF
            pre = None if r_pre is None else r_pre - R_REF
            if now is not None:
                f2.append(now > FAR * sig)
            div = qflow(s, d, "EQCDIV")
            if pre is not None and now is not None and div is not None and div > 0:
                f1.append(pre < THIN * sig)
            if pre is not None and now is not None:
                stock, parent, merger = s[d]["EQCSXQ"], qflow(s, d, "EQCBHCTR"), qflow(s, d, "EQCMRG")
                if stock is not None and parent is not None and merger is not None and merger == 0 \
                        and stock + parent >= 0.005 * s[d]["ASSET"]:
                    eps.append((pre < THIN * sig, THIN * sig <= now <= FAR * sig))
    return {"F1": (sum(f1), len(f1)), "F2": (sum(f2), len(f2)),
            "F3": (sum(e[0] for e in eps), len(eps)), "F4": (sum(e[1] for e in eps), len(eps))}


def show(label, res):
    for k, (hit, n) in res.items():
        share = f"{100 * hit / n:.1f}%" if n else "n/a"
        print(f"  {k}: {hit}/{n} = {share}")


def main():
    for p in [ANNUAL, RATES] + QUARTERLY:
        print(f"{p.split('/')[-1]} sha256 {hashlib.sha256(open(p, 'rb').read()).hexdigest()}")
    samp, q = samples()
    for name in ("A", "B"):
        print(f"\nsample {name}: {len(samp[name])} banks")
        show(name, observed(samp[name], q))


if __name__ == "__main__":
    main()
