import sys
from collections import Counter

from ownership_split import load_ownership, report


def recode(own):
    holders = Counter((d, h) for (_, d), (h, _) in own.items() if h not in ("", "0"))
    return {(c, d): (h, "1" if h not in ("", "0") and holders[(d, h)] >= 2 else "0")
            for (c, d), (h, _) in own.items()}


def main(panel_path, own_path):
    own = load_ownership(own_path)
    coded = recode(own)
    flagged = [k for k, (_, m) in own.items() if m == "1"]
    agree = sum(coded[k][1] == "1" for k in flagged)
    print(f"HCTMULT = 1 bank-quarters classified M by RSSDHCR counts: {agree} of {len(flagged)}")
    report(panel_path, own_path, coded)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
