import os
import subprocess
import sys
import tempfile

import numpy as np
from PIL import Image

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from stage1 import sha

ZERO_ROW, ROW_250 = 1047, 576
X_1990, PX_PER_YEAR = 796, 32.4
POINTS = 34


def main(pdf):
    print(f"{os.path.basename(pdf)} sha256 {sha(pdf)}")
    with tempfile.TemporaryDirectory() as tmp:
        subprocess.run(["pdftoppm", "-f", "11", "-l", "11", "-r", "300", "-png", pdf, os.path.join(tmp, "p")], check=True)
        name = [f for f in os.listdir(tmp) if f.endswith(".png")][0]
        im = np.array(Image.open(os.path.join(tmp, name)).convert("RGB")).astype(int)
    per = (ZERO_ROW - ROW_250) / 250
    print(f"Chart 2 top panel, PDF page 11 at 300 dpi: zero at row {ZERO_ROW}, 250 at row {ROW_250}; "
          f"1990 at x {X_1990}, {PX_PER_YEAR} px a year; {POINTS} points")
    total = 0
    for k in range(POINTS):
        x = min(max(int(round(X_1990 + PX_PER_YEAR * k)), 800), 1862)
        lum = im[540:ZERO_ROW, x - 1:x + 2].mean(axis=(1, 2))
        top = next(i for i in range(len(lum)) if lum[i] < 185) + 540
        count = (ZERO_ROW - top) / per
        total += count
        print(f"{1990 + k} reading {count:6.1f} nearest 5: {5 * round(count / 5)}")
    print(f"sum of readings {total:.0f}")


if __name__ == "__main__":
    main(sys.argv[1])
