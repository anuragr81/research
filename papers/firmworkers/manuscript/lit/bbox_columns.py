import collections
import html
import re
import subprocess
import sys

WORD = re.compile(r'<word xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" yMax="([\d.]+)">([^<]*)</word>')
PAGE = re.compile(r'<page width="([\d.]+)" height="([\d.]+)">(.*?)</page>', re.S)
BAND = 80


def mode_start(words, width):
    starts = collections.Counter(round(x) for x, _, _, _, _ in words if 0.4 * width <= x <= 0.6 * width)
    return starts.most_common(1)[0][0] if starts else None


def lines_of(words):
    if not words:
        return []
    heights = sorted(y1 - y0 for _, y0, _, y1, _ in words)
    tol = 0.5 * heights[len(heights) // 2]
    lines = []
    for w in sorted(words, key=lambda w: (w[1], w[0])):
        if lines and abs(w[1] - lines[-1][0]) <= tol:
            lines[-1][1].append(w)
        else:
            lines.append((w[1], [w]))
    return [" ".join(t for _, _, _, _, t in sorted(ws)) for _, ws in lines]


def page_text(body, width):
    words = [(float(a), float(b), float(c), float(d), html.unescape(t)) for a, b, c, d, t in WORD.findall(body)]
    page_split = mode_start(words, width)
    if page_split is None:
        return "\n".join(lines_of(words)) + "\n"
    left, right = [], []
    for w in words:
        split = mode_start([v for v in words if abs(v[1] - w[1]) <= BAND], width) or page_split
        (left if w[0] < split - 1.5 else right).append(w)
    return "\n".join(lines_of(left) + [""] + lines_of(right)) + "\n"


def main():
    src, out = sys.argv[1], sys.argv[2]
    xml = subprocess.run(["pdftotext", "-bbox", src, "-"], capture_output=True, text=True).stdout
    pages = [page_text(body, float(w)) for w, _, body in PAGE.findall(xml)]
    with open(out, "w") as fh:
        fh.write("\f".join(pages))


if __name__ == "__main__":
    main()
