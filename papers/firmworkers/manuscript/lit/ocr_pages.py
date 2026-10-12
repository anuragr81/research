import sys

from PIL import Image
from rapidocr_onnxruntime import RapidOCR


def lines_of(items):
    lines = []
    for yc, h, x, txt in sorted(items):
        if lines and abs(yc - lines[-1][0]) < 0.5 * max(h, lines[-1][1]):
            lines[-1][2].append((x, txt))
        else:
            lines.append([yc, h, [(x, txt)]])
    return [" ".join(t for _, t in sorted(parts)) for _, _, parts in lines]


def page_lines(engine, path, columns):
    res, _ = engine(path)
    items = []
    for box, txt, _ in res or []:
        ys = [p[1] for p in box]
        xs = [p[0] for p in box]
        items.append(((min(ys) + max(ys)) / 2, max(ys) - min(ys), min(xs), max(xs), txt))
    if not columns:
        return lines_of([(yc, h, x0, t) for yc, h, x0, _, t in items])
    width = Image.open(path).size[0]
    full, left, right = [], [], []
    for yc, h, x0, x1, t in items:
        it = (yc, h, x0, t)
        if x1 - x0 > 0.55 * width:
            full.append(it)
        elif (x0 + x1) / 2 < width / 2:
            left.append(it)
        else:
            right.append(it)
    top = [i for i in full if not left or i[0] < min(l[0] for l in left)]
    rest = [i for i in full if i not in top]
    return lines_of(top) + lines_of(left) + lines_of(right) + lines_of(rest)


def main():
    mode, out_path, pages = sys.argv[1], sys.argv[2], sys.argv[3:]
    engine = RapidOCR()
    with open(out_path, "w") as out:
        for path in pages:
            out.write("\f")
            for line in page_lines(engine, path, mode == "columns"):
                out.write(line + "\n")


if __name__ == "__main__":
    main()
