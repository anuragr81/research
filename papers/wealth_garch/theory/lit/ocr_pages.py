import hashlib
import os
import subprocess
import sys
import tempfile

from rapidocr_onnxruntime import RapidOCR

DPI = 200


def main(pdf, out_dir):
    os.makedirs(out_dir, exist_ok=True)
    engine = RapidOCR()
    with tempfile.TemporaryDirectory() as td:
        subprocess.run(["pdftoppm", "-r", str(DPI), "-png", pdf, os.path.join(td, "p")], check=True)
        pages = sorted(f for f in os.listdir(td) if f.endswith(".png"))
        for i, name in enumerate(pages, start=1):
            result, _ = engine(os.path.join(td, name))
            text = "\n".join(r[1] for r in (result or []))
            with open(os.path.join(out_dir, f"page-{i:03d}.txt"), "w") as fh:
                fh.write(text)
            print(f"page {i}: {len(text)} characters", flush=True)
    digest = hashlib.sha256(open(pdf, "rb").read()).hexdigest()
    with open(os.path.join(out_dir, "SOURCE"), "w") as fh:
        fh.write(f"{digest} {DPI}\n")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
