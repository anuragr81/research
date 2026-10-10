import hashlib
import os
import pathlib
import re
import shutil
import subprocess
import tempfile
import unicodedata

ROOT = pathlib.Path(__file__).resolve().parent.parent
LEAN_PROJECT = ROOT / "lean"
CACHE = pathlib.Path.home() / ".cache" / "firmworkers"
ALLOWED_AXIOMS = {"propext", "Classical.choice", "Quot.sound"}
LIGATURES = {"ﬀ": "ff", "ﬁ": "fi", "ﬂ": "fl", "ﬃ": "ffi", "ﬄ": "ffl"}


class Suite:
    def __init__(self, title):
        self.results = []
        self.title = title
        print("=" * 72)
        print(title)
        print("=" * 72)

    def check(self, name, ok, detail=""):
        self.results.append(bool(ok))
        print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail else ""))
        return bool(ok)

    def finish(self):
        fails = self.results.count(False)
        print("=" * 72)
        print(f"{self.title.split(' ')[0]} SUMMARY: {len(self.results)} checks, {fails} failures")
        print("=" * 72)
        return 1 if fails else 0


def norm(s):
    s = unicodedata.normalize("NFKD", s)
    for k, v in LIGATURES.items():
        s = s.replace(k, v)
    s = re.sub(r"-\s*\n\s*", "", s).lower()
    s = re.sub(r"[^a-z0-9]+", " ", s)
    return re.sub(r"\s+", " ", s).strip()


def sha256(path):
    return hashlib.sha256(pathlib.Path(path).read_bytes()).hexdigest()


def quotation_rows(claims_text, prefix):
    return re.findall(rf"^\|\s*({prefix}-\d+)\s*\|\s*(\d+)\s*\|\s*\"(.+?)\"\s*\|", claims_text, re.M)


def page_texts(text, first_page):
    return {first_page + i: p for i, p in enumerate(text.split("\f"))}


def lean_audit(suite, tag, rel_source, module, namespace, claims_text, controls):
    src_path = ROOT / rel_source
    src = src_path.read_text()
    lakefile = (LEAN_PROJECT / "lakefile.toml").read_text()
    lib = module.split(".")[0]
    suite.check(f"{tag}-L1 {module} is a build root", f'name = "{lib}"' in lakefile and f'"{lib}.+"' in lakefile)
    suite.check(f"{tag}-L2 no sorry in the source", re.search(r"\bsorry\b", src) is None)
    declared = re.findall(r"^theorem\s+([A-Za-z0-9_']+)", src, re.M)
    cited = sorted(set(re.findall(rf"`{re.escape(namespace)}\.([A-Za-z0-9_']+)`", claims_text)))
    missing = [c for c in cited if c not in declared]
    suite.check(f"{tag}-L3 every Lean name cited in CLAIMS.md is declared", cited and not missing,
                f"{len(cited)} cited, {len(declared)} declared" + (f", missing {missing}" if missing else ""))
    lake = shutil.which("lake") or os.path.expanduser("~/.elan/bin/lake")
    b = subprocess.run([lake, "build", module], cwd=LEAN_PROJECT, capture_output=True, text=True)
    suite.check(f"{tag}-L4 lake build {module} succeeds", b.returncode == 0, (b.stdout + b.stderr).strip()[-300:] if b.returncode else "")
    names = [f"{namespace}.{d}" for d in declared]
    with tempfile.NamedTemporaryFile("w", suffix=".lean", dir=LEAN_PROJECT, delete=False) as fh:
        fh.write(f"import {module}\n" + "".join(f"#print axioms {n}\n" for n in names))
        tmp = fh.name
    pr = subprocess.run([lake, "env", "lean", tmp], cwd=LEAN_PROJECT, capture_output=True, text=True)
    os.unlink(tmp)
    out = pr.stdout + pr.stderr
    used = {}
    for n in names:
        m = re.search(rf"'{re.escape(n)}' (does not depend on any axioms|depends on axioms: \[([^\]]*)\])", out)
        if m:
            used[n] = set() if m.group(2) is None else {a.strip() for a in m.group(2).split(",")}
    suite.check(f"{tag}-L5 the axiom audit covers every declared theorem", len(used) == len(names) and names,
                f"{len(used)} of {len(names)}")
    bad = {n: sorted(a - ALLOWED_AXIOMS) for n, a in used.items() if a - ALLOWED_AXIOMS}
    suite.check(f"{tag}-L6 no axiom outside propext, Classical.choice and Quot.sound", not bad, str(bad) if bad else "")
    absent = [c for c in controls if c not in declared]
    suite.check(f"{tag}-L7 the named controls are present", not absent and all(c.startswith("control_") for c in controls),
                f"missing {absent}" if absent else f"{len(controls)} controls")
    return declared
