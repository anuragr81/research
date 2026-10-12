import hashlib
import os
import re
import shlex
import subprocess

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
_runs = {}


def code_block(text):
    m = re.search(r"^```\n(.*?)^```", text, re.M | re.S)
    return m.group(1) if m else None


def pin_mismatches(text, here):
    bad = []
    pins = re.findall(r"^Pinned: (\S+) sha256 ([0-9a-f]{64})$", text, re.M)
    for path, digest in pins:
        full = os.path.join(here, path)
        if not os.path.exists(full) or hashlib.sha256(open(full, "rb").read()).hexdigest() != digest:
            bad.append(path)
    return bad if pins else ["(no pins)"]


def run_output(here, command):
    key = (here, command)
    if key not in _runs:
        run = subprocess.run(shlex.split(command), cwd=here, capture_output=True, text=True)
        _runs[key] = (run.returncode, run.stdout, run.stderr)
    return _runs[key]


def reproduce(path, text=None):
    here = os.path.dirname(os.path.join(ROOT, path))
    text = open(os.path.join(ROOT, path)).read() if text is None else text
    m = re.search(r"^Reproduce: (.+)$", text, re.M)
    expected = code_block(text)
    if not m or expected is None:
        return False, "no Reproduce line or no output block"
    bad = pin_mismatches(text, here)
    if bad:
        return False, f"pins differ {bad}"
    code, out, err = run_output(here, m.group(1))
    if code != 0:
        return False, err.strip()[-200:]
    return out == expected, "" if out == expected else "output differs from the recorded block"


def first_commit_time(path):
    out = subprocess.run(["git", "log", "--diff-filter=A", "--format=%ct", "--", path], cwd=ROOT,
                         capture_output=True, text=True).stdout.split()
    return int(out[-1]) if out else None
