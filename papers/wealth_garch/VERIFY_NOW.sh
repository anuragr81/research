#!/usr/bin/env bash
# VERIFY_NOW.sh -- one-shot verification. Run from the repo root:
#
#     bash VERIFY_NOW.sh
#
# Writes everything to verification_output.txt AND prints it. Send that one
# file back; it is self-contained.
#
# Nothing here re-solves anything: it reads the existing .mat files. Expect
# a few minutes, most of it Octave startup (once per lambda_S).

set -u
OUT="verification_output.txt"
: > "$OUT"
exec > >(tee "$OUT") 2>&1

hr () { printf '=%.0s' {1..78}; echo; }

hr
echo "VERIFICATION REPORT"
echo "date (UTC): $(date -u '+%Y-%m-%d %H:%M:%S')"
echo "host      : $(uname -srm)"
echo "cwd       : $(pwd)"
hr

echo
echo "### 0. ENVIRONMENT"
echo "python3 : $(python3 --version 2>&1)"
for m in sympy numpy scipy; do
  echo -n "  $m: "
  python3 -c "import $m; print($m.__version__)" 2>/dev/null || echo "NOT INSTALLED"
done
echo -n "octave  : "
if command -v octave >/dev/null 2>&1; then octave --version 2>/dev/null | head -1; else echo "NOT ON PATH"; fi
echo -n "pdflatex: "
if command -v pdflatex >/dev/null 2>&1; then pdflatex --version 2>/dev/null | head -1; else echo "NOT ON PATH"; fi
echo -n "git     : "
if git rev-parse --short HEAD >/dev/null 2>&1; then
  echo "HEAD=$(git rev-parse --short HEAD)  branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null)"
  echo "  --- files differing from HEAD (the overlay) ---"
  git status --porcelain | sed 's/^/    /'
else
  echo "not a git repo (or git unavailable)"
fi

echo
echo "### 1. SOLVE DATA PRESENT"
MATDIR="${MATDATA_DIR:-03_empirical/results}"
echo "MATDATA_DIR = $MATDIR"
ls -1 "$MATDIR"/*.mat 2>/dev/null | sed 's/^/    /' || echo "    (no .mat files found)"
echo "  K-sweep files (resultK_*):  $(ls -1 "$MATDIR"/resultK_* 2>/dev/null | wc -l)"
echo "  H-convention files (resultH_*): $(ls -1 "$MATDIR"/resultH_* 2>/dev/null | wc -l)"

echo
echo "### 2. DOCUMENT FIGURE REGENERATION (full detail)"
echo "Every number quoted in the document, recomputed from the .mat files."
hr
timeout 3600 python3 01_theory/verify_document_figures.py | tee /tmp/_figs.out
FIGRC=${PIPESTATUS[0]}
FIGPASS=$(grep -cE '^F[0-9]+[a-z]?[[:space:]]+PASS' /tmp/_figs.out || true)
FIGFAIL=$(grep -cE '^F[0-9]+[a-z]?[[:space:]]+FAIL' /tmp/_figs.out || true)
FIGSKIP=$(grep -cE '^F[0-9]+[a-z]?[[:space:]]+SKIP' /tmp/_figs.out || true)
hr
echo "verify_document_figures.py exit code: $FIGRC"

echo
echo "### 3. FULL HARNESS (04_reproduce/run_all.sh)"
hr
timeout 3600 bash 04_reproduce/run_all.sh
HARNESSRC=$?
hr
echo "run_all.sh exit code: $HARNESSRC"

echo
echo "### 4. DOCUMENT COMPILES, CROSS-REFERENCES RESOLVE"
if command -v pdflatex >/dev/null 2>&1; then
  ( pdflatex -interaction=nonstopmode MANUSCRIPT.tex >/tmp/_v2a.log 2>&1 \
    && bibtex MANUSCRIPT >/dev/null 2>&1 \
    && pdflatex -interaction=nonstopmode MANUSCRIPT.tex >/tmp/_v2a.log 2>&1 \
    && pdflatex -interaction=nonstopmode MANUSCRIPT.tex >/tmp/_v2b.log 2>&1 )
  echo "  LaTeX errors      : $(grep -cE '^!' /tmp/_v2b.log)"
  echo "  undefined refs    : $(grep -ci 'undefined' /tmp/_v2b.log)"
  echo "  pages             : $(grep -o 'Output written.*' /tmp/_v2b.log | head -1)"
  grep -E '^!' /tmp/_v2b.log | head -5 | sed 's/^/    /'
else
  echo "  SKIP: pdflatex not on PATH"
fi

echo
echo "### 5. STRUCTURAL CHECKS"
python3 - <<'PY'
import re, os
p = 'MANUSCRIPT.tex'
if not os.path.exists(p):
    print("  FAIL: MANUSCRIPT.tex not found"); raise SystemExit(1)
s = open(p).read()
bad = False
for env in ("proposition","lemma","corollary","remark","theorem","proof","equation","enumerate"):
    o, c = len(re.findall(r'\\begin\{%s\}'%env, s)), len(re.findall(r'\\end\{%s\}'%env, s))
    if o != c:
        print(f"  MISMATCH {env}: {o} begin vs {c} end"); bad = True
codes = [m[1] for m in re.findall(r'\\begin\{(proposition|theorem|corollary|remark|lemma)\}\[([A-Z]{3,4}):', s)]
dups = sorted({c for c in codes if codes.count(c) > 1})
print(f"  environments balanced : {'yes' if not bad else 'NO'}")
print(f"  numbered results      : {len(codes)}")
print(f"  duplicate codes       : {dups or 'none'}")
PY

echo
echo "### 6. BIBLIOGRAPHY COPIES IN SYNC"
found=$(find . -name references.bib 2>/dev/null)
if [ -n "$found" ]; then
  echo "$found" | while read -r f; do echo "    $(md5sum "$f")"; done
  n=$(echo "$found" | while read -r f; do md5sum "$f" | cut -d' ' -f1; done | sort -u | wc -l)
  echo "  distinct hashes: $n  ($([ "$n" -eq 1 ] && echo 'in sync' || echo 'OUT OF SYNC'))"
else
  echo "    (no references.bib in repo tree)"
fi

echo
hr
echo "SUMMARY"
echo "  document figures : $FIGPASS passed, $FIGFAIL failed, $FIGSKIP skipped   (exit $FIGRC)"
if [ "${FIGPASS:-0}" -eq 0 ]; then
  echo "                     ^^ WARNING: ZERO figures actually checked."
  echo "                        A clean exit here verifies NOTHING. Check section 2."
fi
echo "  full harness     : exit $HARNESSRC   (0 = no unexpected FAIL)"
echo "  report written to: $OUT"
hr
