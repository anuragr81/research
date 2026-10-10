"""Cross-document citation check for the v2 bundle.

Numbering is read from PROOFS_v2.aux, never reconstructed from prose: a
number written in a supporting document is checked against LaTeX's own
record of what PROOFS_v2 compiled to. Run after every edit to PROOFS_v2.tex.

Three checks, in increasing strength:

  1. a spelled-out citation ("Proposition 7") names a number that exists
     for that kind;
  2. a symbolic reference (\\ref{prop:foo}) names a label that exists;
  3. a spelled-out citation that sits next to a tag in the same sentence
     agrees with that tag's actual number.

Check 1 cannot tell whether a citation points at the RIGHT result, only
that the number is occupied. Check 3 closes part of that gap wherever a
document writes both forms; where a document writes only the number, the
weaker guarantee is all that is available and the summary says so.
"""

import re
import sys
from pathlib import Path

BUNDLE_ROOT = Path(__file__).resolve().parent.parent
PROOFS_AUX = BUNDLE_ROOT / "00_document" / "PROOFS_v2.aux"

CITING_DOCUMENTS = [
    BUNDLE_ROOT / "00_document" / "EMPIRICAL_v2.tex",
    BUNDLE_ROOT / "00_reader" / "PITCH_AND_SUMMARY.md",
    BUNDLE_ROOT / "00_reader" / "README.md",
    BUNDLE_ROOT / "README.md",
]

KIND_WORDS = {
    "prop": ["Proposition", "Prop."],
    "thm": ["Theorem", "Thm."],
    "rem": ["Remark", "Rem."],
    "cor": ["Corollary", "Cor."],
    "lem": ["Lemma", "Lem."],
}

WORD_TO_KIND = {w: k for k, words in KIND_WORDS.items() for w in words}

NUMBERED_KINDS = set(KIND_WORDS)


def labels_from_aux():
    if not PROOFS_AUX.exists():
        print(f"ERROR: {PROOFS_AUX} not found. Run pdflatex on PROOFS_v2.tex first.")
        sys.exit(2)
    text = PROOFS_AUX.read_text()
    mapping = {}
    for label, number, _page in re.findall(
        r"\\newlabel\{([^}]+)\}\{\{([^}]*)\}\{(\d+)\}", text
    ):
        if label.split(":")[0] in NUMBERED_KINDS:
            mapping[label] = number
    return mapping


def numbers_by_kind(mapping):
    grouped = {}
    for label, number in mapping.items():
        grouped.setdefault(label.split(":")[0], set()).add(number)
    return grouped


def main():
    mapping = labels_from_aux()
    grouped = numbers_by_kind(mapping)

    print(f"Loaded {len(mapping)} numbered labels from {PROOFS_AUX.name}")
    for kind in sorted(grouped):
        print(f"  {kind:<6} {sorted(grouped[kind], key=lambda n: [int(p) for p in n.split('.')])}")
    print()

    word_pattern = re.compile(
        r"(" + "|".join(re.escape(w) for w in sorted(WORD_TO_KIND, key=len, reverse=True))
        + r")~?\s*(\d+(?:\.\d+)*)"
    )
    ref_pattern = re.compile(r"\\ref\{([^}]+)\}")

    problems = 0
    checked_documents = 0
    documents_with_symbolic_refs = set()

    for document in CITING_DOCUMENTS:
        if not document.exists():
            continue
        checked_documents += 1
        relative = document.relative_to(BUNDLE_ROOT)
        text = document.read_text()

        for lineno, line in enumerate(text.splitlines(), 1):
            for label in ref_pattern.findall(line):
                if label.split(":")[0] not in NUMBERED_KINDS:
                    continue
                documents_with_symbolic_refs.add(relative)
                if label not in mapping:
                    print(f"[FAIL] {relative}:{lineno}  \\ref{{{label}}} -- no such "
                          f"label in PROOFS_v2.aux")
                    problems += 1

            for word, number in word_pattern.findall(line):
                kind = WORD_TO_KIND[word]
                if number not in grouped.get(kind, set()):
                    print(f"[FAIL] {relative}:{lineno}  '{word} {number}' -- no "
                          f"{kind} carries number {number} in PROOFS_v2")
                    problems += 1
                    continue
                same_line_labels = [
                    l for l in ref_pattern.findall(line)
                    if l.split(":")[0] == kind
                ]
                if len(same_line_labels) == 1:
                    expected = mapping[same_line_labels[0]]
                    if expected != number:
                        print(f"[FAIL] {relative}:{lineno}  '{word} {number}' sits "
                              f"beside \\ref{{{same_line_labels[0]}}}, which is "
                              f"number {expected}")
                        problems += 1

    print()
    print(f"Checked {checked_documents} document(s).")
    if problems:
        print(f"{problems} citation problem(s) above.")
        return 1
    weak_only = [
        d for d in (doc.relative_to(BUNDLE_ROOT) for doc in CITING_DOCUMENTS
                    if doc.exists())
        if d not in documents_with_symbolic_refs
    ]
    print("No citation problems found.")
    if weak_only:
        print("Note: these documents cite by number without a symbolic \\ref, so "
              "only the weaker check applies to them -- a number that exists is "
              "not proof it is the intended result:")
        for d in weak_only:
            print(f"  {d}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
