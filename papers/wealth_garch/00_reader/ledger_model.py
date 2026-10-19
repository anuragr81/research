"""Ledger of the model (MANUSCRIPT.tex): entities, and later actions and regularities.

The ledger is a graph. Its nodes are entities, things in the world that an observer
records: the agents and the objects they act on, each with the attributes that can be
read from it. Its edges are actions, what an agent does to an entity, each leaving a
record. A pattern over the edges is computed from the records and is not a node; a
conclusion is annotated on the edge or attribute it concerns, never drawn as a node.
Every node is a row of the Entities table of MEASUREMENT_MAP.tex with status Report or
Rule, and its name is a term of the map's Terminology table, Agreed or In use. Constructs (the
shortfall penalty, the asymmetry parameter, the discount rates, the value function),
calibrated and unobserved quantities, and the model's outputs (the dividend barrier,
the recapitalisation trigger, the injection target) are not nodes.

This file holds the entities agreed with the author on 19 October 2026. The edges are
proposed next.

    python3 00_reader/ledger_model.py        verify
"""

import re
import sys
from pathlib import Path


BUNDLE_ROOT = Path(__file__).resolve().parent.parent
MAP_TEX = BUNDLE_ROOT / "MEASUREMENT_MAP.tex"

AGENT, OBJECT = "agent", "object"
OBSERVED = ("Report", "Rule")

NODES = {
    "bank":           {"kind": AGENT,  "row": "N1", "attributes": ("CERT", "ASSET", "LIAB", "RWAJT")},
    "equity":         {"kind": OBJECT, "row": "N2", "attributes": ("RBCT1J", "RBCT2")},
    "investors":      {"kind": AGENT,  "row": "N3", "attributes": ("RSSDHCR",)},
    "depositors":     {"kind": AGENT,  "row": "N4", "attributes": ("DEP",)},
    "capital rule":   {"kind": OBJECT, "row": "N5", "attributes": ("CET1 minimum", "Tier 1 minimum",
                                                                   "capital conservation buffer", "leverage floor")},
    "liquidity rule": {"kind": OBJECT, "row": "N6", "attributes": ("liquidity coverage ratio",)},
    "regulator":      {"kind": AGENT,  "row": "N7", "attributes": ()},
}


def cells(macro, tex):
    out = []
    for m in re.finditer(r"\\" + macro + r"\{", tex):
        i, row = m.end() - 1, []
        while i < len(tex) and tex[i] == "{":
            depth, j = 0, i
            while True:
                depth += {"{": 1, "}": -1}.get(tex[j], 0)
                if depth == 0:
                    break
                j += 1
            row.append(tex[i + 1:j])
            i = j + 1
        out.append(row)
    return out


def entity_rows(tex):
    body = tex.split(r"\section{Entities}", 1)[1].split(r"\section{", 1)[0]
    return {r[0]: r for r in cells("entrow", body)}


def terminology(tex):
    return {r[0]: r[3] for r in cells("nrow", tex)}


def verify():
    tex = MAP_TEX.read_text()
    rows, terms = entity_rows(tex), terminology(tex)
    failures = []

    def check(name, ok, detail=""):
        print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail and not ok else ""))
        if not ok:
            failures.append(name)

    for node, spec in NODES.items():
        row = rows.get(spec["row"])
        check(f"LM-1 {node}: row {spec['row']} is in the Entities table", row is not None)
        if row is None:
            continue
        check(f"LM-2 {node}: row {spec['row']} names this entity", row[1] == node, row[1])
        check(f"LM-3 {node}: status is observed (Report or Rule)", row[4] in OBSERVED, row[4])
        check(f"LM-4 {node}: a term of the Terminology table, Agreed or In use", terms.get(node) in ("Agreed", "In use"),
              terms.get(node))
        fields = [a for a in spec["attributes"] if a.isupper()]
        missing = [a for a in fields if a not in row[3]]
        check(f"LM-5 {node}: every report field among its attributes is in the row", not missing, missing)
    extra = sorted(set(rows) - {s["row"] for s in NODES.values()})
    check("LM-6 every Entities row is a node of the ledger", not extra, extra)
    check("LM-7 the ledger has agents and objects", {s["kind"] for s in NODES.values()} == {AGENT, OBJECT})
    print(f"LEDGER: {len(NODES)} nodes, {len(failures)} failures")
    return not failures


if __name__ == "__main__":
    sys.exit(0 if verify() else 1)
