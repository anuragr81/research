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

The entities and a first cut of the edges were agreed with the author on 19 October
2026, and revised the same day: a rule is not a node; it enters through the agent who
enforces it (the supervisor, the insurer, the uninsured creditors), and the creditors are
split by deposit insurance. Every edge is a row of the map's Actions table.

    python3 00_reader/ledger_model.py            verify
    python3 00_reader/ledger_model.py --write    verify, then render ledger_model.dot and ledger_model.pdf
"""

import re
import subprocess
import sys
from pathlib import Path


BUNDLE_ROOT = Path(__file__).resolve().parent.parent
MAP_TEX = BUNDLE_ROOT / "MEASUREMENT_MAP.tex"
FIGURE_DOT = BUNDLE_ROOT / "00_reader" / "ledger_model.dot"

AGENT, OBJECT = "agent", "object"
OBSERVED = ("Report", "Rule")

NODES = {
    "bank":                {"kind": AGENT,  "row": "N1", "attributes": ("CERT", "ASSET", "LIAB", "RWAJT")},
    "equity":              {"kind": OBJECT, "row": "N2", "attributes": ("RBCT1J", "RBCT2")},
    "investors":           {"kind": AGENT,  "row": "N3", "attributes": ("RSSDHCR",)},
    "insured creditors":   {"kind": AGENT,  "row": "N4", "attributes": ("DEPINS",)},
    "uninsured creditors": {"kind": AGENT,  "row": "N5", "attributes": ("VOLIAB", "DEPUNA")},
    "supervisor":          {"kind": AGENT,  "row": "N6", "attributes": ("REGAGNT",)},
    "insurer":             {"kind": AGENT,  "row": "N7", "attributes": ()},
}

EDGES = {
    "A1": {"agent": "bank", "target": "investors", "label": "pays dividends", "records": ("EQCDIV",)},
    "A2": {"agent": "investors", "target": "equity", "label": "buy stock, net", "records": ("EQCSXQ",)},
    "A3": {"agent": "investors", "target": "equity", "label": "inject capital (parent)", "records": ("EQCBHCTR",)},
    "A4": {"agent": "insured creditors", "target": "bank", "label": "deposit and withdraw", "records": ("DEPINS",)},
    "A5": {"agent": "bank", "target": "bank", "label": "takes risky exposure", "records": ("RWAJT", "ASSET")},
    "A6": {"agent": "supervisor", "target": "bank", "label": "supervises", "records": ("REGAGNT",)},
    "A7": {"agent": "uninsured creditors", "target": "bank", "label": "lend and withdraw", "records": ("VOLIAB", "DEPUNA")},
    "A8": {"agent": "insurer", "target": "insured creditors", "label": "insures", "records": ("DEPINS",)},
    "A9": {"agent": "supervisor", "target": "bank", "label": "assigns capital category, restricts", "records": ("RBCT1J", "RWAJT")},
    "A10": {"agent": "supervisor", "target": "bank", "label": "closes", "records": ()},
    "A11": {"agent": "insurer", "target": "bank", "label": "prices insurance on capital", "records": ()},
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


def action_rows(tex):
    body = tex.split(r"\section{Actions}", 1)[1].split(r"\section{", 1)[0]
    return {r[0]: r for r in cells("actrow", body)}


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
    actions = action_rows(tex)
    for eid, e in EDGES.items():
        row = actions.get(eid)
        check(f"LM-8 {eid}: row in the Actions table", row is not None)
        if row is None:
            continue
        check(f"LM-9 {eid}: the agent is an agent node and the target a node",
              NODES.get(e["agent"], {}).get("kind") == AGENT and e["target"] in NODES, (e["agent"], e["target"]))
        check(f"LM-10 {eid}: the row names its agent and its target", e["agent"] in row[1] and e["target"] in row[1], row[1])
        check(f"LM-11 {eid}: status is Report or Rule", row[4] in OBSERVED, row[4])
        missing = [f for f in e["records"] if f not in row[3]]
        check(f"LM-12 {eid}: every record field is in the row", not missing, missing)
    check("LM-13 every Actions row is an edge of the ledger", set(actions) == set(EDGES), sorted(set(actions) ^ set(EDGES)))
    extra = sorted(set(rows) - {s["row"] for s in NODES.values()})
    check("LM-6 every Entities row is a node of the ledger", not extra, extra)
    check("LM-7 the ledger has agents and objects", {s["kind"] for s in NODES.values()} == {AGENT, OBJECT})
    print(f"LEDGER: {len(NODES)} nodes, {len(EDGES)} edges, {len(failures)} failures")
    return not failures


def dot():
    lines = ["digraph ledger {", '  rankdir=LR; node [fontname="Helvetica", fontsize=11]; edge [fontname="Helvetica", fontsize=9];']
    for name, spec in NODES.items():
        shape = "ellipse" if spec["kind"] == AGENT else "box"
        attrs = "\\n".join(spec["attributes"])
        label = f"{name}\\n({spec['row']})" + (f"\\n{attrs}" if attrs else "")
        lines.append(f'  "{name}" [shape={shape}, label="{label}"];')
    for eid, e in EDGES.items():
        lines.append(f'  "{e["agent"]}" -> "{e["target"]}" [label="{eid} {e["label"]}\\n{", ".join(e["records"])}"];')
    lines.append("}")
    return "\n".join(lines) + "\n"


if __name__ == "__main__":
    ok = verify()
    if ok and "--write" in sys.argv:
        FIGURE_DOT.write_text(dot())
        subprocess.run(["dot", "-Tpdf", str(FIGURE_DOT), "-o", str(FIGURE_DOT.with_suffix(".pdf"))], check=True)
        print(f"wrote {FIGURE_DOT.name} and {FIGURE_DOT.with_suffix('.pdf').name}")
    sys.exit(0 if ok else 1)
