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

Two models are annotated on the graph, never drawn as nodes: the baseline (the model at
asymmetry parameter one, where the shortfall penalty vanishes, M11) and the enhanced model
(asymmetry parameter above one). Each annotation sits on an edge or an attribute, states
what the model says about it or what it stands for, and rests on manuscript rows, results
files or lit/ IDs. Kind "proven" rests on a manuscript row, "measured" on a results file,
"reading" on sources read and our interpretation of them.

    python3 00_reader/ledger_model.py            verify
    python3 00_reader/ledger_model.py --write    verify, then render ledger_model.dot/.pdf and ledger_models.dot/.pdf
"""

import re
import subprocess
import sys
from pathlib import Path


BUNDLE_ROOT = Path(__file__).resolve().parent.parent
MAP_TEX = BUNDLE_ROOT / "MEASUREMENT_MAP.tex"
FIGURE_DOT = BUNDLE_ROOT / "00_reader" / "ledger_model.dot"
MODELS_DOT = BUNDLE_ROOT / "00_reader" / "ledger_models.dot"
MANUSCRIPT_TEX = BUNDLE_ROOT / "MANUSCRIPT.tex"

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


BASELINE, ENHANCED, BOTH, EVIDENCE = "baseline", "enhanced", "both", "evidence"
PROVEN, MEASURED, READING = "proven", "measured", "reading"

ANNOTATIONS = [
    {"id": "R1", "on": "A1", "model": BOTH, "kind": PROVEN, "rests_on": ("M4",),
     "says": "dividends are paid only at the dividend barrier, which rises in the asymmetry parameter",
     "short": "only at the barrier; barrier rises with the asymmetry parameter (M4)"},
    {"id": "R2", "on": "A2", "model": BOTH, "kind": PROVEN, "rests_on": ("M5", "M16"),
     "says": "a recapitalisation starts at the trigger, which rises in the asymmetry parameter, and lands at a target "
             "that does not depend on where it starts",
     "short": "starts at the trigger (rises with the asymmetry parameter, M5); lands at one target (M16)"},
    {"id": "R3", "on": "A3", "model": BOTH, "kind": PROVEN, "rests_on": ("M5", "M16"),
     "says": "as R2: the model does not separate the parent's injection from a sale of stock",
     "short": "as A2: the model does not tell the parent from outside buyers"},
    {"id": "R4", "on": "A1", "model": BOTH, "kind": MEASURED,
     "rests_on": ("comparison/results_facts.md", "comparison/results_pk.md"),
     "says": "F1, dividends paid from a buffer below one sigma over the reference: baseline 90.0%, observed 4.8% "
             "(sample B, 2015-2024)",
     "short": "F1: baseline 90.0%, observed 4.8%"},
    {"id": "R5", "on": "equity", "model": BOTH, "kind": MEASURED,
     "rests_on": ("comparison/results_facts.md", "comparison/results_pk.md", "comparison/results_bound.md"),
     "says": "F2, buffer more than four sigma above the reference: baseline 0.0%, observed 61.6% (2015-2024); "
             "with the minimum moved to the reference, 0.2% against 48.0% (2005-2014)",
     "short": "F2: baseline 0.0%, observed 61.6%; strongest penalty 0.2% vs 48.0%"},
    {"id": "R6", "on": "A2", "model": BOTH, "kind": MEASURED,
     "rests_on": ("comparison/results_facts.md", "comparison/results_pk.md"),
     "says": "F3 and F4, recapitalisations starting thin and landing between one and four sigma: baseline 70.8% and "
             "81.9%, observed 15.2% and 30.4%",
     "short": "F3/F4: baseline 70.8%/81.9%, observed 15.2%/30.4%"},
    {"id": "R7", "on": "A9", "model": ENHANCED, "kind": READING, "rests_on": ("M11", "PCA-Q1", "PCA-Q5", "OCC-Q4"),
     "says": "the shortfall penalty, zero above the reference and rising below it, stands in for the supervisor's "
             "restrictions, which bind only below a capital threshold; the baseline has no counterpart",
     "short": "the shortfall penalty stands in for this: binds only below a threshold"},
    {"id": "R8", "on": "A11", "model": ENHANCED, "kind": READING, "rests_on": ("M11", "ASMT-Q2", "ASMT-D1", "ASMT-Q5"),
     "says": "the insurer's price falls with the leverage ratio until it reaches the floor rate for the bank's rating; "
             "the shortfall penalty copies it below the reference only, so the reward for capital above the reference "
             "is missing",
     "short": "penalty copies this below the reference only; the reward above it (up to a floor rate) is missing"},
    {"id": "R9", "on": "A7", "model": ENHANCED, "kind": READING, "rests_on": ("M11", "OCCL-D1", "FDIC-Q22"),
     "says": "uninsured creditors price and withdraw at every level of capital; as with R8, the shortfall penalty "
             "copies only the part below the reference",
     "short": "penalty copies this below the reference only"},
    {"id": "R10", "on": "A10", "model": BOTH, "kind": READING, "rests_on": ("PCA-Q4", "PCA-Q8", "OCC-Q5"),
     "says": "both models fail a bank at zero equity; the supervisor acts at 2% tangible equity",
     "short": "models fail at zero equity; the law acts at 2%"},
    {"id": "R11", "on": "equity", "model": EVIDENCE, "kind": READING, "rests_on": ("BDF-Q20", "BDF-Q21", "GH-Q12"),
     "says": "banks choose capital targets substantially above the well-capitalized minima, rising with their risk; the "
             "mean Tier 1 buffer is 6.74 points",
     "short": "evidence: targets well above the minima, rising with risk"},
    {"id": "R12", "on": "equity", "model": EVIDENCE, "kind": MEASURED, "rests_on": ("comparison/results_scale.md",),
     "says": "the barrier model matches the share of buffers far above the reference only if the risk guarded against "
             "is two to three times the bank's measured earnings volatility",
     "short": "model needs risk 2-3x measured earnings volatility"},
    {"id": "R13", "on": "A9", "model": EVIDENCE, "kind": READING, "rests_on": ("MW-Q2", "BDF-Q23"),
     "says": "buffers guard against a costly regulatory intervention; adjustment is slower under severe regulatory "
             "pressure",
     "short": "evidence: buffers guard against intervention"},
    {"id": "R14", "on": "A7", "model": EVIDENCE, "kind": READING, "rests_on": ("BDF-Q22", "BAR-Q13", "MW-Q2"),
     "says": "targets reflect counterparties' sensitivity to default risk; debt funding costs fall after an equity issue",
     "short": "evidence: creditors price default risk"},
    {"id": "R15", "on": "A2", "model": EVIDENCE, "kind": READING, "rests_on": ("MW-Q3", "MW-D1", "JM-Q8", "BDF-Q9"),
     "says": "issuance is costly, so banks do not recapitalise after every loss; but issue costs and audits cannot "
             "raise the buffer above the non-issuer barrier",
     "short": "evidence: issuance costly, but cannot raise the buffer above u0"},
]


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
    mrows = set(re.findall(r"\\mrow\{(M[0-9]+)\}", MANUSCRIPT_TEX.read_text()))
    lit_ids = set()
    for f in (BUNDLE_ROOT / "lit").glob("*/CLAIMS.md"):
        lit_ids |= set(re.findall(r"^\|\s*([A-Z0-9]+-[QD][0-9]+)\s*\|", f.read_text(), re.M))
    for a in ANNOTATIONS:
        target_ok = a["on"] in EDGES or a["on"] in NODES
        check(f"LM-15 {a['id']}: annotates an edge or a node, never stands alone", target_ok, a["on"])
        check(f"LM-16 {a['id']}: model and kind are known", a["model"] in (BASELINE, ENHANCED, BOTH, EVIDENCE)
              and a["kind"] in (PROVEN, MEASURED, READING), (a["model"], a["kind"]))
        bad = [r for r in a["rests_on"] if not (r in mrows or r in lit_ids or (BUNDLE_ROOT / r).exists())]
        check(f"LM-17 {a['id']}: everything it rests on resolves (manuscript row, lit/ ID or file)", not bad, bad)
        need = {PROVEN: lambda r: r in mrows, MEASURED: lambda r: r.endswith(".md"), READING: lambda r: r in lit_ids}
        check(f"LM-18 {a['id']}: its kind matches what it rests on", any(need[a["kind"]](r) for r in a["rests_on"]),
              a["kind"])
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


def models_dot():
    lines = ["digraph models {", '  rankdir=LR; node [fontname="Helvetica", fontsize=11]; edge [fontname="Helvetica", fontsize=9];',
             '  labelloc="t"; label="Baseline and enhanced model on the ledger. Black: both models. Blue: enhanced only. '
             'Green: evidence. Dashed boxes: annotations on an entity.";']
    for name, spec in NODES.items():
        shape = "ellipse" if spec["kind"] == AGENT else "box"
        lines.append(f'  "{name}" [shape={shape}, label="{name}"];')
    notes = {}
    for a in ANNOTATIONS:
        notes.setdefault(a["on"], []).append(a)
    for eid, e in EDGES.items():
        parts = [f"{eid} {e['label']}"] + [f"{a['id']}: {a['short']}" for a in notes.get(eid, [])]
        color = "blue" if any(a["model"] == ENHANCED for a in notes.get(eid, [])) else "black"
        if any(a["model"] == EVIDENCE for a in notes.get(eid, [])) and color == "black":
            color = "darkgreen"
        style = "bold" if eid in notes else "solid"
        label = "\\n".join(parts)
        lines.append(f'  "{e["agent"]}" -> "{e["target"]}" [label="{label}", color={color}, fontcolor={color}, style={style}];')
    for node in NODES:
        if node in notes:
            text = "\\n".join(f"{a['id']}: {a['short']}" for a in notes[node])
            lines.append(f'  "{node} notes" [shape=note, style=dashed, fontsize=9, label="{text}"];')
            lines.append(f'  "{node}" -> "{node} notes" [style=dotted, arrowhead=none];')
    lines.append("}")
    return "\n".join(lines) + "\n"


if __name__ == "__main__":
    ok = verify()
    if ok and "--write" in sys.argv:
        for path, text in ((FIGURE_DOT, dot()), (MODELS_DOT, models_dot())):
            path.write_text(text)
            subprocess.run(["dot", "-Tpdf", str(path), "-o", str(path.with_suffix(".pdf"))], check=True)
            print(f"wrote {path.name} and {path.with_suffix('.pdf').name}")
    sys.exit(0 if ok else 1)
