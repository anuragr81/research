"""Ledger of the theory paper (theory/MANUSCRIPT.tex): entities, actions, annotations.

The ledger follows the bundle's spec, 00_reader/ledger_model.py and the Entities
and Actions tables of MEASUREMENT_MAP.tex. Its nodes are entities, the agents and
the objects they act on, each a row of the map's Entities table with status Report
or Rule and named by a term of the map's Terminology table, Agreed or In use. Its
edges are actions, rows of the map's Actions table. A rule is not a node; it enters
through the agent who enforces it. Constructs (the shortfall penalty, the asymmetry
parameter, the value function) and the model's outputs (the dividend barrier, the
recapitalisation trigger, the injection target) are not nodes. A result of the
theory paper is an annotation on the edge or node it concerns, resting on rows of
theory/MANUSCRIPT.tex.

The theory ledger is a subgraph of the bundle's: every node and edge here is one of
00_reader/ledger_model.py with the same row, kind, agent and target, and every map
row left out is listed in EXCLUDED with its reason. Edge status follows Paper A
(papers/subjective_racism/paper_a/notes/ledger_diagram.md): settled when a theory
row proves the action, given when the model takes it as a rule or an input, open
when no row, assumption or map object carries it.

The node names and the edges are a proposal (theory/notes/ledger.md), not yet
agreed with the author.

    python3 theory/checks/ledger_theory.py
"""

import importlib.util
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
THEORY = HERE.parent
BUNDLE = THEORY.parent
MANUSCRIPT = THEORY / "MANUSCRIPT.tex"

spec = importlib.util.spec_from_file_location("ledger_model", BUNDLE / "00_reader" / "ledger_model.py")
LM = importlib.util.module_from_spec(spec)
spec.loader.exec_module(LM)

AGENT, OBJECT = LM.AGENT, LM.OBJECT
SETTLED, GIVEN, OPEN = "settled", "given", "open"
PROVEN, READING = "proven", "reading"

NODES = {
    "bank":                {"kind": AGENT,  "row": "N1", "players": ("P1",)},
    "equity":              {"kind": OBJECT, "row": "N2", "players": ()},
    "investors":           {"kind": AGENT,  "row": "N3", "players": ("P2",)},
    "insured creditors":   {"kind": AGENT,  "row": "N4", "players": ("P3",)},
    "uninsured creditors": {"kind": AGENT,  "row": "N5", "players": ("P3",)},
    "supervisor":          {"kind": AGENT,  "row": "N6", "players": ("P4",)},
}

EDGES = {
    "A1": {"agent": "bank", "target": "investors", "status": SETTLED, "rests_on": ("M1",)},
    "A2": {"agent": "investors", "target": "equity", "status": SETTLED, "rests_on": ("M2", "A5")},
    "A3": {"agent": "investors", "target": "equity", "status": SETTLED, "rests_on": ("M2", "A5")},
    "A4": {"agent": "insured creditors", "target": "bank", "status": GIVEN, "rests_on": ("A1",)},
    "A5": {"agent": "bank", "target": "bank", "status": GIVEN, "rests_on": ("A2",)},
    "A6": {"agent": "supervisor", "target": "bank", "status": GIVEN, "rests_on": ("A2",)},
    "A7": {"agent": "uninsured creditors", "target": "bank", "status": GIVEN, "rests_on": ("A1",)},
    "A9": {"agent": "supervisor", "target": "bank", "status": OPEN, "rests_on": ()},
}

EXCLUDED = {
    "N7": "the insurer is not in the model as stated, and no theory row bears on it",
    "A8": "an action of the insurer, which is outside the theory ledger",
    "A10": "closure at the distress boundary; no theory row (the parent's drift row at the boundary is not carried)",
    "A11": "an action of the insurer, which is outside the theory ledger",
}

ANNOTATIONS = [
    {"id": "T1", "on": ("A1",), "kind": PROVEN, "rests_on": ("M1",),
     "says": "dividends are paid only at the dividend barrier, which rises in the asymmetry parameter and in the fixed issuance cost"},
    {"id": "T2", "on": ("A2", "A3"), "kind": PROVEN, "rests_on": ("M2", "M7"),
     "says": "an issue starts at the trigger, which rises in the asymmetry parameter and falls in the fixed issuance cost, "
             "and lands at a target that does not depend on where it starts and rises in the fixed issuance cost; "
             "the model does not tell the parent from outside buyers"},
    {"id": "T3", "on": ("A2", "A3"), "kind": PROVEN, "rests_on": ("M4", "M5", "M6"),
     "says": "the value has no kink at the trigger and is convex just above it, and with a positive fixed issuance cost "
             "the discounted number of issues is finite"},
    {"id": "T4", "on": ("A1", "A2"), "kind": PROVEN, "rests_on": ("M3", "K1", "C1"),
     "says": "where dividends are paid and where issues start identify the asymmetry parameter and the fixed issuance cost, "
             "because the two parameters move the trigger in opposite directions"},
    {"id": "T5", "on": ("equity",), "kind": PROVEN, "rests_on": ("M9",),
     "says": "the capital buffer is a coordinate of equity against required capital, with the requirement at zero "
             "and zero equity at minus infinity"},
    {"id": "T6", "on": ("A5", "equity"), "kind": PROVEN, "rests_on": ("M10", "M11", "K2", "C2"),
     "says": "where the exposure binds the cap near an end, buffer variance tends to a limit set by the asset and "
             "liability shocks and the cap alone, which the asymmetry parameter does not enter"},
    {"id": "T7", "on": ("A5", "equity"), "kind": PROVEN, "rests_on": ("M12", "K2"),
     "says": "for every exposure within the cap, binding or not, buffer variance is bounded by the same quantities"},
    {"id": "T8", "on": ("A4", "A7"), "kind": PROVEN, "rests_on": ("M10", "M11", "M12"),
     "says": "the liability shock, one block across insured and uninsured creditors, enters the limits and the bounds "
             "of buffer variance"},
    {"id": "T9", "on": ("A5",), "kind": PROVEN, "rests_on": ("C3", "M11", "M12", "L2"),
     "says": "the surplus limit needs borrowing to hold the risky asset; with no borrowing the liquidity branch never "
             "binds and the deficit limit and the bound are what apply"},
    {"id": "T10", "on": ("A6",), "kind": READING, "rests_on": ("A2",),
     "says": "the supervisor holds the bank to the regulatory cap, a rule the model takes as given"},
    {"id": "T11", "on": ("A9",), "kind": READING, "rests_on": ("M8",),
     "says": "the shortfall penalty, zero at an asymmetry parameter of one and concave above it, stands for the "
             "supervisor's restrictions below the reference; no row derives the reading (Appendix D)"},
]


def theory_rows(tex):
    ids = set()
    for macro in ("krow", "prow", "arow", "mrow", "lrow", "crow"):
        ids |= set(re.findall(r"^\\" + macro + r"\{([A-Z][0-9]+)\}", tex, re.M))
    return ids


def players(tex):
    return {m.group(1): m.group(2) for m in re.finditer(r"^\\prow\{(P[0-9]+)\}\{([^{}]*)\}", tex, re.M)}


def verify(nodes, edges, annotations, excluded, tex, maptex):
    failures, checks = [], []

    def check(name, ok, detail=""):
        print(f"[{'PASS' if ok else 'FAIL'}] {name}" + (f"   {detail}" if detail and not ok else ""))
        checks.append(name)
        if not ok:
            failures.append(name)

    ents, acts, terms = LM.entity_rows(maptex), LM.action_rows(maptex), LM.terminology(maptex)
    rows, plays = theory_rows(tex), players(tex)
    for node, s in nodes.items():
        row = ents.get(s["row"])
        check(f"LT-1 {node}: row {s['row']} is in the map's Entities table, names it, status Report or Rule",
              row is not None and row[1] == node and row[4] in LM.OBSERVED, row and (row[1], row[4]))
        check(f"LT-2 {node}: a term of the map's Terminology table, Agreed or In use",
              terms.get(node) in ("Agreed", "In use"), terms.get(node))
        b = LM.NODES.get(node, {})
        check(f"LT-3 {node}: the bundle ledger has it with the same row and kind",
              b.get("row") == s["row"] and b.get("kind") == s["kind"], b)
        check(f"LT-4 {node}: its players are rows of the theory Players table",
              all(p in plays for p in s["players"]), s["players"])
    for eid, e in edges.items():
        b = LM.EDGES.get(eid, {})
        check(f"LT-5 {eid}: a row of the map's Actions table and an edge of the bundle ledger, same agent and target",
              eid in acts and b.get("agent") == e["agent"] and b.get("target") == e["target"], (e["agent"], e["target"]))
        check(f"LT-6 {eid}: agent an agent node, target a node, status settled, given or open, rows exist",
              nodes.get(e["agent"], {}).get("kind") == AGENT and e["target"] in nodes
              and e["status"] in (SETTLED, GIVEN, OPEN) and all(r in rows for r in e["rests_on"])
              and (e["status"] == OPEN) == (not e["rests_on"]), e)
    for a in annotations:
        check(f"LT-7 {a['id']}: sits on nodes or edges of the theory ledger and rests on rows of the theory manuscript",
              a["on"] and all(o in nodes or o in edges for o in a["on"]) and a["rests_on"]
              and all(r in rows for r in a["rests_on"]) and a["kind"] in (PROVEN, READING),
              [r for r in a["rests_on"] if r not in rows])
        check(f"LT-8 {a['id']}: a proven annotation rests on a model row",
              a["kind"] != PROVEN or any(r.startswith("M") for r in a["rests_on"]))
    models = sorted((r for r in rows if r.startswith("M")), key=lambda r: int(r[1:]))
    placed = {r for a in annotations for r in a["rests_on"]}
    check("LT-9 every model row of the theory manuscript is placed on the graph",
          models and all(m in placed for m in models), [m for m in models if m not in placed])
    check("LT-10 every theory player is carried by a node",
          plays and all(any(p in s["players"] for s in nodes.values()) for p in plays),
          [p for p in plays if not any(p in s["players"] for s in nodes.values())])
    covered = {s["row"] for s in nodes.values()} | set(edges) | set(excluded)
    check("LT-11 every row of the map's Entities and Actions tables is in the theory ledger or excluded with a reason",
          not (set(ents) | set(acts)) - covered and all(excluded.values()),
          sorted((set(ents) | set(acts)) - covered))
    check("LT-12 no row is both in the ledger and excluded",
          not ({s["row"] for s in nodes.values()} | set(edges)) & set(excluded))
    return failures, plays, checks


def main():
    tex, maptex = MANUSCRIPT.read_text(), LM.MAP_TEX.read_text()
    print("=" * 72)
    print("LEDGER OF THE THEORY PAPER (theory/checks/ledger_theory.py)")
    print("=" * 72)
    failures, plays, checks = verify(NODES, EDGES, ANNOTATIONS, EXCLUDED, tex, maptex)
    print("-" * 72)
    print("CONTROLS  each faulty ledger must fail")
    controls = []

    def control(name, nodes=NODES, edges=EDGES, annotations=ANNOTATIONS, excluded=EXCLUDED):
        import contextlib
        import io
        with contextlib.redirect_stdout(io.StringIO()):
            f, _, _ = verify(nodes, edges, annotations, excluded, tex, maptex)
        ok = bool(f)
        controls.append(ok)
        print(f"[{'PASS' if ok else 'FAIL'}] {name}")

    control("LT-C1 a node named by an unagreed term fails",
            nodes={**{k: v for k, v in NODES.items() if k != "investors"},
                   "shareholders": {"kind": AGENT, "row": "N3", "players": ("P2",)}})
    control("LT-C2 an annotation resting on a row the manuscript lacks fails",
            annotations=ANNOTATIONS + [{"id": "TX", "on": ("A1",), "kind": PROVEN, "rests_on": ("M99",), "says": "x"}])
    control("LT-C3 a model row placed nowhere fails",
            annotations=[a for a in ANNOTATIONS if "M8" not in a["rests_on"]])
    control("LT-C4 a map row neither in the ledger nor excluded fails",
            excluded={k: v for k, v in EXCLUDED.items() if k != "A10"})
    control("LT-C5 an edge whose agent is an object fails",
            edges={**EDGES, "A2": {**EDGES["A2"], "agent": "equity"}})
    print("-" * 72)
    names = {"P1": "bank", "P2": "investors", "P3": "insured creditors, uninsured creditors", "P4": "supervisor"}
    for p, name in plays.items():
        if name.strip().lower() not in names.get(p, ""):
            print(f"      decision: player {p} is named '{name}' in the manuscript and '{names.get(p)}' in the map")
    nf = len(failures) + sum(1 for ok in controls if not ok)
    print("=" * 72)
    print(f"LEDGER SUMMARY: {len(checks) + len(controls)} checks, {nf} failures")
    print("=" * 72)
    sys.exit(1 if nf else 0)


if __name__ == "__main__":
    main()
