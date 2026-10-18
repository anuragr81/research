"""Ledger of the conclusions (skeleton_who_pays.tex, Section 5): entities, actions, regularities.

The ledger is a graph. Its nodes are entities, things in the world that an observer
records: three agents and three objects, each with the attributes that can be read
from it. Its edges are actions, what an agent does to an entity, each leaving a
record. A count or pattern over the edges of one kind (how many "buys" edges, on
whom "awards" lands) is a pattern, computed from the records; it is not a node.
A conclusion is a regularity of the graph across contests, how a pattern responds
when an attribute or another pattern varies, and it is annotated on the edge or
attribute it concerns, never drawn as a node. Every node, attribute, edge and
pattern is a row of MEASUREMENT_MAP.tex whose Observed cell carries R or C.
Constructs, unobserved facts, nature and the names of theorems are not in the
graph; they enter a regularity only as its premises or grounds.

The summary that opens Section 4 is rendered from the regularities, and
figures/conclusions_ledger.dot from the graph.

verify() checks that every node, attribute, edge and pattern is an observable row
of the map and is well formed for its kind; that every edge is an option its agent
has in the manuscript's table of players; that every conclusion is represented by
a regularity; that each regularity varies attributes or patterns and moves a
pattern, cites only rows its conclusions rest on and a standing they state; that
every regularity with literature rows is expressed exactly once in the summary;
that each sentence names what its regularities vary and move; that no cited row
has changed since the regularities were last reviewed; the surface rules of
notes/writing_discipline.md; and that the paragraph and the figure's graph are
this ledger's renderings. It does not check that a regularity is a fair reading of
its conclusions; that is what the review before --restamp is for.

    python3 checks/ledger_literature.py              verify
    python3 checks/ledger_literature.py --write      render the paragraph and the figure's graph
    python3 checks/ledger_literature.py --restamp    record the cited rows as reviewed
"""

import ast
import hashlib
import json
import re
import sys
import textwrap
from pathlib import Path


HERE = Path(__file__).resolve().parent.parent
MANUSCRIPT_TEX = HERE / "skeleton_who_pays.tex"
MAP_TEX = HERE / "MEASUREMENT_MAP.tex"
VERIFY_DOCS = HERE / "checks" / "verify_docs.py"
STAMPS = HERE / "checks" / "ledger_literature_stamps.json"
FIGURE_DOT = HERE / "figures" / "conclusions_ledger.dot"     # dot -Tpdf renders figures/conclusions_ledger.pdf

BEGIN = "% BEGIN literature summary, rendered by checks/ledger_literature.py --write; edit the ledger, not this text"
END = "% END literature summary"

WORDS = (180, 700)
SURFACE_MARKS = {":": "colon", "\u2014": "dash", "\u2013": "dash", " - ": "dash", "--": "dash",
                 "$": "symbol", "\\": "macro"}
ROW_ID = re.compile(r"\b[LMKC][0-9]+\b")

MAP_TABLES = (("player", r"\section*{Players}"),
              ("input", r"\section*{Inputs}"),
              ("internal", r"\section*{Model-internal variables}"),
              ("output", r"\section*{Outputs}"))


# ---------------------------------------------------------------------------
# Nodes: entities, with their readable attributes
# ---------------------------------------------------------------------------
# "map" is the row's Symbol cell exactly as MEASUREMENT_MAP.tex writes it; "names" are the
# words the prose may use, each a Terminology term or a phrase of the row itself.

AGENT, OBJECT = "agent", "object"

NODES = {
    "challenger":  {"kind": AGENT, "map": r"challenger $i$", "names": ("challenger",),
                    "attributes": ("wealth",)},
    "incumbent":   {"kind": AGENT, "map": r"incumbent", "names": ("incumbent",), "attributes": ()},
    "institution": {"kind": AGENT, "map": r"institution", "names": ("institution",), "attributes": ()},
    "position":    {"kind": OBJECT, "map": r"$V$", "names": ("prize",),
                    "attributes": ("ranking",)},
    "credential":  {"kind": OBJECT, "map": r"credential", "names": ("bought component",),
                    "attributes": ("fee",)},
    "rule":        {"kind": OBJECT, "map": r"published rule", "names": ("published rule",),
                    "attributes": ("decisiveness",)},
}

ATTRIBUTES = {
    "wealth":       {"of": "challenger", "map": r"$w_i$; $(w_1,\dots,w_Q)$; $w_{(j)}$, $j$",
                     "names": ("wealth", "single-crossing widening")},
    "ranking":      {"of": "position", "map": r"$V$", "names": ("ranking",)},
    "fee":          {"of": "credential", "map": r"$c$", "names": ("fee",)},
    "decisiveness": {"of": "rule", "map": r"published rule", "names": ("weight on the base component",)},
    "score":        {"of": "competes", "map": r"$\sigma_i$", "names": ("score",)},   # of an edge
}

# Rows of the map's Players table that are not agents of the ledger, with the reason.
NOT_AGENTS = {"nature": "its draws are never observed; only the scores they produce are recorded"}

TABLE_OF_PLAYERS = {"challenger": "Challenger $i$", "incumbent": "Incumbent",
                    "institution": "The institution"}


# ---------------------------------------------------------------------------
# Edges: actions, each from an agent, with the record it leaves
# ---------------------------------------------------------------------------
# "table" is the phrase of the agent's row in the manuscript's table of players that gives it
# this option, or "" where the table lists the action under the institution's settings.

EDGES = {
    "admits":     {"from": "institution", "to": "challenger", "verb": "admits", "map": r"$Q$",
                   "table": "", "record": "who is eligible; their number"},
    "publishes":  {"from": "institution", "to": "rule", "verb": "publishes", "map": r"published rule",
                   "table": r"Fixes $V$, $c$ and $\mu$", "record": "the rule as published"},
    "issues":     {"from": "institution", "to": "credential", "verb": "issues", "map": r"$c$",
                   "table": r"Fixes $V$, $c$ and $\mu$", "record": "at the fee"},
    "holds_position": {"from": "incumbent", "to": "position", "verb": "holds", "map": r"incumbent",
                       "table": "", "record": "the holder before the contest"},
    "holds_credential": {"from": "incumbent", "to": "credential", "verb": "holds", "map": r"incumbent",
                         "table": "In M30, enter or not", "record": "scored as an entrant; buys it in M30"},
    "competes":   {"from": "challenger", "to": "position", "verb": "competes for", "map": r"$\sigma_i$",
                   "table": "", "record": "every challenger; the score"},
    "buys":       {"from": "challenger", "to": "credential", "verb": "buys", "map": r"$e_i$",
                   "table": "Enter or not", "record": "the entry decision, who paid"},
    "awards":     {"from": "institution", "to": "position", "verb": "awards",
                   "map": r"$\mathbf 1\{i\text{ wins}\}$", "table": "", "record": "to the winner"},
}


# ---------------------------------------------------------------------------
# Patterns: counts and patterns over the edges of one kind
# ---------------------------------------------------------------------------

PATTERNS = {
    "Q":           {"over": "admits", "map": r"$Q$", "names": ("number of challengers",)},
    "entrant_set": {"over": "buys", "map": r"$S$", "names": ("entrant set", "equilibrium")},
    "count":       {"over": "buys", "map": r"$k^*$", "names": ("entry count",)},
    "marginal":    {"over": "buys", "map": r"$w_{(k^*)}$", "names": ("marginal entrant",)},
    "assortative": {"over": "buys", "map": r"$\{(1),\dots,(k^*)\}$", "names": ("assortative set", "richest")},
    "share":       {"over": "awards", "map": r"Entrants' share of the prize", "names": ("entrants' share",)},
    "lone_gain":   {"over": "awards", "map": r"$\Delta_Q(0)$; $D(Q)=\Delta_{Q+1}(0)-\Delta_Q(0)$; $W_Q$",
                    "names": ("first entrant's gain",)},
}


# ---------------------------------------------------------------------------
# Regularities: what the conclusions say about the graph
# ---------------------------------------------------------------------------

# Standing against the papers read, each with the words a cited conclusion must use.
STANDING = {
    "new":         r"\bnew among\b|\bare new\b|\bis new\b",
    "sharpening":  r"\bsharpening\b",
    "credited":    r"\bform of\b|\bcredits\b",
    "opposite":    r"\bopposite\b",
    "settled":     r"\bsettled without\b",
    "implication": None,     # C15 to C18, which the Conclusions call implications for the rule
    "model":       None,     # a statement about the model with no comparison to a paper
}
IMPLICATIONS = {"C15", "C16", "C17", "C18"}
COMPARATIVE = {"new", "sharpening", "credited", "opposite", "settled"}

# varies: attributes or patterns that change; fixed: what is held; moves: the pattern that
# responds; effect: how, in words; given: observable conditions (attributes, patterns, edges);
# premises: model assumptions that are constructs, in words.
REGULARITIES = {
    "spread_sign": {
        "varies": ("wealth",), "fixed": ("fee", "decisiveness", "Q"), "moves": "count",
        "effect": "a single-crossing widening moves it, with the sign set by the cut-off wealth against the break-even wealth",
        "given": ("marginal",),
        "premises": "the fee's utility cost falls with wealth; draws independent of wealth",
        "conclusions": ("C1", "C5"), "standing": ("new",),
        "model": ("M13", "M23", "M24", "M26"), "lit": ("L12", "L29", "L30"),
    },
    "two_ranks": {
        "varies": ("wealth",), "fixed": ("fee", "decisiveness", "Q"), "moves": "count",
        "effect": "any change of profile is signed at two ranks, outside the band", "given": ("marginal",),
        "premises": "",
        "conclusions": ("C2", "C8"), "standing": ("sharpening",),
        "model": ("M12", "M13", "M14"), "lit": ("L9",),
    },
    "count_pinned": {
        "varies": ("entrant_set",), "fixed": ("fee", "decisiveness", "Q", "wealth"), "moves": "count",
        "effect": "the same in every equilibrium", "given": ("score",),
        "premises": "the gain from buying depends on how many buy, not on who",
        "conclusions": ("C3", "C6"), "standing": ("new",),
        "model": ("M4", "M6", "M26", "M27", "M29"), "lit": ("L2", "L3", "L5", "L25", "L28", "L29", "L31"),
    },
    "who_enters": {
        "varies": ("wealth",), "fixed": ("fee", "decisiveness", "Q"), "moves": "assortative",
        "effect": "the richest buying is always an equilibrium", "given": (),
        "premises": "unique when the next-richest's cost exceeds the gain",
        "conclusions": ("C4",), "standing": ("settled",),
        "model": ("M5", "M6", "M7", "M8"), "lit": ("L2", "L3", "L7"),
    },
    "gain_form": {
        "varies": ("score",), "fixed": ("Q",), "moves": "lone_gain",
        "effect": "the form of Ryvkin and Drugov's marginal benefit", "given": (),
        "premises": "",
        "conclusions": ("C14",), "standing": ("credited",),
        "model": ("M1", "M17", "M21"), "lit": ("L8",),
    },
    "gain_in_Q": {
        "varies": ("Q",), "fixed": ("fee", "decisiveness"), "moves": "lone_gain",
        "effect": "never falls", "given": (),
        "premises": "a uniform base component",
        "conclusions": ("C7",), "standing": ("new",),
        "model": ("M18", "M19"), "lit": ("L8", "L26"),
    },
    "closure_sign": {
        "varies": ("decisiveness",), "fixed": ("fee", "wealth", "Q"), "moves": "count",
        "effect": "does not rise as the credential is less decisive", "given": ("holds_credential",),
        "premises": "the incumbent holds the credential",
        "conclusions": ("C9", "C12"), "standing": ("new", "opposite"),
        "model": ("M28",), "lit": ("L20", "L22"),
    },
    "share_follows": {
        "varies": ("count", "decisiveness"), "fixed": (), "moves": "share",
        "effect": "moves with the count; does not rise as the credential is less decisive",
        "given": (), "premises": "",
        "conclusions": ("C10", "C13"), "standing": ("new",),
        "model": ("M11", "M12", "M13", "M28", "M31"), "lit": ("L23", "L25", "L28", "L29", "L32"),
    },
    "incumbent_entry": {
        "varies": ("entrant_set",), "fixed": ("fee", "decisiveness", "Q", "wealth"), "moves": "count",
        "effect": "the incumbent out while a challenger buys, only in one of several equilibria",
        "given": ("holds_credential",), "premises": "the incumbent has the lowest cost",
        "conclusions": ("C11",), "standing": ("model",),
        "model": ("M6", "M7", "M30"), "lit": (),
    },
    "fee_order": {
        "varies": ("fee",), "fixed": ("decisiveness", "Q", "wealth"), "moves": "assortative",
        "effect": "a uniform fee changes how many buy, not the order", "given": (),
        "premises": "the fee's utility cost falls with wealth",
        "conclusions": ("C15",), "standing": ("implication",),
        "model": ("M5", "M8", "M11", "M23", "M33"), "lit": (),
    },
    "prize_cap": {
        "varies": ("ranking", "fee"), "fixed": ("decisiveness", "Q", "wealth"), "moves": "count",
        "effect": "the prize caps how many buy; the cap sets a fee", "given": (),
        "premises": "the cost to the next-richest against the prize",
        "conclusions": ("C16",), "standing": ("implication",),
        "model": ("M10", "M33"), "lit": (),
    },
    "compression": {
        "varies": ("wealth",), "fixed": ("fee", "decisiveness", "Q"), "moves": "count",
        "effect": "a single-crossing narrowing is read at two ranks; no general sign for less inequality",
        "given": ("marginal",), "premises": "",
        "conclusions": ("C17",), "standing": ("implication",),
        "model": ("M12", "M13", "M14", "M33"), "lit": (),
    },
    "prize_fee": {
        "varies": ("ranking", "fee"), "fixed": ("decisiveness", "Q", "wealth"), "moves": "count",
        "effect": "does not fall in the prize and does not rise in the fee", "given": (),
        "premises": "challengers at or below the fee out",
        "conclusions": ("C19",), "standing": ("credited",),
        "model": ("M11", "M31"), "lit": ("L2", "L3", "L4"),
    },
    "share_benchmark": {
        "varies": ("count", "Q"), "fixed": ("decisiveness",), "moves": "share",
        "effect": "above the benchmark only if the credential counts", "given": (),
        "premises": "",
        "conclusions": ("C18",), "standing": ("implication",),
        "model": ("M31", "M33"), "lit": ("L19",),
    },
}


# ---------------------------------------------------------------------------
# Sentences: the summary that opens Section 4
# ---------------------------------------------------------------------------

SENTENCES = [
    {"para": 1, "expresses": (),
     "text": "In the related models something other than wealth decides who stands at the "
             "margin, a device among identical entrants or, in Costrell and Loury (L9), "
             "technology."},
    {"para": 1, "expresses": (),
     "text": "Entry models with identical entrants pin the entry count in pure strategies and "
             "leave who enters to mixed entry (L2), private random delays (L3) or sequential "
             "arrival (L7)."},
    {"para": 1, "expresses": ("count_pinned", "who_enters"),
     "text": "Here challengers differ in wealth, every pure-strategy equilibrium has the same "
             "entry count, and the assortative set entering is always one of them."},
    {"para": 1, "expresses": (),
     "text": "Count invariance rests on anonymity and need not hold without it, as when the "
             "bought component varies with wealth, and entrant sets of that count can differ, "
             "which is identity multiplicity."},
    {"para": 1, "expresses": (),
     "text": "Costrell and Loury (L9) sign how a mean-preserving spread in abilities moves "
             "wages by where one marginal worker stands, a quantile that technology fixes."},
    {"para": 1, "expresses": ("spread_sign", "two_ranks"),
     "text": "Here a single-crossing widening of the wealth profile moves the entry count "
             "by where the cut-off wealth stands against the widening's break-even wealth, "
             "which moves with the wealth profile, and for any change of profile two ranks, "
             "the marginal entrant and the "
             "first outsider, sign the change outside the band."},
    {"para": 1, "expresses": (),
     "text": "The sign needs anonymity and burden-monotonicity, and burden-monotonicity fails "
             "when utility has a convex stretch longer than the fee."},
    {"para": 2, "expresses": (),
     "text": "Effort models (L20, L22) and this paper ask how participation responds when the "
             "ranking reads the costly choice more."},
    {"para": 2, "expresses": (),
     "text": "In effort models fewer take part, because effort rises with the accuracy of the "
             "ranking and competes away the rents of entering."},
    {"para": 2, "expresses": ("closure_sign",),
     "text": "Here, with the incumbent scored as an entrant, a lower weight on the base "
             "component never lowers the entry count, because the fee does not rise with it; "
             "for an incumbent protected from the rule the argument does not apply."},
    {"para": 2, "expresses": (),
     "text": "In the entry models read outsiders cannot win (L29), so no entrants' share is "
             "defined, and in Fryer and Loury (L25) those who do not invest still compete but "
             "form a continuum, so no share is derived as a function of how many enter."},
    {"para": 2, "expresses": ("share_follows", "share_benchmark"),
     "text": "Here outsiders compete on the base component, and the entrants' share rises "
             "weakly with the entry count, does not rise in the weight on the base component, "
             "and is at least the entry count plus one over the number of challengers plus "
             "one, the incumbent counted in both."},
    {"para": 2, "expresses": (),
     "text": "A bought component that adds nothing gives exactly that benchmark, so only a "
             "share above it shows that the bought component counts."},
    {"para": 3, "expresses": (),
     "text": "In the economics of selection, wealth decides who competes through a constraint "
             "or a price."},
    {"para": 3, "expresses": (),
     "text": "In Lazear and Rosen (L1) individuals self-select the payment scheme in "
             "accordance with their wealth, through declining absolute risk aversion; in "
             "Fernández and Galí (L30) wealth matters only through a borrowing constraint, "
             "and without one markets and tournaments allocate alike; in Fryer and Loury "
             "(L25) slots are sold at a common price, and with no policy intervention the "
             "group with higher investment costs is less than proportionally represented "
             "among slot owners."},
    {"para": 3, "expresses": (),
     "text": "Here wealth acts through the entry cost of a fixed fee, which needs only that "
             "utility be strictly concave, and not through a constraint, a market-clearing "
             "price or a condition on the third derivative (L1, L12)."},
    {"para": 3, "expresses": (),
     "text": "The addition to Fryer and Loury is a fixed number of positions awarded by rank "
             "among finitely many challengers, which pins the entry count and the assortative "
             "set and makes the entrants' share a function of the count, with proportional "
             "representation, the entry count plus one over the number of challengers plus "
             "one, the benchmark at which the bought component adds nothing."},
    {"para": 3, "expresses": (),
     "text": "The addition to rent-seeking contests with an entry fee, Corcoran and Karels "
             "(L29), is that outsiders compete on the base component, so the share is defined "
             "at all."},
    {"para": 3, "expresses": (),
     "text": "The account fails where the bought component varies with wealth, since then the "
             "gain depends on who enters and the entry count is not pinned."},
]


# ---------------------------------------------------------------------------
# Reading the documents
# ---------------------------------------------------------------------------

def map_rows(tex):
    """Symbol cell -> {table, observed, text}, for every row of the map's tables."""
    rows = {}
    for table, header in MAP_TABLES:
        a = tex.find(header)
        if a < 0:
            continue
        b = tex.find(r"\end{longtable}", a)
        body = tex[tex.find(r"\endfoot", a) + len(r"\endfoot"):b]
        for row in re.split(r"\\\\\s*(?:\\addlinespace)?", body):
            row = row.strip()
            if not row:
                continue
            cells = [c.strip() for c in row.split("&")]
            if len(cells) == 5:
                rows[cells[0]] = {"table": table, "observed": cells[3], "text": row}
    return rows


def row_blocks(tex):
    """Every \\lrow, \\mrow, \\krow and \\crow, mapped to its source up to the next row or table end."""
    starts = [(m.start(), m.group(1)) for m in re.finditer(r"\\[lmkc]row\{([LMKC][0-9]+)\}", tex)]
    blocks = {}
    for n, (pos, rid) in enumerate(starts):
        nxt = starts[n + 1][0] if n + 1 < len(starts) else len(tex)
        end = tex.find(r"\end{longtable}", pos)
        blocks[rid] = tex[pos:min(nxt, end) if end >= 0 else nxt]
    return blocks


def rests_on(block):
    """The row identifiers in the last argument of a \\crow."""
    i = block.rfind("{")
    return set(ROW_ID.findall(block[i:])) if i >= 0 else set()


def statement(block):
    i = block.rfind("{")
    return block[:i] if i >= 0 else block


def table_of_players(tex):
    """Player label -> its row of the manuscript's table of players (tab:players)."""
    a = tex.find(r"\label{tab:players}")
    b = tex.find(r"\end{longtable}", a)
    if a < 0 or b < 0:
        return {}
    body = tex[tex.find(r"\endfoot", a) + len(r"\endfoot"):b]
    rows = {}
    for row in re.split(r"\\\\\s*(?:\\addlinespace)?", body):
        row = row.strip()
        if row:
            rows[row.split("&", 1)[0].strip()] = row
    return rows


def terminology_terms(tex):
    a = tex.find(r"\section{Terminology}")
    b = tex.find(r"\end{longtable}", a)
    if a < 0 or b < 0:
        return set()
    terms = set()
    for line in tex[a:b].split("\n"):
        if "&" in line and not line.lstrip().startswith(("%", "\\")):
            for t in line.split("&", 1)[0].split(","):
                terms.add(t.strip().lower())
    return terms


def observable(cell):
    """Whether the map's Observed cell says the object is recorded (R) or computed (C)."""
    return re.search(r"\b[RC]\b", cell) is not None


def digest(text):
    return hashlib.sha256(text.encode()).hexdigest()[:16]


def banned_patterns():
    tree = ast.parse(VERIFY_DOCS.read_text())
    for node in tree.body:
        if isinstance(node, ast.Assign) and any(getattr(t, "id", None) == "BANNED" for t in node.targets):
            return ast.literal_eval(node.value)
    return []


CITATION = re.compile(r"\((L[0-9]+(?:, L[0-9]+)*)\)")


def citations(text):
    """The literature rows a sentence cites in parentheses."""
    return [r for m in CITATION.finditer(text) for r in m.group(1).split(", ")]


def surface_faults(text):
    faults = [name for mark, name in SURFACE_MARKS.items() if mark in text]
    faults += [f"row id {r}" for r in ROW_ID.findall(CITATION.sub("", text))]
    faults += [f"retired synonym {p}" for p in banned_patterns() if re.search(p, text, re.I)]
    return faults


def mapped():
    """Every element of the graph that has a map row: key -> (kind, record)."""
    out = {}
    for k, v in NODES.items():
        out[k] = ("node", v)
    for k, v in ATTRIBUTES.items():
        out[k] = ("attribute", v)
    for k, v in EDGES.items():
        out[k] = ("edge", v)
    for k, v in PATTERNS.items():
        out[k] = ("pattern", v)
    return out


# ---------------------------------------------------------------------------
# Stamps
# ---------------------------------------------------------------------------

def stamped_sources(blocks, mrows, players):
    out = {}
    for key, (kind, v) in mapped().items():
        if v["map"] in mrows:
            out[f"map:{key}"] = digest(mrows[v["map"]]["text"])
    for agent, label in TABLE_OF_PLAYERS.items():
        if label in players:
            out[f"players:{agent}"] = digest(players[label])
    for r in REGULARITIES.values():
        for c in r["conclusions"]:
            if c in blocks:
                out[c] = digest(blocks[c])
    return out


def load_stamps():
    return json.loads(STAMPS.read_text()) if STAMPS.exists() else {}


def restamp():
    tex = MANUSCRIPT_TEX.read_text()
    stamps = stamped_sources(row_blocks(tex), map_rows(MAP_TEX.read_text()), table_of_players(tex))
    STAMPS.write_text(json.dumps(dict(sorted(stamps.items())), indent=1) + "\n")


# ---------------------------------------------------------------------------
# Rendering: the paragraph and the figure's graph
# ---------------------------------------------------------------------------

def rendered():
    paras = []
    for n in sorted({s["para"] for s in SENTENCES}):
        prose = " ".join(s["text"] for s in SENTENCES if s["para"] == n)
        paras.append(textwrap.fill(prose, width=80, break_on_hyphens=False))
    return BEGIN + "\n" + "\n\n".join(paras) + "\n" + END


def paragraph_in_manuscript(tex):
    a = tex.find(BEGIN)
    b = tex.find(END, a)
    if a < 0 or b < 0:
        return None
    return tex[a:b + len(END)]


def concerns():
    """For each attribute and pattern, the conclusions in which it varies or responds."""
    out = {}
    for r in REGULARITIES.values():
        for v in r["varies"]:
            out.setdefault(v, {}).setdefault("varied in", set()).update(r["conclusions"])
        out.setdefault(r["moves"], {}).setdefault("responds in", set()).update(r["conclusions"])
    return out


def crows(s):
    return ", ".join(sorted(s, key=lambda c: int(c[1:])))


def notes_for(key, con):
    return "; ".join(f"{k} {crows(v)}" for k, v in con.get(key, {}).items())


def dot():
    con = concerns()
    out = ['digraph conclusions {', 'rankdir=LR; nodesep=0.5; ranksep=1.6;',
           'node [fontname="Helvetica", fontsize=11, shape=plaintext];',
           'edge [fontname="Helvetica", fontsize=9];', 'fontname="Helvetica";']
    for key, n in NODES.items():
        fill = "#c8e6c9" if n["kind"] == AGENT else "#e3f2fd"
        rows = [f'<tr><td colspan="2" bgcolor="{fill}" align="left"><b>{n["names"][0]}</b></td></tr>']
        for a in n["attributes"]:
            rows.append(f'<tr><td align="left">{ATTRIBUTES[a]["names"][0]}</td><td align="left">'
                        f'<font point-size="8" color="#555555">{notes_for(a, con)}</font></td></tr>')
        out.append(f'"{key}" [label=<<table border="1" cellborder="0" cellspacing="0" cellpadding="3" '
                   f'color="#999999">{"".join(rows)}</table>>];')
    for key, e in EDGES.items():
        parts = [f'<b>{e["verb"]}</b>', f'<font point-size="8">{e["record"]}</font>']
        for a, av in ATTRIBUTES.items():
            if av["of"] == key:
                notes = notes_for(a, con)
                parts.append(f'<font point-size="8">{av["names"][0]}' + (f", {notes}" if notes else "") + '</font>')
        for p, pv in PATTERNS.items():
            if pv["over"] == key:
                notes = notes_for(p, con)
                parts.append(f'<font point-size="8" color="#880e4f">{pv["names"][0]}'
                             + (f", {notes}" if notes else "") + '</font>')
        colour = {"challenger": "#1b5e20", "institution": "#0d47a1"}.get(e["from"], "#4e342e")
        out.append(f'"{e["from"]}" -> "{e["to"]}" [label=<{"<br/>".join(parts)}>, color="{colour}", '
                   f'fontcolor="{colour}"];')
    out.append('legend [label=<<table border="0" cellspacing="2">'
               '<tr><td align="left"><b>Nodes</b> are entities with their readable attributes, '
               'agents green and objects blue.</td></tr>'
               '<tr><td align="left"><b>Edges</b> are actions, from the agent to what it acts on, '
               'with the record each leaves; in purple, the counts and patterns over edges of that kind.</td></tr>'
               '<tr><td align="left">Each attribute or pattern lists the conclusions in which it varies '
               'or responds.</td></tr></table>>];')
    out.append('}')
    return "\n".join(out)


def write():
    tex = MANUSCRIPT_TEX.read_text()
    old = paragraph_in_manuscript(tex)
    if old is None:
        sys.exit("no BEGIN/END markers in the manuscript")
    MANUSCRIPT_TEX.write_text(tex.replace(old, rendered()))
    FIGURE_DOT.parent.mkdir(exist_ok=True)
    FIGURE_DOT.write_text(dot() + "\n")


# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------

def graph_faults(nodes, attributes, edges, patterns, mrows, terms, players):
    faults = []
    elements = [("node", k, v) for k, v in nodes.items()] + [("attribute", k, v) for k, v in attributes.items()] \
        + [("edge", k, v) for k, v in edges.items()] + [("pattern", k, v) for k, v in patterns.items()]
    for kind, key, v in elements:
        row = mrows.get(v["map"])
        if row is None:
            faults.append(f"{kind} {key} is not a row of the measurement map")
            continue
        if not observable(row["observed"]):
            faults.append(f"{kind} {key} is not observable; the map codes it {row['observed']!r}")
        for n in v.get("names", ()):
            if n.lower() not in terms and n.lower() not in row["text"].lower():
                faults.append(f"{kind} {key} is called {n!r}, which is neither a Terminology term "
                              f"nor a phrase of its map row")
        if kind == "node" and v["kind"] == AGENT and row["table"] != "player":
            faults.append(f"agent {key} is not in the map's Players table")
        if kind == "node":
            for a in v["attributes"]:
                if attributes.get(a, {}).get("of") != key:
                    faults.append(f"node {key} lists attribute {a}, which is not an attribute of it")
        if kind == "attribute" and v["of"] not in nodes and v["of"] not in edges:
            faults.append(f"attribute {key} is of {v['of']}, which is neither a node nor an edge")
        if kind == "pattern" and v["over"] not in edges:
            faults.append(f"pattern {key} is over {v['over']}, which is not an edge")
        if kind == "edge":
            if nodes.get(v["from"], {}).get("kind") != AGENT:
                faults.append(f"edge {key} does not start from an agent")
            if v["to"] not in nodes:
                faults.append(f"edge {key} points to {v['to']}, which is not a node")
            label = TABLE_OF_PLAYERS.get(v["from"])
            if label not in players:
                faults.append(f"edge {key}: its agent has no row in the table of players")
            elif v["table"] and v["table"] not in players[label]:
                faults.append(f"edge {key}: {v['table']!r} is not in its agent's row of the table of players")
    for symbol, row in mrows.items():
        if row["table"] == "player":
            is_agent = any(n["kind"] == AGENT and n["map"] == symbol for n in nodes.values())
            if not is_agent and symbol not in NOT_AGENTS:
                faults.append(f"player row {symbol!r} of the map is neither an agent nor excused in NOT_AGENTS")
    return faults


def regularity_faults(regs, attributes, edges, patterns, blocks):
    faults = []
    variable = set(attributes) | set(patterns)
    represented = {c for r in regs.values() for c in r["conclusions"]}
    for c in sorted((b for b in blocks if b.startswith("C")), key=lambda x: int(x[1:])):
        if c not in represented:
            faults.append(f"conclusion {c} is represented by no regularity")
    for rid, r in regs.items():
        if not r["varies"]:
            faults.append(f"regularity {rid} varies nothing")
        for v in tuple(r["varies"]) + tuple(r["fixed"]):
            if v not in variable:
                faults.append(f"regularity {rid} varies or fixes {v}, which is not an attribute or a pattern")
        if r["moves"] not in patterns:
            faults.append(f"regularity {rid} moves {r['moves']}, which is not a pattern over an edge")
        for g in r["given"]:
            if g not in variable and g not in edges:
                faults.append(f"regularity {rid} is given {g}, which is not an attribute, pattern or edge")
        missing = [c for c in r["conclusions"] if c not in blocks]
        if not r["conclusions"] or missing:
            faults.append(f"regularity {rid} cites no conclusion, or one that does not exist: {missing}")
            continue
        grounds = set().union(*(rests_on(blocks[c]) for c in r["conclusions"]))
        stray = [g for g in tuple(r["model"]) + tuple(r["lit"]) if g not in grounds]
        if stray:
            faults.append(f"regularity {rid} cites {stray}, on which its conclusions do not rest")
        text = " ".join(statement(blocks[c]) for c in r["conclusions"])
        for s in r["standing"]:
            if s not in STANDING:
                faults.append(f"regularity {rid} has unknown standing {s!r}")
            elif s == "implication" and not set(r["conclusions"]) <= IMPLICATIONS:
                faults.append(f"regularity {rid} is an implication but cites a conclusion outside C15 to C18")
            elif STANDING[s] and not re.search(STANDING[s], text):
                faults.append(f"regularity {rid} is {s!r}, which none of its conclusions states")
        if set(r["standing"]) & COMPARATIVE and not r["lit"]:
            faults.append(f"regularity {rid} compares with the literature but cites no literature row")
    return faults


def sentence_faults(sentences, regs, attributes, patterns, blocks=None):
    faults = []
    for n, s in enumerate(sentences, 1):
        for c in citations(s["text"]):
            if blocks is not None and c not in blocks:
                faults.append(f"sentence {n} cites {c}, which is not a row of the literature table")
    names = {**{k: v["names"] for k, v in attributes.items()}, **{k: v["names"] for k, v in patterns.items()}}
    expressed = [rid for s in sentences for rid in s["expresses"]]
    for rid, r in regs.items():
        n = expressed.count(rid)
        if n > 1:
            faults.append(f"regularity {rid} is expressed {n} times")
    for rid in sorted(set(expressed) - set(regs)):
        faults.append(f"a sentence expresses {rid}, which is not a regularity")
    for n, s in enumerate(sentences, 1):
        text = s["text"].lower()
        for rid in s["expresses"]:
            r = regs.get(rid)
            if r is None:
                continue
            for e in tuple(r["varies"]) + (r["moves"],):
                if e in names and not any(name.lower() in text for name in names[e]):
                    faults.append(f"sentence {n} does not name {names[e][0]!r}")
        f = surface_faults(s["text"])
        if f:
            faults.append(f"sentence {n} breaks the surface rules: {f}")
    return faults


def controls(blocks, mrows, terms, players):
    """Each check catches the fault it exists for."""
    parsers_ok = (
        row_blocks(r"\crow{C1}{a}{M1, L2}\crow{C2}{b}{M3}\end{longtable}")
        == {"C1": r"\crow{C1}{a}{M1, L2}", "C2": r"\crow{C2}{b}{M3}"}
        and rests_on(r"\crow{C1}{the statement M9}{M1, L2}") == {"M1", "L2"}
        and observable("K; the prize itself R") and not observable("N; rules ordered")
        and "row id M28" in surface_faults("as M28 shows")
        and "colon" in surface_faults("one thing: another")
        and any(f.startswith("retired synonym") for f in surface_faults("the payer enters"))
        and not surface_faults("A single-crossing widening of wealth moves the entry count.")
    )
    g = graph_faults(
        dict(NODES, bogus={"kind": OBJECT, "map": "$zz$", "names": ("x",), "attributes": ()},
             construct={"kind": OBJECT, "map": r"$\kappa(w)=u(w)-u(w-c)$", "names": ("entry cost",), "attributes": ()}),
        dict(ATTRIBUTES, misnamed={"of": "credential", "map": r"$c$", "names": ("toll",)}),
        dict(EDGES, bid={"from": "challenger", "to": "position", "verb": "bids", "map": r"$e_i$",
                         "table": "Bids an amount", "record": ""},
             drift={"from": "position", "to": "credential", "verb": "x", "map": r"$e_i$", "table": "", "record": ""}),
        dict(PATTERNS, orphan={"over": "wealth", "map": r"$S$", "names": ("entrant set",)}),
        mrows, terms, players)
    graph_ok = (any("bogus is not a row" in f for f in g)
                and any("construct is not observable" in f for f in g)
                and any("'toll'" in f for f in g)
                and any("'Bids an amount' is not in its agent's row" in f for f in g)
                and any("drift does not start from an agent" in f for f in g)
                and any("orphan is over wealth" in f for f in g))
    bad = dict(REGULARITIES)
    bad["share_benchmark"] = dict(REGULARITIES["share_benchmark"], lit=("L1",))
    bad["gain_form"] = dict(REGULARITIES["gain_form"], standing=("new",))
    bad["prize_cap"] = dict(REGULARITIES["prize_cap"], moves="fee")
    bad["compression"] = dict(REGULARITIES["compression"], varies=("credential",))
    del bad["fee_order"]
    r = regularity_faults(bad, ATTRIBUTES, EDGES, PATTERNS, blocks)
    regs_ok = (any("C15 is represented by no regularity" in f for f in r)
               and any("share_benchmark cites ['L1']" in f for f in r)
               and any("gain_form is 'new'" in f for f in r)
               and any("prize_cap moves fee" in f for f in r)
               and any("compression varies or fixes credential" in f for f in r))
    s = sentence_faults(SENTENCES + [{"para": 9, "expresses": ("closure_sign", "nothing"), "text": "Unnamed (L99)."}],
                        REGULARITIES, ATTRIBUTES, PATTERNS, blocks)
    sentences_ok = (any("closure_sign is expressed 2 times" in f for f in s)
                    and any("cites L99, which is not a row" in f for f in s)
                    and "row id L9" not in surface_faults("as Costrell and Loury (L9) show")
                    and "row id M9" in surface_faults("as (M9) shows")
                    and any("nothing, which is not a regularity" in f for f in s)
                    and any("does not name" in f for f in s))
    return parsers_ok and graph_ok and regs_ok and sentences_ok


def verify(report=True):
    ok = True

    def fail(msg):
        nonlocal ok
        ok = False
        if report:
            print("FAIL " + msg)

    tex = MANUSCRIPT_TEX.read_text()
    blocks = row_blocks(tex)
    mrows = map_rows(MAP_TEX.read_text())
    terms = terminology_terms(tex)
    players = table_of_players(tex)

    for f in (graph_faults(NODES, ATTRIBUTES, EDGES, PATTERNS, mrows, terms, players)
              + regularity_faults(REGULARITIES, ATTRIBUTES, EDGES, PATTERNS, blocks)
              + sentence_faults(SENTENCES, REGULARITIES, ATTRIBUTES, PATTERNS, blocks)):
        fail(f)

    stamps = load_stamps()
    for k, d in sorted(stamped_sources(blocks, mrows, players).items()):
        if stamps.get(k) != d:
            fail(f"{k} changed since the regularities were reviewed against it; "
                 f"re-read the regularities that cite it, then run --restamp")

    words = len(" ".join(s["text"] for s in SENTENCES).split())
    if not WORDS[0] <= words <= WORDS[1]:
        fail(f"the summary has {words} words, outside {WORDS[0]} to {WORDS[1]}")

    current = paragraph_in_manuscript(tex)
    if current is None:
        fail("the manuscript has no rendered literature summary")
    elif current != rendered():
        fail("the paragraph in the manuscript is not the ledger's rendering; run --write")

    if not FIGURE_DOT.exists() or FIGURE_DOT.read_text() != dot() + "\n":
        fail("figures/conclusions_ledger.dot is not the ledger's graph; run --write")

    if not controls(blocks, mrows, terms, players):
        fail("control: a check misses the fault it exists for")

    if report:
        print(f"conclusions ledger: {len(NODES)} nodes, {len(ATTRIBUTES)} attributes, {len(EDGES)} edges, "
              f"{len(PATTERNS)} patterns; {len(REGULARITIES)} regularities; "
              f"{len(SENTENCES)} sentences, {words} words")
        used = {e for r in REGULARITIES.values() for e in tuple(r["varies"]) + (r["moves"],) + tuple(r["given"])}
        unused = sorted(k for k in list(ATTRIBUTES) + list(PATTERNS) if k not in used)
        if unused:
            print(f"REPORT attributes or patterns in no regularity: {', '.join(unused)}")
        expressed = {rid for s in SENTENCES for rid in s["expresses"]}
        silent = sorted(rid for rid, r in REGULARITIES.items() if r["lit"] and rid not in expressed)
        if silent:
            print(f"REPORT regularities with literature rows the summary does not express: {', '.join(silent)}")
        print("verify(): " + ("True" if ok else "False"))
    return ok


if __name__ == "__main__":
    if "--write" in sys.argv:
        write()
    elif "--restamp" in sys.argv:
        restamp()
    sys.exit(0 if verify() else 1)
