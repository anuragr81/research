"""Ledger of the conclusions (MANUSCRIPT.tex, Section 5), over observable entities.

Entities first, then the relations among them, then the sentences rendered from the
relations.

  Entities   what an observer can record or compute from records, each a row of
             MEASUREMENT_MAP.tex whose Observed cell carries R or C. Four kinds:
             agents (who act), objects (what they act on, with readable attributes),
             actions (agent, verb, object, with the record each leaves), and measures
             of actions (counts, sets and frequencies computed from those records).
             A change of the wealth profile is a state, two recorded profiles.
             Constructs (utility, entry cost, gain, payoffs), unobserved facts (the
             weight as a number, the draws, nature) and the names of theorems are
             not entities; they appear only as premises or grounds of a relation.
  Relations  what the conclusions C1 to C18 say: a change in some entities moves a
             measure of an action, with other entities fixed and under conditions
             that are themselves entities. Each relation cites the conclusions it
             represents, the model and literature rows those conclusions rest on,
             its premises (model assumptions that are constructs) and its standing
             against the papers read.
  Sentences  the summary that opens Section 4, each expressing listed relations and
             naming their entities in the manuscript's words; no row identifiers.

verify() checks that every entity is an observable row of the map and is well
formed for its kind; that every action is an option its agent has in the
manuscript's table of players; that every conclusion is represented; that each
relation cites only rows its conclusions rest on and a standing they state; that
every relation with literature rows is expressed exactly once; that each sentence
names what its relations vary and measure; that no cited row has changed since the
relations were last reviewed; the surface rules of notes/writing_discipline.md; and
that the paragraph in MANUSCRIPT.tex and figures/conclusions_ledger.dot are this
ledger's renderings. It does not check that a relation is a fair reading of its
conclusions; that is what the review before --restamp is for.

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
MANUSCRIPT_TEX = HERE / "MANUSCRIPT.tex"
MAP_TEX = HERE / "MEASUREMENT_MAP.tex"
VERIFY_DOCS = HERE / "checks" / "verify_docs.py"
STAMPS = HERE / "checks" / "ledger_literature_stamps.json"
FIGURE_DOT = HERE / "figures" / "conclusions_ledger.dot"     # dot -Tpdf renders figures/conclusions_ledger.pdf

BEGIN = "% BEGIN literature summary, rendered by checks/ledger_literature.py --write; edit the ledger, not this text"
END = "% END literature summary"

WORDS = (180, 220)
SURFACE_MARKS = {":": "colon", "\u2014": "dash", "\u2013": "dash", " - ": "dash", "--": "dash",
                 "$": "symbol", "\\": "macro"}
ROW_ID = re.compile(r"\b[LMKC][0-9]+\b")

MAP_TABLES = (("player", r"\section*{Players}"),
              ("input", r"\section*{Inputs}"),
              ("internal", r"\section*{Model-internal variables}"),
              ("output", r"\section*{Outputs}"))


# ---------------------------------------------------------------------------
# Entities
# ---------------------------------------------------------------------------

AGENT, OBJECT, ATTRIBUTE, ACTION, MEASURE, STATE = "agent", "object", "attribute", "action", "measure", "state"
KINDS = {AGENT, OBJECT, ATTRIBUTE, ACTION, MEASURE, STATE}

# "map" is the row's Symbol cell exactly as MEASUREMENT_MAP.tex writes it; "names" are the
# words the prose may use, each a Terminology term or a phrase of the row itself. An action
# names its agent, verb and object and, in "table", the phrase of the agent's row in the
# manuscript's table of players that gives it that option. A measure names the action it
# measures; an attribute the object it is read from.

ENTITIES = {
    # agents
    "challenger":  {"kind": AGENT, "map": r"challenger $i$", "names": ("challenger",)},
    "incumbent":   {"kind": AGENT, "map": r"incumbent", "names": ("incumbent",)},
    "institution": {"kind": AGENT, "map": r"institution", "names": ("institution",)},
    # objects and their readable attributes
    "rule":        {"kind": OBJECT, "map": r"published rule", "names": ("closure", "published rule")},
    "fee":         {"kind": ATTRIBUTE, "of": "rule", "map": r"$c$", "names": ("fee",)},
    "decisiveness": {"kind": ATTRIBUTE, "of": "rule", "map": r"published rule",
                     "names": ("decisive", "credential")},
    "prize":       {"kind": OBJECT, "map": r"$V$", "names": ("prize",)},
    "prize_ranking": {"kind": ATTRIBUTE, "of": "prize", "map": r"$V$", "names": ("ranking",)},
    "Q":           {"kind": OBJECT, "map": r"$Q$", "names": ("number of challengers",)},
    "profile":     {"kind": OBJECT, "map": r"$w_i$; $(w_1,\dots,w_Q)$; $w_{(j)}$, $j$",
                    "names": ("wealth",)},
    "scores":      {"kind": OBJECT, "map": r"$\sigma_i$", "names": ("score",)},
    # a state: two recorded profiles
    "change":      {"kind": STATE, "map": r"$w\mapsto w'$; $T$",
                    "names": ("pivot-spread", "change of profile")},
    # actions
    "pays":        {"kind": ACTION, "agent": "challenger", "verb": "pays", "object": "fee",
                    "map": r"$e_i$", "table": "Enter or not",
                    "names": ("enter", "paid")},
    "incumbent_pays": {"kind": ACTION, "agent": "incumbent", "verb": "pays, in M30", "object": "fee",
                       "map": r"incumbent", "table": "In M30, enter or not", "names": ("incumbent",)},
    "sets_fee":    {"kind": ACTION, "agent": "institution", "verb": "sets", "object": "fee",
                    "map": r"$c$", "table": r"Fixes $V$, $c$ and $\mu$", "names": ("fee",)},
    "publishes":   {"kind": ACTION, "agent": "institution", "verb": "publishes", "object": "rule",
                    "map": r"published rule", "table": r"Fixes $V$, $c$ and $\mu$", "names": ("rule",)},
    "admits":      {"kind": ACTION, "agent": "institution", "verb": "admits", "object": "Q",
                    "map": r"$Q$", "table": "", "names": ("number of challengers",)},
    "awards":      {"kind": ACTION, "agent": "institution", "verb": "awards", "object": "prize",
                    "reads": "scores", "map": r"$\mathbf 1\{i\text{ wins}\}$", "table": "",
                    "names": ("takes the prize", "wins")},
    # measures of actions
    "entrant_set": {"kind": MEASURE, "of": "pays", "map": r"$S$", "names": ("entrant set", "equilibrium")},
    "count":       {"kind": MEASURE, "of": "pays", "map": r"$k^*$", "names": ("entry count",)},
    "marginal":    {"kind": MEASURE, "of": "pays", "map": r"$w_{(k^*)}$", "names": ("marginal entrant",)},
    "assortative": {"kind": MEASURE, "of": "pays", "map": r"$\{(1),\dots,(k^*)\}$",
                    "names": ("assortative set", "richest")},
    "lone_payer_gain": {"kind": MEASURE, "of": "awards",
                        "map": r"$\Delta_Q(0)$; $D(Q)=\Delta_{Q+1}(0)-\Delta_Q(0)$; $W_Q$",
                        "names": ("first entrant's gain",)},
    "share":       {"kind": MEASURE, "of": "awards", "map": r"Entrants' share of the prize",
                    "names": ("entrants' share",)},
}

# Rows of the map's Players table that are not agents of the ledger, with the reason.
NOT_AGENTS = {"nature": "its draws are never observed; only the scores they produce are recorded"}

TABLE_OF_PLAYERS = {"challenger": "Challenger $i$", "incumbent": "Incumbent",
                    "institution": "The institution"}


# ---------------------------------------------------------------------------
# Relations
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

# varies: what changes; fixed: what is held; measure: the measure of an action that responds;
# effect: how, in words; given: observable conditions; premises: model assumptions that are
# constructs, in words.
RELATIONS = {
    "spread_sign": {
        "varies": ("change",), "fixed": ("rule", "prize", "Q"), "measure": "count",
        "effect": "sign set by the marginal entrant against the pivot", "given": ("marginal",),
        "premises": "the fee's utility cost falls with wealth; draws independent of wealth",
        "conclusions": ("C1", "C5"), "standing": ("new",),
        "model": ("M13", "M23", "M24", "M26"), "lit": ("L12", "L29", "L30"),
    },
    "two_ranks": {
        "varies": ("change",), "fixed": ("rule", "prize", "Q"), "measure": "count",
        "effect": "signed by the change at two ranks, outside the band", "given": ("marginal",),
        "premises": "",
        "conclusions": ("C2", "C8"), "standing": ("sharpening",),
        "model": ("M12", "M13", "M14"), "lit": ("L9",),
    },
    "count_pinned": {
        "varies": ("entrant_set",), "fixed": ("rule", "prize", "Q", "profile"), "measure": "count",
        "effect": "the same in every equilibrium", "given": ("scores",),
        "premises": "the gain from paying depends on how many pay, not on who",
        "conclusions": ("C3", "C6"), "standing": ("new",),
        "model": ("M4", "M6", "M26", "M27", "M29"), "lit": ("L2", "L3", "L5", "L25", "L28", "L29", "L31"),
    },
    "who_enters": {
        "varies": ("profile",), "fixed": ("rule", "prize", "Q"), "measure": "assortative",
        "effect": "the richest paying is always an equilibrium", "given": (),
        "premises": "unique when the next-richest's cost exceeds the gain",
        "conclusions": ("C4",), "standing": ("settled",),
        "model": ("M5", "M6", "M7", "M8"), "lit": ("L2", "L3", "L7"),
    },
    "gain_form": {
        "varies": ("scores",), "fixed": ("Q",), "measure": "lone_payer_gain",
        "effect": "the form of Ryvkin and Drugov's marginal benefit", "given": (),
        "premises": "",
        "conclusions": ("C14",), "standing": ("credited",),
        "model": ("M1", "M17", "M21"), "lit": ("L8",),
    },
    "gain_in_Q": {
        "varies": ("Q",), "fixed": ("rule", "prize"), "measure": "lone_payer_gain",
        "effect": "never falls", "given": (),
        "premises": "a uniform base component",
        "conclusions": ("C7",), "standing": ("new",),
        "model": ("M18", "M19"), "lit": ("L8", "L26"),
    },
    "closure_sign": {
        "varies": ("decisiveness",), "fixed": ("fee", "prize", "profile"), "measure": "count",
        "effect": "does not rise as the credential is less decisive", "given": ("incumbent",),
        "premises": "the incumbent holds the credential",
        "conclusions": ("C9", "C12"), "standing": ("new", "opposite"),
        "model": ("M28",), "lit": ("L20", "L22"),
    },
    "share_follows": {
        "varies": ("count", "decisiveness"), "fixed": (), "measure": "share",
        "effect": "moves with the count; does not rise as the credential is less decisive",
        "given": (), "premises": "",
        "conclusions": ("C10", "C13"), "standing": ("new",),
        "model": ("M11", "M12", "M13", "M28", "M31"), "lit": ("L23", "L25", "L28", "L29", "L32"),
    },
    "incumbent_entry": {
        "varies": ("entrant_set",), "fixed": ("rule", "prize", "Q", "profile"), "measure": "count",
        "effect": "the incumbent sits out while a challenger pays only in one of several equilibria",
        "given": ("incumbent",), "premises": "the incumbent has the lowest cost",
        "conclusions": ("C11",), "standing": ("model",),
        "model": ("M6", "M7", "M30"), "lit": (),
    },
    "fee_order": {
        "varies": ("fee",), "fixed": ("rule", "prize", "Q", "profile"), "measure": "assortative",
        "effect": "a uniform fee changes how many pay, not the order", "given": (),
        "premises": "the fee's utility cost falls with wealth",
        "conclusions": ("C15",), "standing": ("implication",),
        "model": ("M5", "M8", "M11", "M23", "M33"), "lit": (),
    },
    "prize_cap": {
        "varies": ("prize_ranking", "fee"), "fixed": ("rule", "Q", "profile"), "measure": "count",
        "effect": "the prize caps how many pay; the cap sets a fee", "given": (),
        "premises": "the cost to the next-richest against the prize",
        "conclusions": ("C16",), "standing": ("implication",),
        "model": ("M10", "M33"), "lit": (),
    },
    "compression": {
        "varies": ("change",), "fixed": ("rule", "prize", "Q"), "measure": "count",
        "effect": "read at two ranks; no general sign for less inequality", "given": ("marginal",),
        "premises": "",
        "conclusions": ("C17",), "standing": ("implication",),
        "model": ("M12", "M13", "M14", "M33"), "lit": (),
    },
    "share_benchmark": {
        "varies": ("count", "Q"), "fixed": ("rule",), "measure": "share",
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
    {"expresses": ("spread_sign", "two_ranks"),
     "text": "No paper read signs how a pivot-spread of wealth moves the entry count. With "
             "a fee that weighs less on the rich, the marginal entrant against the pivot "
             "sets the sign, and outside the band two ranks suffice, against Costrell and "
             "Loury's one quantile fixed by technology."},
    {"expresses": ("count_pinned", "who_enters"),
     "text": "Every equilibrium has the same entry count, new for finitely many challengers of "
             "differing wealth, since entering's gain depends on how many enter, not who, and "
             "the richest entering, the assortative set, settles who enters without the "
             "devices of identical-entrant models."},
    {"expresses": ("gain_form", "gain_in_Q"),
     "text": "The first entrant's gain, what entering adds to the chance of the award when no "
             "other enters, read from the scores, has Ryvkin and Drugov's form and never falls in the number of challengers with a uniform "
             "base component, which is new."},
    {"expresses": ("closure_sign",),
     "text": "Across rules that make the credential less decisive, the incumbent holding it, "
             "the entry count does not rise, a new sign opposite to the effort models "
             "read."},
    {"expresses": ("share_follows",),
     "text": "The entrants' share of the prize as a function of the entry count is new, since "
             "elsewhere outsiders cannot win or agents form a continuum, and does not rise "
             "as the credential becomes less decisive."},
    {"expresses": ("share_benchmark",),
     "text": "Only an entrants' share above its benchmark, the entry count plus one over the "
             "number of challengers plus one, shows the credential counts, as with Tilly's "
             "opportunity hoarding."},
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


def surface_faults(text):
    faults = [name for mark, name in SURFACE_MARKS.items() if mark in text]
    faults += [f"row id {r}" for r in ROW_ID.findall(text)]
    faults += [f"retired synonym {p}" for p in banned_patterns() if re.search(p, text, re.I)]
    return faults


def entities_of(r):
    return tuple(r["varies"]) + tuple(r["fixed"]) + (r["measure"],) + tuple(r["given"])


# ---------------------------------------------------------------------------
# Stamps
# ---------------------------------------------------------------------------

def stamped_sources(blocks, mrows, players):
    out = {}
    for key, e in ENTITIES.items():
        if e["map"] in mrows:
            out[f"map:{key}"] = digest(mrows[e["map"]]["text"])
    for agent, label in TABLE_OF_PLAYERS.items():
        if label in players:
            out[f"players:{agent}"] = digest(players[label])
    for r in RELATIONS.values():
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
    prose = " ".join(s["text"] for s in SENTENCES)
    return BEGIN + "\n" + textwrap.fill(prose, width=80, break_on_hyphens=False) + "\n" + END


def paragraph_in_manuscript(tex):
    a = tex.find(BEGIN)
    b = tex.find(END, a)
    if a < 0 or b < 0:
        return None
    return tex[a:b + len(END)]


STANDING_COLOURS = {"new": "#2e7d32", "sharpening": "#1565c0", "credited": "#6a1b9a",
                    "settled": "#00838f", "opposite": "#c62828", "implication": "#ef6c00",
                    "model": "#616161"}
KIND_STYLE = {AGENT: ("box", "#c8e6c9"), OBJECT: ("box", "#e3f2fd"), ATTRIBUTE: ("box", "#e3f2fd"),
              ACTION: ("hexagon", "#fff9c4"), MEASURE: ("box", "#fce4ec"), STATE: ("box", "#e3f2fd")}
KIND_TITLE = {AGENT: "Agents", OBJECT: "Objects and their readable attributes",
              ACTION: "Actions", MEASURE: "Measures of actions"}


def dot():
    out = ['digraph conclusions {', 'rankdir=LR; nodesep=0.25; ranksep=1.2; newrank=true;',
           'node [fontname="Helvetica", fontsize=11]; edge [fontname="Helvetica", fontsize=9];',
           'fontname="Helvetica";']
    columns = {AGENT: [AGENT], OBJECT: [OBJECT, ATTRIBUTE, STATE], ACTION: [ACTION], MEASURE: [MEASURE]}
    for col, kinds in columns.items():
        out.append(f'subgraph cluster_{col} {{ label="{KIND_TITLE[col]}"; style=rounded; color="#999999"; rank=same;')
        for key, e in ENTITIES.items():
            if e["kind"] in kinds:
                shape, fill = KIND_STYLE[e["kind"]]
                sub = {ATTRIBUTE: "attribute of " + ENTITIES[e["of"]]["names"][0] if e["kind"] == ATTRIBUTE else "",
                       STATE: "two recorded profiles"}.get(e["kind"], "")
                head = e["verb"] if e["kind"] == ACTION else e["names"][0]
                label = head + (f'<br/><font point-size="8">{sub}</font>' if sub else "")
                out.append(f'"{key}" [shape={shape}, style="rounded,filled", fillcolor="{fill}", label=<{label}>];')
        out.append('}')
    for key, e in ENTITIES.items():
        if e["kind"] == ACTION:
            out.append(f'"{e["agent"]}" -> "{key}" [color="black"];')
            out.append(f'"{key}" -> "{e["object"]}" [color="black"];')
            if "reads" in e:
                out.append(f'"{e["reads"]}" -> "{key}" [label="read by", color="black", style=dotted];')
        elif e["kind"] == MEASURE:
            out.append(f'"{e["of"]}" -> "{key}" [color="#880e4f", style=dotted];')
    for rid, r in RELATIONS.items():
        colour = STANDING_COLOURS[r["standing"][0]]
        label = (f'{r["effect"]}<br/><font point-size="8">{", ".join(r["conclusions"])}; '
                 f'{"/".join(r["standing"])}</font>')
        out.append(f'"rel_{rid}" [shape=ellipse, color="{colour}", penwidth=2, fontsize=9, label=<{label}>];')
        for v in r["varies"]:
            out.append(f'"{v}" -> "rel_{rid}" [color="{colour}"];')
        out.append(f'"rel_{rid}" -> "{r["measure"]}" [color="{colour}", penwidth=1.6];')
        for g in r["given"]:
            out.append(f'"{g}" -> "rel_{rid}" [color="{colour}", style=dashed, arrowhead=odot];')
    out.append('legend [shape=plaintext, fontsize=9, label=<'
               '<table border="0" cellspacing="2">'
               '<tr><td align="left"><b>Boxes</b> are observable rows of the measurement map</td></tr>'
               '<tr><td align="left">black edge, an action from its agent to its object (table of players)</td></tr>'
               '<tr><td align="left">dotted edge, a measure computed from an action\'s record</td></tr>'
               '<tr><td align="left"><b>Ellipses</b> are relations the conclusions state; colour is their standing; '
               'in from what varies, out to the measure, dashed from a condition</td></tr>'
               + "".join(f'<tr><td align="left"><font color="{c}">{s}</font></td></tr>'
                         for s, c in STANDING_COLOURS.items())
               + '</table>>];')
    out.append('}')
    return "\n".join(out)


def write():
    tex = MANUSCRIPT_TEX.read_text()
    old = paragraph_in_manuscript(tex)
    if old is None:
        sys.exit("no BEGIN/END markers in MANUSCRIPT.tex")
    MANUSCRIPT_TEX.write_text(tex.replace(old, rendered()))
    FIGURE_DOT.parent.mkdir(exist_ok=True)
    FIGURE_DOT.write_text(dot() + "\n")


# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------

def entity_faults(entities, mrows, terms, players):
    faults = []
    for key, e in entities.items():
        row = mrows.get(e["map"])
        if row is None:
            faults.append(f"entity {key} is not a row of the measurement map")
            continue
        if not observable(row["observed"]):
            faults.append(f"entity {key} is not observable; the map codes it {row['observed']!r}")
        if e["kind"] not in KINDS:
            faults.append(f"entity {key} has unknown kind {e['kind']!r}")
        for n in e["names"]:
            if n.lower() not in terms and n.lower() not in row["text"].lower():
                faults.append(f"entity {key} is called {n!r}, which is neither a Terminology term "
                              f"nor a phrase of its map row")
        if e["kind"] == AGENT and row["table"] != "player":
            faults.append(f"agent {key} is not in the map's Players table")
        if e["kind"] == ATTRIBUTE and entities.get(e.get("of"), {}).get("kind") != OBJECT:
            faults.append(f"attribute {key} is not of an object")
        if e["kind"] == MEASURE and entities.get(e.get("of"), {}).get("kind") != ACTION:
            faults.append(f"measure {key} is not of an action")
        if e["kind"] == ACTION:
            if entities.get(e.get("agent"), {}).get("kind") != AGENT:
                faults.append(f"action {key} has no agent")
            if entities.get(e.get("object"), {}).get("kind") not in (OBJECT, ATTRIBUTE):
                faults.append(f"action {key} acts on {e.get('object')}, which is not an object")
            label = TABLE_OF_PLAYERS.get(e.get("agent"))
            if label not in players:
                faults.append(f"action {key}: its agent has no row in the table of players")
            elif e["table"] and e["table"] not in players[label]:
                faults.append(f"action {key}: {e['table']!r} is not in its agent's row of the table of players")
    for symbol, row in mrows.items():
        if row["table"] == "player":
            is_agent = any(e["kind"] == AGENT and e["map"] == symbol for e in entities.values())
            if not is_agent and symbol not in NOT_AGENTS:
                faults.append(f"player row {symbol!r} of the map is neither an agent nor excused in NOT_AGENTS")
    return faults


def relation_faults(relations, entities, blocks):
    faults = []
    represented = {c for r in relations.values() for c in r["conclusions"]}
    for c in sorted((b for b in blocks if b.startswith("C")), key=lambda x: int(x[1:])):
        if c not in represented:
            faults.append(f"conclusion {c} is represented by no relation")
    for rid, r in relations.items():
        for e in entities_of(r):
            if e not in entities:
                faults.append(f"relation {rid} names {e}, which is not an entity")
        if entities.get(r["measure"], {}).get("kind") != MEASURE:
            faults.append(f"relation {rid} measures {r['measure']}, which is not a measure of an action")
        if not r["varies"]:
            faults.append(f"relation {rid} varies nothing")
        missing = [c for c in r["conclusions"] if c not in blocks]
        if not r["conclusions"] or missing:
            faults.append(f"relation {rid} cites no conclusion, or one that does not exist: {missing}")
            continue
        grounds = set().union(*(rests_on(blocks[c]) for c in r["conclusions"]))
        stray = [g for g in tuple(r["model"]) + tuple(r["lit"]) if g not in grounds]
        if stray:
            faults.append(f"relation {rid} cites {stray}, on which its conclusions do not rest")
        text = " ".join(statement(blocks[c]) for c in r["conclusions"])
        for s in r["standing"]:
            if s not in STANDING:
                faults.append(f"relation {rid} has unknown standing {s!r}")
            elif s == "implication" and not set(r["conclusions"]) <= IMPLICATIONS:
                faults.append(f"relation {rid} is an implication but cites a conclusion outside C15 to C18")
            elif STANDING[s] and not re.search(STANDING[s], text):
                faults.append(f"relation {rid} is {s!r}, which none of its conclusions states")
        if set(r["standing"]) & COMPARATIVE and not r["lit"]:
            faults.append(f"relation {rid} compares with the literature but cites no literature row")
    return faults


def sentence_faults(sentences, relations, entities):
    faults = []
    expressed = [rid for s in sentences for rid in s["expresses"]]
    for rid, r in relations.items():
        n = expressed.count(rid)
        if (r["lit"] and n != 1) or n > 1:
            faults.append(f"relation {rid} is expressed {n} times")
    for rid in sorted(set(expressed) - set(relations)):
        faults.append(f"a sentence expresses {rid}, which is not a relation")
    for n, s in enumerate(sentences, 1):
        text = s["text"].lower()
        for rid in s["expresses"]:
            r = relations.get(rid)
            if r is None:
                continue
            for e in tuple(r["varies"]) + (r["measure"],):
                if not any(name.lower() in text for name in entities[e]["names"]):
                    faults.append(f"sentence {n} does not name {entities[e]['names'][0]!r}")
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
        and not surface_faults("A pivot-spread of wealth moves the entry count.")
    )
    bad_entities = dict(ENTITIES,
                        bogus={"kind": OBJECT, "map": "$zz$", "names": ("x",)},
                        construct={"kind": OBJECT, "map": r"$\kappa(w)=u(w)-u(w-c)$", "names": ("entry cost",)},
                        misnamed={"kind": OBJECT, "map": r"$c$", "names": ("toll",)},
                        orphan={"kind": MEASURE, "of": "fee", "map": r"$S$", "names": ("entrant set",)},
                        bid={"kind": ACTION, "agent": "challenger", "verb": "bids", "object": "prize",
                             "map": r"$e_i$", "table": "Bids an amount", "names": ("bid",)})
    e = entity_faults(bad_entities, mrows, terms, players)
    entities_ok = (any("bogus is not a row" in f for f in e)
                   and any("construct is not observable" in f for f in e)
                   and any("'toll'" in f for f in e)
                   and any("orphan is not of an action" in f for f in e)
                   and any("'Bids an amount' is not in its agent's row" in f for f in e))
    bad = dict(RELATIONS)
    bad["share_benchmark"] = dict(RELATIONS["share_benchmark"], lit=("L1",))
    bad["gain_form"] = dict(RELATIONS["gain_form"], standing=("new",))
    bad["prize_cap"] = dict(RELATIONS["prize_cap"], measure="fee")
    del bad["fee_order"]
    r = relation_faults(bad, ENTITIES, blocks)
    relations_ok = (any("C15 is represented by no relation" in f for f in r)
                    and any("share_benchmark cites ['L1']" in f for f in r)
                    and any("gain_form is 'new'" in f for f in r)
                    and any("prize_cap measures fee" in f for f in r))
    s = sentence_faults(SENTENCES + [{"expresses": ("closure_sign", "nothing"), "text": "Unnamed."}],
                        RELATIONS, ENTITIES)
    sentences_ok = (any("closure_sign is expressed 2 times" in f for f in s)
                    and any("nothing, which is not a relation" in f for f in s)
                    and any("does not name" in f for f in s))
    return parsers_ok and entities_ok and relations_ok and sentences_ok


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

    for f in (entity_faults(ENTITIES, mrows, terms, players)
              + relation_faults(RELATIONS, ENTITIES, blocks)
              + sentence_faults(SENTENCES, RELATIONS, ENTITIES)):
        fail(f)

    stamps = load_stamps()
    for k, d in sorted(stamped_sources(blocks, mrows, players).items()):
        if stamps.get(k) != d:
            fail(f"{k} changed since the relations were reviewed against it; "
                 f"re-read the relations that cite it, then run --restamp")

    words = len(" ".join(s["text"] for s in SENTENCES).split())
    if not WORDS[0] <= words <= WORDS[1]:
        fail(f"the summary has {words} words, outside {WORDS[0]} to {WORDS[1]}")

    current = paragraph_in_manuscript(tex)
    if current is None:
        fail("MANUSCRIPT.tex has no rendered literature summary")
    elif current != rendered():
        fail("the paragraph in MANUSCRIPT.tex is not the ledger's rendering; run --write")

    if not FIGURE_DOT.exists() or FIGURE_DOT.read_text() != dot() + "\n":
        fail("figures/conclusions_ledger.dot is not the ledger's graph; run --write")

    if not controls(blocks, mrows, terms, players):
        fail("control: a check misses the fault it exists for")

    if report:
        kinds = {}
        for e in ENTITIES.values():
            kinds[e["kind"]] = kinds.get(e["kind"], 0) + 1
        print(f"conclusions ledger: {len(ENTITIES)} entities ("
              + ", ".join(f"{n} {k}" for k, n in kinds.items())
              + f"); {len(RELATIONS)} relations; {len(SENTENCES)} sentences, {words} words")
        used = {e for r in RELATIONS.values() for e in entities_of(r)}
        unused = sorted(k for k, e in ENTITIES.items() if k not in used and e["kind"] in (MEASURE, STATE, ATTRIBUTE))
        if unused:
            print(f"REPORT measures, states or attributes in no relation: {', '.join(unused)}")
        print("verify(): " + ("True" if ok else "False"))
    return ok


if __name__ == "__main__":
    if "--write" in sys.argv:
        write()
    elif "--restamp" in sys.argv:
        restamp()
    sys.exit(0 if verify() else 1)
