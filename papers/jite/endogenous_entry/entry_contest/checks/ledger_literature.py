"""Ledger of the conclusions (MANUSCRIPT.tex, Section 5), and the literature summary rendered from it.

Entities first, then the relations among them, then the sentences.

  Entities   rows of MEASUREMENT_MAP.tex, each with its table (input, model-internal,
             output) and its Observed code as the map gives them.
  Relations  what the conclusions C1 to C18 say about the entities: a change in some
             entities moves, pins, orders or caps another, under conditions that are
             themselves entities. Each relation cites the conclusions it represents, the
             model and literature rows those conclusions rest on, and its standing
             against the papers read (new, a sharpening, credited, opposite, or an
             implication for the rule).
  Sentences  the summary that opens Section 4. Each sentence expresses listed relations
             and names their entities; the prose carries no row identifiers.

verify() checks that every entity is a row of the measurement map; that every
conclusion is represented by a relation; that each relation cites only rows its
conclusions rest on, and that its standing is one its conclusions state; that every
relation with literature rows is expressed exactly once in the summary; that each
sentence names the entities of its relations; that no cited row has changed since
the relations were last reviewed against it; the surface rules of
notes/writing_discipline.md; and that the paragraph in MANUSCRIPT.tex is this
ledger's rendering. It does not check that a relation is a fair reading of its
conclusions; that is what the review before --restamp is for.

    python3 checks/ledger_literature.py              verify
    python3 checks/ledger_literature.py --write      render the paragraph into MANUSCRIPT.tex
    python3 checks/ledger_literature.py --restamp    record the cited rows as reviewed
    python3 checks/ledger_literature.py --dot        print the graph in Graphviz DOT
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
# Entities: rows of the measurement map
# ---------------------------------------------------------------------------
# "map" is the row's Symbol cell exactly as MEASUREMENT_MAP.tex writes it;
# "names" are the words the prose may use for it, each a Terminology term or a
# phrase of the row itself.

ENTITIES = {
    # players, from the map's Players table
    "challenger": {"map": r"challenger $i$", "names": ("challenger",)},
    "incumbent":  {"map": r"incumbent", "names": ("incumbent",)},
    "nature":     {"map": r"nature", "names": ("nature",)},
    "institution": {"map": r"institution", "names": ("institution",)},
    # objects
    "Q":          {"map": r"$Q$", "names": ("number of challengers",)},
    "C":          {"map": r"$C$", "names": ("scored as an entrant",)},
    "w":          {"map": r"$w_i$; $(w_1,\dots,w_Q)$; $w_{(j)}$, $j$", "names": ("wealth",)},
    "c":          {"map": r"$c$", "names": ("fee",)},
    "V":          {"map": r"$V$", "names": ("prize",)},
    "mu":         {"map": r"$\mu$", "names": ("weight on the base component",)},
    "r":          {"map": r"$r_i$", "names": ("base component",)},
    "s":          {"map": r"$s_i$", "names": ("bought component",)},
    "T":          {"map": r"$w\mapsto w'$; $T$", "names": ("pivot-spread", "change of profile")},
    "x0":         {"map": r"$x_0$", "names": ("pivot",)},
    "kappa":      {"map": r"$\kappa(w)=u(w)-u(w-c)$", "names": ("entry cost",)},
    "Delta":      {"map": r"$\Delta(m)$", "names": ("gain",)},
    "cap":        {"map": r"$\Delta(m)\le V/(m{+}1)$; $k^*\le V/\kappa$", "names": ("saturation",)},
    "pinned":     {"map": r"$\kappa(w_{(k^*+1)})>\Delta(k^*-1)$", "names": ("identities pinned",)},
    "band":       {"map": r"$d_j$; the band", "names": ("band",)},
    "gain_Q":     {"map": r"$\Delta_Q(0)$; $D(Q)=\Delta_{Q+1}(0)-\Delta_Q(0)$; $W_Q$",
                   "names": ("first entrant's gain",)},
    "e":          {"map": r"$e_i$", "names": ("enter",)},
    "S":          {"map": r"$S$", "names": ("entrant set", "equilibrium")},
    "kstar":      {"map": r"$k^*$", "names": ("entry count",)},
    "marginal":   {"map": r"$w_{(k^*)}$", "names": ("marginal entrant",)},
    "assortative": {"map": r"$\{(1),\dots,(k^*)\}$", "names": ("assortative set",)},
    "share":      {"map": r"Entrants' share of the prize", "names": ("entrants' share",)},
}


# ---------------------------------------------------------------------------
# Options: what each player may choose (manuscript, table of players, tab:players)
# ---------------------------------------------------------------------------
# "table1" is the phrase of the player's row in that table that names the option.

OPTIONS = {
    "challenger":  {"verb": "chooses", "options": ("e",), "table1": "Enter or not"},
    "incumbent":   {"verb": "chooses in M30", "options": ("e",), "table1": "In M30, enter or not"},
    "nature":      {"verb": "draws", "options": ("r", "s"), "table1": "Draws $r_i$ and $s_i$"},
    "institution": {"verb": "sets", "options": ("V", "c", "mu"), "table1": "Fixes $V$, $c$ and $\\mu$"},
}
TABLE1_ROWS = {"challenger": "Challenger $i$", "incumbent": "Incumbent", "nature": "Nature",
               "institution": "The institution"}


# ---------------------------------------------------------------------------
# Relations: what the conclusions say about the entities
# ---------------------------------------------------------------------------

SIGN_SET_BY = "sign set by"
NON_INCREASING = "does not rise in"
FOLLOWS = "moves with"
PINNED = "pinned by"
SELECTS = "selects"
ORDERS = "orders"
CAPS = "caps"
NEVER_FALLS = "never falls in"
FORM = "takes the form"
SHOWS = "shows"

EFFECTS = {SIGN_SET_BY, NON_INCREASING, FOLLOWS, PINNED, SELECTS, ORDERS, CAPS, NEVER_FALLS,
           FORM, SHOWS}

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

RELATIONS = {
    "spread_sign": {
        "from": ("T", "x0"), "effect": SIGN_SET_BY, "to": "kstar", "given": ("marginal", "kappa"),
        "conclusions": ("C1", "C5"), "standing": ("new",),
        "model": ("M13", "M23", "M24", "M26"),
        "lit": ("L12", "L29", "L30"),
    },
    "two_ranks": {
        "from": ("T",), "effect": SIGN_SET_BY, "to": "kstar", "given": ("marginal", "band"),
        "conclusions": ("C2", "C8"), "standing": ("sharpening",),
        "model": ("M12", "M13", "M14"), "lit": ("L9",),
    },
    "count_pinned": {
        "from": ("Delta",), "effect": PINNED, "to": "kstar", "given": ("w",),
        "conclusions": ("C3", "C6"), "standing": ("new",),
        "model": ("M4", "M6", "M26", "M27", "M29"),
        "lit": ("L2", "L3", "L5", "L25", "L28", "L29", "L31"),
    },
    "who_enters": {
        "from": ("w", "kappa"), "effect": SELECTS, "to": "assortative", "given": ("pinned",),
        "conclusions": ("C4",), "standing": ("settled",),
        "model": ("M5", "M6", "M7", "M8"), "lit": ("L2", "L3", "L7"),
    },
    "gain_form": {
        "from": ("Delta",), "effect": FORM, "to": "gain_Q", "given": (),
        "conclusions": ("C14",), "standing": ("credited",),
        "model": ("M1", "M17", "M21"), "lit": ("L8",),
    },
    "gain_in_Q": {
        "from": ("Q",), "effect": NEVER_FALLS, "to": "gain_Q", "given": ("r",),
        "conclusions": ("C7",), "standing": ("new",),
        "model": ("M18", "M19"), "lit": ("L8", "L26"),
    },
    "weight_sign": {
        "from": ("mu",), "effect": NON_INCREASING, "to": "kstar", "given": ("C",),
        "conclusions": ("C9", "C12"), "standing": ("new", "opposite"),
        "model": ("M28",), "lit": ("L20", "L22"),
    },
    "share_follows": {
        "from": ("kstar", "mu"), "effect": FOLLOWS, "to": "share", "given": (),
        "conclusions": ("C10", "C13"), "standing": ("new",),
        "model": ("M11", "M12", "M13", "M28", "M31"),
        "lit": ("L23", "L25", "L28", "L29", "L32"),
    },
    "incumbent_entry": {
        "from": ("kappa",), "effect": SELECTS, "to": "S", "given": ("C", "pinned"),
        "conclusions": ("C11",), "standing": ("model",),
        "model": ("M6", "M7", "M30"), "lit": (),
    },
    "fee_order": {
        "from": ("c", "w"), "effect": ORDERS, "to": "assortative", "given": ("kappa",),
        "conclusions": ("C15",), "standing": ("implication",),
        "model": ("M5", "M8", "M11", "M23", "M33"), "lit": (),
    },
    "prize_cap": {
        "from": ("V", "kappa"), "effect": CAPS, "to": "kstar", "given": ("cap",),
        "conclusions": ("C16",), "standing": ("implication",),
        "model": ("M10", "M33"), "lit": (),
    },
    "compression": {
        "from": ("T", "x0"), "effect": SIGN_SET_BY, "to": "kstar", "given": ("marginal", "band"),
        "conclusions": ("C17",), "standing": ("implication",),
        "model": ("M12", "M13", "M14", "M33"), "lit": (),
    },
    "share_benchmark": {
        "from": ("share", "kstar", "Q"), "effect": SHOWS, "to": "s", "given": (),
        "conclusions": ("C18",), "standing": ("implication",),
        "model": ("M31", "M33"), "lit": ("L19",),
    },
}


# ---------------------------------------------------------------------------
# Sentences: the summary that opens Section 4
# ---------------------------------------------------------------------------

SENTENCES = [
    {"expresses": ("spread_sign", "two_ranks"),
     "text": "No paper read signs how a pivot-spread of wealth moves the entry count. Here, "
             "given anonymity and an entry cost that falls with wealth, the marginal entrant against the pivot sets the sign, and outside "
             "the band two ranks suffice where Costrell and Loury use one quantile fixed by "
             "technology."},
    {"expresses": ("count_pinned", "who_enters"),
     "text": "Every equilibrium has the same entry count, new for finitely many challengers "
             "whose entry costs differ with wealth, because the gain depends on the count "
             "alone, and the assortative set settles who enters without the devices of "
             "models with identical entrants."},
    {"expresses": ("gain_form", "gain_in_Q"),
     "text": "The first entrant's gain has the form of Ryvkin and Drugov's marginal benefit, "
             "and that it never falls in the number of challengers with a uniform base "
             "component is new."},
    {"expresses": ("weight_sign",),
     "text": "With the incumbent scored as an entrant, the entry count does not rise in the "
             "weight on the base component, a new sign, opposite to the effort models read."},
    {"expresses": ("share_follows",),
     "text": "The entrants' share of the prize as a function of the entry count is new, since "
             "elsewhere outsiders cannot win or agents form a continuum, and it does "
             "not rise in the weight on the base component."},
    {"expresses": ("share_benchmark",),
     "text": "Only an entrants' share above its benchmark, the entry count plus one over the "
             "number of challengers plus one, shows that the bought component counts, which "
             "bears on Tilly's opportunity hoarding."},
]


# ---------------------------------------------------------------------------
# Reading the documents
# ---------------------------------------------------------------------------

def map_rows(tex):
    """Symbol cell -> {table, observed, text}, for every row of the map's three tables."""
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


def table1_rows(tex):
    """Player label -> its row of the table of players (tab:players)."""
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


def entities_of(r):
    return tuple(r["from"]) + (r["to"],) + tuple(r["given"])


# ---------------------------------------------------------------------------
# Stamps
# ---------------------------------------------------------------------------

def stamped_sources(blocks, mrows, t1=None):
    out = {}
    for player, label in TABLE1_ROWS.items():
        if t1 and label in t1:
            out[f"table1:{player}"] = digest(t1[label])
    for key, e in ENTITIES.items():
        if e["map"] in mrows:
            out[f"map:{key}"] = digest(mrows[e["map"]]["text"])
    for r in RELATIONS.values():
        for c in r["conclusions"]:
            if c in blocks:
                out[c] = digest(blocks[c])
    return out


def load_stamps():
    return json.loads(STAMPS.read_text()) if STAMPS.exists() else {}


def restamp():
    tex = MANUSCRIPT_TEX.read_text()
    stamps = stamped_sources(row_blocks(tex), map_rows(MAP_TEX.read_text()), table1_rows(tex))
    STAMPS.write_text(json.dumps(dict(sorted(stamps.items())), indent=1) + "\n")


# ---------------------------------------------------------------------------
# Rendering
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


def write():
    tex = MANUSCRIPT_TEX.read_text()
    old = paragraph_in_manuscript(tex)
    if old is None:
        sys.exit("no BEGIN/END markers in MANUSCRIPT.tex")
    MANUSCRIPT_TEX.write_text(tex.replace(old, rendered()))
    FIGURE_DOT.parent.mkdir(exist_ok=True)
    FIGURE_DOT.write_text(dot() + "\n")


def observed_code(cell):
    """The first of R, C, N, K that the map's Observed cell gives."""
    m = re.search(r"\b([RCNK])\b", cell)
    return m.group(1) if m else "?"


OBSERVED_COLOURS = {"R": "#c8e6c9", "C": "#bbdefb", "N": "#eeeeee", "K": "#ffe0b2"}
STANDING_COLOURS = {"new": "#2e7d32", "sharpening": "#1565c0", "credited": "#6a1b9a",
                    "settled": "#00838f", "opposite": "#c62828", "implication": "#ef6c00",
                    "model": "#616161"}


def dot():
    mrows = map_rows(MAP_TEX.read_text())
    out = ['digraph conclusions {', 'rankdir=LR; nodesep=0.3; ranksep=1.4; newrank=true;',
           'node [fontname="Helvetica", fontsize=11]; edge [fontname="Helvetica", fontsize=9];',
           'fontname="Helvetica";']
    titles = {"player": "Players", "input": "Inputs", "internal": "Model-internal variables", "output": "Outputs"}
    for table, _ in MAP_TABLES:
        out.append(f'subgraph cluster_{table} {{ label="{titles[table]}"; style=rounded; color="#999999"; rank=same;')
        for key, e in ENTITIES.items():
            row = mrows.get(e["map"])
            if row and row["table"] == table:
                code = observed_code(row["observed"])
                out.append(f'"{key}" [shape=box, style="rounded,filled", '
                           f'fillcolor="{OBSERVED_COLOURS.get(code, "white")}", '
                           f'label=<{e["names"][0]}<br/><font point-size="9">observed {code}</font>>];')
        out.append('}')
    for player, o in OPTIONS.items():
        for opt in o["options"]:
            out.append(f'"{player}" -> "{opt}" [color="black", penwidth=1.4, label="{o["verb"]}", '
                       f'fontsize=8];')
    for rid, r in RELATIONS.items():
        colour = STANDING_COLOURS[r["standing"][0]]
        label = f'{r["effect"]}<br/><font point-size="8">{", ".join(r["conclusions"])}; {"/".join(r["standing"])}</font>'
        out.append(f'"rel_{rid}" [shape=ellipse, color="{colour}", penwidth=2, fontsize=10, label=<{label}>];')
        for f in r["from"]:
            out.append(f'"{f}" -> "rel_{rid}" [color="{colour}"];')
        out.append(f'"rel_{rid}" -> "{r["to"]}" [color="{colour}", penwidth=1.6];')
        for g in r["given"]:
            out.append(f'"{g}" -> "rel_{rid}" [color="{colour}", style=dashed, arrowhead=odot];')
    out.append('legend [shape=plaintext, fontsize=9, label=<'
               '<table border="0" cellspacing="2">'
               '<tr><td align="left"><b>Fill: Observed code of the map</b></td></tr>'
               '<tr><td bgcolor="#c8e6c9" align="left">R recorded</td></tr>'
               '<tr><td bgcolor="#bbdefb" align="left">C computed from records</td></tr>'
               '<tr><td bgcolor="#eeeeee" align="left">N not observed, a fact</td></tr>'
               '<tr><td bgcolor="#ffe0b2" align="left">K not observed, a construct</td></tr>'
               '<tr><td align="left"><b>Black: a player\'s option</b> (table of players)</td></tr>'
               '<tr><td align="left"><b>Relation colour: standing</b> (C rows; dashed = condition)</td></tr>'
               + "".join(f'<tr><td align="left"><font color="{c}">{s}</font></td></tr>'
                         for s, c in STANDING_COLOURS.items())
               + '</table>>];')
    out.append('}')
    return "\n".join(out)


# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------

def entity_faults(entities, mrows, terms):
    faults = []
    for key, e in entities.items():
        row = mrows.get(e["map"])
        if row is None:
            faults.append(f"entity {key} is not a row of the measurement map")
            continue
        for n in e["names"]:
            if n.lower() not in terms and n.lower() not in row["text"].lower():
                faults.append(f"entity {key} is called {n!r}, which is neither a Terminology term "
                              f"nor a phrase of its map row")
    return faults


def option_faults(options, entities, mrows, t1):
    faults = []
    for player, o in options.items():
        row = mrows.get(entities.get(player, {}).get("map"))
        if row is None or row["table"] != "player":
            faults.append(f"{player} has options but is not a row of the map's Players table")
        label = TABLE1_ROWS.get(player)
        if label not in t1:
            faults.append(f"{player} has no row in the table of players")
        elif o["table1"] not in t1[label]:
            faults.append(f"{player}'s option {o['table1']!r} is not in its row of the table of players")
        for opt in o["options"]:
            orow = mrows.get(entities.get(opt, {}).get("map"))
            if orow is None or orow["table"] == "player":
                faults.append(f"{player}'s option {opt} is not an object of the map")
    for key, e in entities.items():
        row = mrows.get(e["map"])
        if row and row["table"] == "player" and key not in options:
            faults.append(f"player {key} has no option recorded")
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
        if r["effect"] not in EFFECTS:
            faults.append(f"relation {rid} has unknown effect {r['effect']!r}")
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
        if any(s in ("new", "sharpening", "credited", "opposite", "settled") for s in r["standing"]) \
                and not r["lit"]:
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
            for e in (r["to"],) + tuple(r["from"]):
                if not any(name.lower() in text for name in entities[e]["names"]):
                    faults.append(f"sentence {n} does not name {entities[e]['names'][0]!r}")
        f = surface_faults(s["text"])
        if f:
            faults.append(f"sentence {n} breaks the surface rules: {f}")
    return faults


def controls(blocks, mrows, terms):
    """Each check catches the fault it exists for."""
    parsers_ok = (
        row_blocks(r"\crow{C1}{a}{M1, L2}\crow{C2}{b}{M3}\end{longtable}")
        == {"C1": r"\crow{C1}{a}{M1, L2}", "C2": r"\crow{C2}{b}{M3}"}
        and rests_on(r"\crow{C1}{the statement M9}{M1, L2}") == {"M1", "L2"}
        and "row id M28" in surface_faults("as M28 shows")
        and "colon" in surface_faults("one thing: another")
        and any(f.startswith("retired synonym") for f in surface_faults("the payer enters"))
        and not surface_faults("A pivot-spread of wealth moves the entry count.")
    )
    e = entity_faults(dict(ENTITIES, bogus={"map": "$zz$", "names": ("x",)},
                           misnamed={"map": r"$c$", "names": ("toll",)}), mrows, terms)
    entities_ok = (any("bogus is not a row" in f for f in e) and any("'toll'" in f for f in e))
    bad = dict(RELATIONS)
    bad["share_benchmark"] = dict(RELATIONS["share_benchmark"], lit=("L1",))
    bad["gain_form"] = dict(RELATIONS["gain_form"], standing=("new",))
    bad["prize_cap"] = dict(RELATIONS["prize_cap"], to="nothing")
    del bad["fee_order"]
    r = relation_faults(bad, ENTITIES, blocks)
    relations_ok = (any("C15 is represented by no relation" in f for f in r)
                    and any("share_benchmark cites ['L1']" in f for f in r)
                    and any("gain_form is 'new'" in f for f in r)
                    and any("names nothing" in f for f in r))
    s = sentence_faults(SENTENCES + [{"expresses": ("weight_sign", "nothing"), "text": "Unnamed."}],
                        RELATIONS, ENTITIES)
    sentences_ok = (any("weight_sign is expressed 2 times" in f for f in s)
                    and any("nothing, which is not a relation" in f for f in s)
                    and any("does not name" in f for f in s))
    t1 = table1_rows(MANUSCRIPT_TEX.read_text())
    o = option_faults(dict(OPTIONS, nature=dict(OPTIONS["nature"], table1="Bids an amount"),
                           c={"verb": "x", "options": ("challenger",), "table1": ""}),
                      ENTITIES, mrows, t1)
    options_ok = (any("'Bids an amount' is not in its row" in f for f in o)
                  and any("c has options but is not a row" in f for f in o)
                  and any("option challenger is not an object" in f for f in o))
    return parsers_ok and entities_ok and relations_ok and sentences_ok and options_ok


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
    t1 = table1_rows(tex)

    for f in (entity_faults(ENTITIES, mrows, terms)
              + option_faults(OPTIONS, ENTITIES, mrows, t1)
              + relation_faults(RELATIONS, ENTITIES, blocks)
              + sentence_faults(SENTENCES, RELATIONS, ENTITIES)):
        fail(f)

    stamps = load_stamps()
    for k, d in sorted(stamped_sources(blocks, mrows, t1).items()):
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

    if not controls(blocks, mrows, terms):
        fail("control: a check misses the fault it exists for")

    if report:
        used = {e for r in RELATIONS.values() for e in entities_of(r)}
        used |= set(OPTIONS) | {x for o in OPTIONS.values() for x in o["options"]}
        tables = {}
        for key in used:
            tables.setdefault(mrows.get(ENTITIES[key]["map"], {}).get("table", "?"), []).append(key)
        print(f"conclusions ledger: {len(ENTITIES)} entities "
              + ", ".join(f"{len(v)} {k}" for k, v in sorted(tables.items()))
              + f"; {len(RELATIONS)} relations; {len(SENTENCES)} sentences, {words} words")
        unused = sorted(set(ENTITIES) - used)
        if unused:
            print(f"REPORT entities in no relation: {', '.join(unused)}")
        print("verify(): " + ("True" if ok else "False"))
    return ok


if __name__ == "__main__":
    if "--dot" in sys.argv:
        print(dot())
        sys.exit(0)
    if "--write" in sys.argv:
        write()
    elif "--restamp" in sys.argv:
        restamp()
    sys.exit(0 if verify() else 1)
