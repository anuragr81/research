"""Ledger for the summary of the literature survey (MANUSCRIPT.tex, Section 4).

Entities first, then the relations among them, then the sentences. An entity is
a strand of the literature (a set of literature rows), the search of
15 October 2026, or an object of the model (a term of the Terminology appendix
with the rows that state it). A relation joins a strand to objects of the model
and records its grounds: the literature rows on the strand's side and, for a
contrast, the model rows on this paper's side. Each sentence of the summary
expresses one or more relations and names their entities in the manuscript's
words; the prose carries no row identifiers.

verify() checks that every entity and relation resolves, that every relation is
expressed exactly once, that each sentence names the entities of its relations,
that no cited row has changed since the summary was last reviewed against it,
that the prose obeys the surface rules of notes/writing_discipline.md, and that
the paragraph in MANUSCRIPT.tex is the one this ledger renders. It does not
check that a relation is a fair reading of its rows; that is what the review
before --restamp is for.

    python3 checks/ledger_literature.py              verify
    python3 checks/ledger_literature.py --write      render the paragraph into MANUSCRIPT.tex
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
LITERATURE_TEX = HERE / "LITERATURE.tex"
VERIFY_DOCS = HERE / "checks" / "verify_docs.py"
STAMPS = HERE / "checks" / "ledger_literature_stamps.json"

BEGIN = "% BEGIN literature summary, rendered by checks/ledger_literature.py --write; edit the ledger, not this text"
END = "% END literature summary"

WORDS = (180, 220)
SURFACE_MARKS = {":": "colon", "\u2014": "dash", "\u2013": "dash", " - ": "dash", "--": "dash",
                 "$": "symbol", "\\": "macro"}
ROW_ID = re.compile(r"\b[LMKC][0-9]+\b")
SEARCH_GROUND = "LITERATURE.tex:sec:search-answered"


# ---------------------------------------------------------------------------
# Entities
# ---------------------------------------------------------------------------

STRAND = "strand"          # a set of literature rows, named as the prose names it
SEARCH = "search"          # a search recorded in LITERATURE.tex
OBJECT = "object"          # an object of the model, named by its Terminology term

ENTITY_KINDS = {STRAND, SEARCH, OBJECT}

ENTITIES = {
    # strands of the literature
    "entry_models":    {"kind": STRAND, "name": "models of entry",
                        "rows": ("L2", "L3", "L4", "L6", "L7", "L24", "L29")},
    "effort_contests": {"kind": STRAND, "name": "contests with an effort margin",
                        "rows": ("L20", "L22")},
    "dispersion":      {"kind": STRAND, "name": "orders of dispersion",
                        "rows": ("L10", "L11", "L21")},
    "costrell_loury":  {"kind": STRAND, "name": "Costrell and Loury", "rows": ("L9",)},
    "wealth_effects":  {"kind": STRAND, "name": "wealth effects in contests",
                        "rows": ("L1", "L12")},
    "investment":      {"kind": STRAND, "name": "models of investment before a selection",
                        "rows": ("L25", "L27", "L28", "L30", "L32", "L33")},
    # the search
    "search":          {"kind": SEARCH, "name": "the search of 15 October 2026",
                        "rows": (SEARCH_GROUND,)},
    # objects of the model, each a term of the Terminology appendix
    "entry_count":     {"kind": OBJECT, "name": "entry count", "rows": ("K1", "M6")},
    "prize":           {"kind": OBJECT, "name": "prize", "rows": ("M11",)},
    "fee":             {"kind": OBJECT, "name": "fee", "rows": ("M11",)},
    "wealth":          {"kind": OBJECT, "name": "wealth", "rows": ("M5",)},
    "assortative_set": {"kind": OBJECT, "name": "assortative set", "rows": ("M8",)},
    "scored_as_entrant": {"kind": OBJECT, "name": "scored as an entrant", "rows": ("M28",)},
    "weight":          {"kind": OBJECT, "name": "weight on the base component", "rows": ("M28",)},
    "pivot_spread":    {"kind": OBJECT, "name": "pivot-spread", "rows": ("M13",)},
    "marginal_entrant": {"kind": OBJECT, "name": "marginal entrant", "rows": ("M12", "K2")},
    "burden_monotonicity": {"kind": OBJECT, "name": "burden-monotonicity", "rows": ("M23",)},
    "outsider":        {"kind": OBJECT, "name": "outsider", "rows": ("M26",)},
    "base_component":  {"kind": OBJECT, "name": "base component", "rows": ("M1",)},
    "bought_component": {"kind": OBJECT, "name": "bought component", "rows": ("M1",)},
    "entry_cost":      {"kind": OBJECT, "name": "entry cost", "rows": ("M23",)},
}


# ---------------------------------------------------------------------------
# Relations
# ---------------------------------------------------------------------------

GIVES = "gives"              # the strand supplies an object or result the model uses
LEAVES_OPEN = "leaves_open"  # the strand leaves a question that the model answers
OPPOSES = "opposes"          # the strand's sign is the opposite of the model's
DIFFERS = "differs"          # same object, a different mechanism or condition
NEAREST = "nearest"          # the closest strand; shares some objects, lacks others
ABSENT = "absent"            # no paper read or found combines the objects

RELATION_KINDS = {GIVES, LEAVES_OPEN, OPPOSES, DIFFERS, NEAREST, ABSENT}
CONTRASTS = {LEAVES_OPEN, OPPOSES, DIFFERS, NEAREST}   # must carry the model's side

RELATIONS = {
    "count_monotone": {
        "subject": "entry_models", "kind": GIVES, "objects": ("entry_count", "prize", "fee"),
        "lit": ("L2", "L3", "L4"), "model": ("K5", "M11"),
    },
    "prize_bound": {
        "subject": "entry_models", "kind": GIVES, "objects": ("prize", "entry_count"),
        "lit": ("L6", "L7", "L24", "L29"), "model": ("M10",),
    },
    "who_enters": {
        "subject": "entry_models", "kind": LEAVES_OPEN,
        "objects": ("wealth", "entry_count", "assortative_set"),
        "lit": ("L2", "L3"), "model": ("K1", "M8"),
    },
    "accuracy_sign": {
        "subject": "effort_contests", "kind": OPPOSES,
        "objects": ("scored_as_entrant", "weight", "entry_count"),
        "lit": ("L20", "L22"), "model": ("M28", "K8"),
    },
    "spread_order": {
        "subject": "dispersion", "kind": GIVES, "objects": ("pivot_spread",),
        "lit": ("L10", "L11", "L21"), "model": ("M13",),
    },
    "pivot_position": {
        "subject": "costrell_loury", "kind": DIFFERS,
        "objects": ("pivot_spread", "marginal_entrant"),
        "lit": ("L9",), "model": ("M12", "K2"),
    },
    "third_derivative": {
        "subject": "wealth_effects", "kind": DIFFERS, "objects": ("burden_monotonicity", "wealth"),
        "lit": ("L1", "L12"), "model": ("M23", "M5"),
    },
    "nearest_models": {
        "subject": "investment", "kind": NEAREST,
        "objects": ("outsider", "entry_count", "fee"),
        "lit": ("L25", "L27", "L28", "L30", "L32", "L33"), "model": ("M26", "M31"),
    },
    "combination": {
        "subject": "search", "kind": ABSENT,
        "objects": ("fee", "bought_component", "outsider", "base_component", "wealth", "entry_cost"),
        "lit": (), "model": ("M1", "M23"), "grounds": (SEARCH_GROUND,),
    },
}


# ---------------------------------------------------------------------------
# Sentences
# ---------------------------------------------------------------------------

SENTENCES = [
    {"expresses": ("count_monotone", "prize_bound"),
     "text": "Models of entry give the entry count's monotonicity in the prize and the "
             "fee and its bound by the prize."},
    {"expresses": ("who_enters",),
     "text": "With identical challengers they leave who enters unexplained, where here "
             "wealth pins the entry count and entry by the assortative set is always an "
             "equilibrium."},
    {"expresses": ("accuracy_sign",),
     "text": "In contests with an effort margin a more accurate ranking of the costly "
             "choice can drive contestants out once effort competes the rents away, where "
             "here, with the incumbent scored as an entrant, a lower weight on the base "
             "component never lowers the entry count."},
    {"expresses": ("spread_order", "pivot_position"),
     "text": "The pivot-spreads come from the orders of dispersion, and the position that "
             "signs their effect is fixed by technology in Costrell and "
             "Loury and set by the marginal entrant here."},
    {"expresses": ("third_derivative",),
     "text": "Wealth effects in contests turn in print on the third derivative of "
             "utility, where here burden-monotonicity, which strict concavity gives, "
             "sorts entry by wealth."},
    {"expresses": ("nearest_models",),
     "text": "Models of investment before a selection come nearest, since those who do "
             "not invest still compete, as an outsider does here, "
             "but none has an entry count of challengers who pay a fee."},
    {"expresses": ("combination",),
     "text": "No paper read or found in the search of 15 October 2026 combines a fixed "
             "fee buying the bought component, outsiders competing on the base "
             "component, a fixed number of positions awarded by rank, and wealth acting "
             "only through the entry cost."},
]


# ---------------------------------------------------------------------------
# Reading the manuscript
# ---------------------------------------------------------------------------

def row_blocks(tex):
    """Every \\lrow, \\mrow, \\krow and \\crow, mapped to its source up to the next row or table end."""
    starts = [(m.start(), m.group(1)) for m in re.finditer(r"\\[lmkc]row\{([LMKC][0-9]+)\}", tex)]
    blocks = {}
    for n, (pos, rid) in enumerate(starts):
        nxt = starts[n + 1][0] if n + 1 < len(starts) else len(tex)
        end = tex.find(r"\end{longtable}", pos)
        blocks[rid] = tex[pos:min(nxt, end) if end >= 0 else nxt]
    return blocks


def terminology_terms(tex):
    """The first column of the Terminology appendix's table."""
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


def literature_labels(tex):
    return set(re.findall(r"\\label\{([^}]*)\}", tex))


def resolve(ground, blocks, labels):
    """The source text a ground stands on, or None if it does not exist."""
    if ground.startswith("LITERATURE.tex:"):
        label = ground.split(":", 1)[1]
        if label not in labels:
            return None
        lit = LITERATURE_TEX.read_text()
        pos = lit.find(r"\label{" + label + "}")
        nxt = lit.find(r"\paragraph", pos)
        return lit[pos:nxt if nxt >= 0 else len(lit)]
    return blocks.get(ground)


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


# ---------------------------------------------------------------------------
# Grounds and stamps
# ---------------------------------------------------------------------------

def grounds_of_relation(r):
    return tuple(r["lit"]) + tuple(r["model"]) + tuple(r.get("grounds", ()))


def all_grounds():
    out = set()
    for e in ENTITIES.values():
        out |= set(e["rows"])
    for r in RELATIONS.values():
        out |= set(grounds_of_relation(r))
    return out


def current_stamps(blocks, labels):
    out = {}
    for g in all_grounds():
        src = resolve(g, blocks, labels)
        if src is not None:
            out[g] = digest(src)
    return out


def load_stamps():
    return json.loads(STAMPS.read_text()) if STAMPS.exists() else {}


def restamp():
    tex = MANUSCRIPT_TEX.read_text()
    stamps = current_stamps(row_blocks(tex), literature_labels(LITERATURE_TEX.read_text()))
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


# ---------------------------------------------------------------------------
# Checks
# ---------------------------------------------------------------------------

def entity_faults(entities, blocks, labels, terms):
    faults = []
    for key, e in entities.items():
        if e["kind"] not in ENTITY_KINDS:
            faults.append(f"entity {key} has unknown kind {e['kind']}")
        if not e["rows"]:
            faults.append(f"entity {key} rests on nothing")
        for g in e["rows"]:
            if resolve(g, blocks, labels) is None:
                faults.append(f"entity {key} cites {g}, which does not exist")
        if e["kind"] == STRAND and not all(re.fullmatch(r"L[0-9]+", g) for g in e["rows"]):
            faults.append(f"strand {key} holds a row that is not a literature row")
        if e["kind"] == OBJECT and e["name"].lower() not in terms:
            faults.append(f"object {key} is named {e['name']!r}, which is not a Terminology term")
    return faults


def relation_faults(relations, entities, blocks, labels):
    faults = []
    for rid, r in relations.items():
        subj = entities.get(r["subject"])
        if subj is None or subj["kind"] == OBJECT:
            faults.append(f"relation {rid} has subject {r['subject']}, which is not a strand or a search")
        if r["kind"] not in RELATION_KINDS:
            faults.append(f"relation {rid} has unknown kind {r['kind']}")
        for o in r["objects"]:
            if entities.get(o, {}).get("kind") != OBJECT:
                faults.append(f"relation {rid} has object {o}, which is not an object of the model")
        if subj is not None and not set(r["lit"]) <= set(subj["rows"]):
            faults.append(f"relation {rid} cites literature rows outside its subject's strand")
        if r["kind"] in CONTRASTS and not r["model"]:
            faults.append(f"relation {rid} is a contrast without the model's side")
        if r["kind"] == ABSENT and len(r["objects"]) < 2:
            faults.append(f"relation {rid} records an absent combination of fewer than two objects")
        if not grounds_of_relation(r):
            faults.append(f"relation {rid} rests on nothing")
        for g in grounds_of_relation(r):
            if resolve(g, blocks, labels) is None:
                faults.append(f"relation {rid} cites {g}, which does not exist")
    return faults


def sentence_faults(sentences, relations, entities):
    faults = []
    expressed = [rid for s in sentences for rid in s["expresses"]]
    for rid in relations:
        if expressed.count(rid) != 1:
            faults.append(f"relation {rid} is expressed {expressed.count(rid)} times, not once")
    for rid in sorted(set(expressed) - set(relations)):
        faults.append(f"a sentence expresses {rid}, which is not a relation")
    previous = ""
    for n, s in enumerate(sentences, 1):
        text = s["text"].lower()
        for rid in s["expresses"]:
            r = relations.get(rid)
            if r is None:
                continue
            subject = entities[r["subject"]]["name"].lower()
            if subject not in text and subject not in previous:
                faults.append(f"sentence {n} does not name {subject!r}, nor does the sentence before it")
            for o in r["objects"]:
                if entities[o]["name"].lower() not in text:
                    faults.append(f"sentence {n} does not name {entities[o]['name']!r}")
        f = surface_faults(s["text"])
        if f:
            faults.append(f"sentence {n} breaks the surface rules: {f}")
        previous = text
    return faults


def controls(blocks, labels):
    """Each check catches the fault it exists for."""
    sample = "\\section{Terminology}\nentrant, outsider & x & y \\\\\nfee & c & z \\\\\n\\end{longtable}"
    parsers_ok = (
        row_blocks(r"\lrow{L1}{a}\lrow{L2}{b}\end{longtable}") == {"L1": r"\lrow{L1}{a}", "L2": r"\lrow{L2}{b}"}
        and terminology_terms(sample) == {"entrant", "outsider", "fee"}
        and resolve("L99", blocks, labels) is None
        and resolve("LITERATURE.tex:no-such-label", blocks, labels) is None
        and "row id M28" in surface_faults("as M28 shows")
        and "colon" in surface_faults("one thing: another")
        and any(f.startswith("retired synonym") for f in surface_faults("the payer enters"))
        and not surface_faults("The pivot-spreads come from the orders of dispersion.")
    )
    bad_entities = dict(ENTITIES, bogus={"kind": OBJECT, "name": "no such term", "rows": ("L99",)},
                        mixed={"kind": STRAND, "name": "x", "rows": ("M1",)})
    e = entity_faults(bad_entities, blocks, labels, terminology_terms(MANUSCRIPT_TEX.read_text()))
    entities_ok = (any("not a Terminology term" in f for f in e) and any("L99" in f for f in e)
                   and any("not a literature row" in f for f in e))
    bad_relations = dict(RELATIONS, bogus={"subject": "prize", "kind": OPPOSES, "objects": ("table",),
                                           "lit": ("L1",), "model": ()})
    r = relation_faults(bad_relations, ENTITIES, blocks, labels)
    relations_ok = (any("not a strand" in f for f in r) and any("not an object" in f for f in r)
                    and any("without the model's side" in f for f in r)
                    and any("outside its subject" in f for f in r))
    bad_sentences = SENTENCES + [{"expresses": ("count_monotone", "nothing"), "text": "Nothing named here."}]
    s = sentence_faults(bad_sentences, RELATIONS, ENTITIES)
    sentences_ok = (any("count_monotone is expressed 2 times" in f for f in s)
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
    labels = literature_labels(LITERATURE_TEX.read_text())
    terms = terminology_terms(tex)

    for f in (entity_faults(ENTITIES, blocks, labels, terms)
              + relation_faults(RELATIONS, ENTITIES, blocks, labels)
              + sentence_faults(SENTENCES, RELATIONS, ENTITIES)):
        fail(f)

    stamps = load_stamps()
    for g in sorted(all_grounds()):
        src = resolve(g, blocks, labels)
        if src is not None and stamps.get(g) != digest(src):
            fail(f"{g} changed since the summary was reviewed against it; "
                 f"re-read the relations that cite it, then run --restamp")

    words = len(" ".join(s["text"] for s in SENTENCES).split())
    if not WORDS[0] <= words <= WORDS[1]:
        fail(f"the summary has {words} words, outside {WORDS[0]} to {WORDS[1]}")

    current = paragraph_in_manuscript(tex)
    if current is None:
        fail("MANUSCRIPT.tex has no rendered literature summary")
    elif current != rendered():
        fail("the paragraph in MANUSCRIPT.tex is not the ledger's rendering; run --write")

    if not controls(blocks, labels):
        fail("control: a check misses the fault it exists for")

    if report:
        lit_rows = sorted((r for r in blocks if r.startswith("L")), key=lambda r: int(r[1:]))
        placed = {g for e in ENTITIES.values() if e["kind"] == STRAND for g in e["rows"]}
        print(f"literature ledger: {len(ENTITIES)} entities, {len(RELATIONS)} relations, "
              f"{len(SENTENCES)} sentences, {words} words")
        unplaced = [r for r in lit_rows if r not in placed]
        if unplaced:
            print(f"REPORT literature rows in no strand of the summary: {', '.join(unplaced)}")
        print("verify(): " + ("True" if ok else "False"))
    return ok


if __name__ == "__main__":
    if "--write" in sys.argv:
        write()
    elif "--restamp" in sys.argv:
        restamp()
    sys.exit(0 if verify() else 1)
