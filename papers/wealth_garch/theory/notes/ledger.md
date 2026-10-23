# The ledger of the theory paper, proposed, 23 October 2026

A planning note, not part of the paper. The author asked that the theory
paper's entities be identified under the ledger spec the other manuscripts
use. The spec of this bundle is `00_reader/ledger_model.py` with the
Entities and Actions tables of the parent `MEASUREMENT_MAP.tex`, agreed with
the author on 19 October 2026. It is the form of the entry-contest paper
(`papers/jite/endogenous_entry/entry_contest/checks/ledger_literature.py`),
with Paper A's status words for the arrows
(`papers/subjective_racism/paper_a/notes/ledger_diagram.md`). The ledger
below is checked by `checks/ledger_theory.py` (Suite 3 of `verify.sh`).
It replaces sections 3 and 4 of `notes/entities.md`, which reduced the
entities under Paper A's rule alone and so made equity an attribute and the
creditors one entity, against the bundle's agreed ledger.

## 1. The rules

- A node is an entity: an agent, or an object that an agent acts on. Each is
  a row of the map's Entities table, observed as Report or Rule, and named by
  an Agreed or In use term of the map's Terminology table.
- A rule is not a node. It enters through the agent who enforces it.
- Constructs and outputs are not nodes. The shortfall penalty, the asymmetry
  parameter $\lambda_S$, the value function, the dividend barrier, the
  recapitalisation trigger and the injection target are not drawn.
- An edge is an action, a row of the map's Actions table. Solid (settled)
  when a theory row proves it, dashed (given) when the model takes it as a
  rule or an input, dotted (open) when nothing in the paper carries it.
- A result is an annotation on the edge or node it concerns, resting on rows
  of `MANUSCRIPT.tex`. Every model row M1 to M12 sits on the graph.
- The theory ledger is a subgraph of the bundle's. What it leaves out is
  listed with the reason.

## 2. Entities

Six of the bundle's seven entities.

| Node | Map row | Kind | Theory player | Why it is in |
|---|---|---|---|---|
| bank | N1 | agent | P1 Bank | The agent whose policy the rows solve |
| equity | N2 | object | none | What dividends draw down and issues add to. The capital buffer is its coordinate (M9), and buffer variance is read on it (M10 to M12) |
| investors | N3 | agent | P2, "Shareholders" in the manuscript | Receive dividends and supply issues at $K$ and $\kappa$ |
| insured creditors | N4 | agent | P3, "Creditors" | Hold part of the liabilities, whose shock enters the variance limits |
| uninsured creditors | N5 | agent | P3, "Creditors" | Hold the rest. The model takes the two parts as one block (G9). For banks outside the liquidity rule they are the only enforcers of the liquidity branch (G5), the branch K2's surplus limit and C3 turn on |
| supervisor | N6 | agent | P4 Supervisor | Holds the bank to the cap and the requirement |

Left out: the insurer (N7). It is not in the model as stated, and no theory
row bears on it.

## 3. Actions

| Edge | Agent, action, entity | Status | Rows | Results on it |
|---|---|---|---|---|
| A1 | bank pays dividends to investors | settled | M1 | T1, T4 |
| A2 | investors buy stock in equity | settled | M2, A5 | T2, T3, T4 |
| A3 | investors inject capital into equity | settled | M2, A5 | T2, T3 |
| A4 | insured creditors deposit at and withdraw from the bank | given | A1 | T8 |
| A5 | bank takes risky exposure | given | A2 | T6, T7, T9 |
| A6 | supervisor supervises the bank | given | A2 | T10 |
| A7 | uninsured creditors lend to and withdraw from the bank | given | A1 | T8 |
| A9 | supervisor assigns the bank a capital category, with its restrictions | open | none | T11 |

The risky exposure (A5) is given, not settled: the rows do not derive it.
They bound every exposure within the cap and give the limits where it binds.

Left out: A8 and A11, the insurer's actions, and A10, closure at the distress
boundary, since the parent's row on the drift at the boundary is not carried.

## 4. Results on the graph

| ID | On | Kind | Rests on | Says |
|---|---|---|---|---|
| T1 | A1 | proven | M1 | Dividends are paid only at the barrier, which rises in $\lambda_S$ and in $K$ |
| T2 | A2, A3 | proven | M2, M7 | An issue starts at the trigger, rising in $\lambda_S$ and falling in $K$, and lands at a target that does not depend on where it starts and rises in $K$ |
| T3 | A2, A3 | proven | M4, M5, M6 | No kink at the trigger, convexity just above it, and finitely many discounted issues when $K>0$ |
| T4 | A1, A2 | proven | M3, K1, C1 | Where dividends are paid and where issues start identify $\lambda_S$ and $K$ |
| T5 | equity | proven | M9 | The buffer as a coordinate of equity against required capital |
| T6 | A5, equity | proven | M10, M11, K2, C2 | Where the exposure binds the cap, buffer variance tends to limits that $\lambda_S$ does not enter |
| T7 | A5, equity | proven | M12, K2 | The same quantities bound buffer variance for every compliant exposure |
| T8 | A4, A7 | proven | M10, M11, M12 | The liability shock, one block across both kinds of creditor, enters the limits and bounds |
| T9 | A5 | proven | C3, M11, M12, L2 | With no borrowing the liquidity branch never binds |
| T10 | A6 | reading | A2 | The supervisor holds the bank to the cap, a rule taken as given |
| T11 | A9 | reading | M8 | The shortfall penalty stands for the supervisor's restrictions below the reference. No row derives it |

The two headlines sit on the graph as follows. K1 is a statement about two
edges, the dividend edge and the issue edge (T4). K2 is a statement about the
exposure edge and the node it moves, equity (T6, T7), set against T1 and T2,
where $\lambda_S$ does move the thresholds.

## 5. Decisions for the author

1. **Names.** The map's agreed term is "investors", and the theory Players
   table says "Shareholders" (P2). Recommended: rename P2 to Investors, as
   the parent ledger does.
2. **Creditors.** The Players table has one player, "Creditors" (P3), and the
   map two entities. Recommended: keep one player in the table, since the
   model has one block of liabilities, and draw both nodes. The uninsured
   creditors are the only enforcers of the liquidity branch for most banks,
   which is where K2's surplus limit and C3 apply.
3. **Insurer.** Out (recommended), or in with open edges only.
4. **The shortfall penalty (T11).** Which agent's action it stands for decides
   what $\lambda_S$ means and so what K1 identifies. The parent's reading
   puts it on the supervisor's restrictions (A9), and copies the insurer's
   price (A11) and the uninsured creditors' pricing (A7) below the reference
   only. The theory ledger carries only the first, as open.
5. **A figure.** Paper A and the entry-contest paper draw the ledger as
   Figure 1. Recommended once the names are agreed. Graphviz is not installed
   in this container, so the parent's `--write` route does not render here.
   A TikZ drawing like Paper A's `ledger_picture.tex` would need nothing new.
