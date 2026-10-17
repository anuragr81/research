# The ledger diagram, 17 October 2026

A planning note, not part of the paper. It fixes the entities of Paper A1
and the actions between them, so that the ledger can be drawn as a graph.
The entities come from `notes/entities.md` (E numbers) and the Players
table of `MANUSCRIPT.tex` (P numbers). The actions are the choices of the
Players table, the operations the model rows prove, and the relationships
still to be understood (U numbers of `notes/entities.md`). The drawing is
`notes/ledger_diagram.tex`, rendered to `notes/ledger_diagram.pdf` and
`notes/ledger_diagram.png`.

## 1. Rules of the drawing

- A node is an entity. A player (blue) faces a choice. A state (grey)
  faces none. An instrument (yellow) is what a player acts through. A
  constraint (red) limits a player's choice set. The record (white) is what
  an observer of the model writes down.
- An arrow is an action of one entity on another, labelled by the verb and
  by the rows that settle it or by the open relationship.
- Solid means a model row proves the action. Dashed means the model takes
  the action as a rule or an input. Dotted means open, with no row,
  assumption or map object.
- Entities that `notes/entities.md` lists as cases of another entity are
  drawn as arrows, not nodes. The passer (E7) is the arrow "passes" from
  the person met to the outsider group, and the low-class host member (E6)
  is the arrow "prefers outsiders" from the observer to the outsider group.
  The marker (E12) and the class signal (E13) are the two arrows from the
  person met to the observer. Shadow information (E15) is the arrow from
  the clients to the venue.

## 2. Entities

| Node | Kind | Entity | Players table | Choice drawn as outgoing arrows |
|---|---|---|---|---|
| Person met | Player | E1, with the passer E7 as a case | P2 when passing | Shows the marker, shows class, passes, engages in return, pays the venue's price, class affords the budget |
| Observer | Player | E2, with the low-class host member E6 as a case | P1 | Engages or refuses, engages both ways, prefers outsiders, class affords the budget, refusals reach the record |
| Pair | Player | E3 | P6 | Sets the shared level, friendships reach the record |
| Venue or platform | Player | E10 | P5 | Admits or excludes, screens the clients, admissions reach the record |
| Clients | Player in the predecessor, input in M15 | E11 | None | Pay and self-select, know which venue is right |
| Host group | State | E5 | P4 | None. Its standing and spread enter the observer's decision and the venue's margin |
| Outsider group | State | E4 | P3 | None. Its standing and spread enter the observer's decision |
| Budget | Instrument | E8 | None | None. It pays the activity level |
| Shared activity | Instrument | E9 | None | None |
| Law | Constraint | E14 | None | Bars admission on group |
| Setting | Constraint | E16 | None | Fixes who can meet whom |
| Record | Record | E17 | None | None |

## 3. Actions

| From | To | Action | Rows | Status |
|---|---|---|---|---|
| Person met | Observer | Shows the marker, read as evidence on group alone | A4, M1 to M3, M17 | Settled |
| Observer | Person met | Engages or refuses by own class and expected distance | A14, M18 | Settled |
| Person met | Observer | Shows class within the group, the penalty vanishes | U5 | Open |
| Host group | Observer | Standing and spread enter the expected distance | M18, U12 | Settled |
| Outsider group | Observer | Standing and spread enter the expected distance | M18, U12 | Settled |
| Observer | Outsider group | A low-class host member prefers outsiders | U3 | Open |
| Person met | Outsider group | Passes, leaving the visible group, the standing falls | A12, M14, U6 | Rule taken as given |
| Observer | Pair | Engages, both ways | U2 | Open |
| Person met | Pair | Engages in return | U2 | Open |
| Person met | Budget | Class affords the budget | A15, M19 | Settled |
| Observer | Budget | Class affords the budget | A15, M19 | Settled |
| Budget | Shared activity | Pays the level from the own budget, the ideal level rises in the budget | M19 | Settled |
| Pair | Shared activity | Sets the midpoint level, each side loses the sensitivity times the squared class gap | M19 | Settled |
| Setting | Pair | Fixes who can meet whom | U11 | Open |
| Law | Venue | Bars admission on group, leaves price, location and atmosphere | U8 | Open |
| Host group | Venue | The host standing enters the admission margin | M15 | Input taken as given |
| Clients | Venue | Pay, self-select on atmosphere | A13, U7 | Input taken as given |
| Clients | Venue | Know which venue is right, shadow information | U9 | Open |
| Venue | Clients | Screens by price, location and atmosphere | U8 | Open |
| Venue | Person met | Admits or excludes outsiders by the admission rate | A13, M15 | Rule taken as given |
| Person met | Venue | Pays the price, which may certify class | U10 | Input taken as given |
| Observer | Record | The refusal share against the standing | M18 | Settled |
| Venue | Record | Admissions | M15 | Input taken as given |
| Pair | Record | Friending bias as Chetty et al. measure it | U11 | Open |

Nine actions are settled, six are rules or inputs taken as given, and
nine are open. The settled actions all lie in the encounter, between the
person met, the observer, the groups, the budget and the activity. The
venue's actions are taken as given or open, and the pair's own action, that
both must engage, is open on both arrows.

## 4. What the drawing says about the order of work

The open arrows cluster in three places. The pair's two arrows and the
setting's arrow are the equilibrium and the puzzle (U2, U11). The class
signal's arrow is the vanishing penalty (U5). The venue's four open arrows
are the shadow market (U8, U9). The low-class host member's arrow (U3) is
a property of the settled decision and can be a row on its own. That
matches the order of `notes/entities.md`, section 3.
