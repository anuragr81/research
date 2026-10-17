# The ledger diagram, 17 October 2026

A planning note, not part of the paper. It fixes the entities of Paper A1
and the actions between them, so that the ledger can be drawn as a graph.
The E numbers are those of `notes/entities.md`, the P numbers those of the
Players table of `skeleton_ClassMarkers.tex`, and the U numbers the open relationships
of `notes/entities.md`. The drawing is `notes/ledger_diagram.tex`, rendered
to `notes/ledger_diagram.pdf` and `notes/ledger_diagram.png`. The picture is
`ledger_picture.tex`, which Figure 1 of the manuscript includes.

## 1. The decision

The author's rule, 17 October 2026: an entity is something that acts or is
acted on and is not a case of another entity. Roles, attributes and actions
are not entities. Under that rule the seventeen items of `notes/entities.md`
reduce to two entities.

| Entity | Attributes | Population state | Roles a person or platform plays |
|---|---|---|---|
| Person | class, unseen; group; marker (A4); budget $b=\beta c$ (A15) | the standing and spread of each group, $\bar c_I$, $\nu_I$, $\bar c_O$, $\nu_O$ (E4, E5) | observer (E2, P1); person met (E1, P2); passer, a person met who passes (E7); low-class host member, an observer of low class in the host group (E6); client, a host person buying from the platform (E11) |
| Platform | admission rate, price, location, atmosphere (E10, P5); the law's strictness as a constraint, which bars admission on the group (E14) | none | the setting, when the measured encounters take place in it (E16) |

What the first drawing had as nodes and this one does not.

| Former node | What it is now |
|---|---|
| Pair (E3, P6) | The action "engage, both ways", a double arrow between the observer and the person met |
| Host group, outsider group (E5, E4, P4, P3) | The population state of Person, the partition of persons by group with its standing and spread |
| Clients (E11) | A role of Person |
| Budget (E8) | An attribute of Person |
| Shared activity (E9) | The action "share one activity at the midpoint level", a double arrow between the observer and the person met |
| Law (E14) | A constraint on the platform's screening action |
| Setting (E16) | A role of Platform, the platform in which the measured encounters happen |
| Record (E17) | The measured-as column of the action table, the map's status word on the action |

The marker (E12) and the class signal (E13) were already arrows, and so was
shadow information (E15).

## 2. Rules of the drawing

- A node is an entity or a role of one. The Person box holds the role boxes
  Observer, Person met and Client, and the Population state box. The Platform
  box stands alone.
- An arrow is an action of one role on another, labelled by the verb and by
  the rows that settle it or by the open relationship. A double arrow is a
  joint action of two roles.
- Solid means a model row proves the action. Dashed means the model takes
  the action as a rule or an input. Dotted means open, with no row,
  assumption or map object.
- An action that reaches the data carries "recorded as" in its label.

## 3. Actions

| From | To | Action | Rows | Status | Recorded as |
|---|---|---|---|---|---|
| Person met | Observer | Shows the marker, read as evidence on the group alone | A4, M1 to M3, M17 | Settled | |
| Observer | Person met | Engages or refuses by own class and expected distance | A14, M18 | Settled | the refusal share $F$ against the standing (M18) |
| Person met | Observer | Shows class within the group, the penalty vanishes | U5 | Open | |
| Population state | Observer | The standing and spread of each group enter the expected distance | M18, U12 | Settled | |
| Observer | Person met | A low-class host member prefers outsiders | U3 | Open | |
| Person met | Population state | Passes, leaving the visible group, the standing falls | A12, M14, U6 | Rule taken as given | |
| Observer | Person met, both ways | Engage, both ways | U2 | Open | friendships, friending bias as Chetty et al. measure it (U11) |
| Observer | Person met, both ways | Share one activity at the midpoint level, each pays from the own budget, each loses $\kappa(c_i-c_j)^2$ | A15, M19 | Settled | |
| Platform | The encounter | As the setting, fixes who can meet whom and the group shares | U11 | Open | |
| Platform | Person met | Admits or excludes outsiders by the admission rate | A13, M15 | Rule taken as given | admissions, callbacks (M15) |
| Person met | Platform | Pays the price, which may certify class | U10 | Input taken as given | |
| Client | Platform | Pays, self-selects on atmosphere | A13, U7 | Input taken as given | |
| Client | Platform | Knows which platform is right, shadow information | U9 | Open | |
| Platform | Client | Screens by price, location and atmosphere, the law barring the group | U8 | Open | |
| Population state | Platform | The host standing enters the admission margin | M15 | Input taken as given | |

Fifteen actions. Four are settled, five are rules or inputs taken as given,
and six are open. The first drawing had twenty-four arrows. Nine of them
were merged here: the two group arrows into one from the population state,
the two arrows into the pair into one double arrow, the four budget and
activity arrows into one double arrow, the law's arrow into a constraint
on the platform's screening, and the three arrows into the record into the
measured-as column.

## 4. What the drawing says about the order of work

The settled actions are all person on person: the marker, the refusal, the
population state entering the expected distance, and the shared activity.
The two double arrows between the observer and the person met are the
heart of the model, and one of them, that both must engage, is open (U2).
The platform has no settled action. Its arrows are taken as given or open, and
its open arrows are the shadow market (U8, U9) and the setting's arrow into
the encounter (U11). That matches the order of `notes/entities.md`,
section 3: the equilibrium of the encounter first, then the class signal
(U5), then the platform.
