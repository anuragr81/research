# Entities and the relationships to be understood, 17 October 2026

A planning note, not part of the paper. It takes the ledger spec's Players
table (`papers/MANUSCRIPT_SKELETON.md`, section 4.2, with the columns ID,
Player, Choice faced, How the model settles it, In the literature and Rows),
the measurement map (`MEASUREMENT_MAP.tex`), the conclusions (C1, C10, C11,
C19) and the author's account of A1 (`notes/motivation_scan.md`, "Paper A1 as
the author describes it"), and lists every entity those documents name or
need, and every relationship between entities that a row of the restructured
paper would have to settle. Quotation IDs are rows of the records under
`lit/`, and Lean names are theorems in `lean/mathlib/`.

The IDs are the note's own. E numbers are entities and U numbers are
relationships still to be understood. Each relationship carries one of three
status words. **Settled** means a model row proves it. **Stated** means the
model takes it as a rule or an input and no row derives it. **Open** means no
row, no assumption and no object of the measurement map carries it.

## 1. Entities

The Players table has five entries, P1 to P5. The author's account needs
seventeen entities, of which four are players who face a choice, three are
states that face none, five are instruments, two are constraints and three
are records. Entities E1, E2, E7, E10 and E11 correspond to P1 to P5, and
the rest are new.

| ID | Entity | Kind | Choice faced | Where it stands now | In the literature |
|---|---|---|---|---|---|
| E1 | The person met, with unseen class $c_i$ and a marker | Player | Which marker to show (pass or stay), which venue to enter, and whether to engage in return | P2 for passing (M14). The venue choice and the return engagement have no row | The applicant of the audit studies (HH-Q3, BM-Q5), the passer of the census studies (DNQ-Q9) |
| E2 | The observer, with own class $c_j$ | Player | Engage or refuse, from the belief about $c_i$ and from $c_j$ | P1. The belief is settled (M1 to M3). The engagement decision is a rule, and the discrimination-rate function $F$ stands in for it (map, "Engagement threshold") | The employer of Phelps (L4). Every observer has one ranking in the sources read (PS-Q4) |
| E3 | The pair, the bilateral encounter | Player (two) | Engagement happens only when both engage | Not in the map. The predecessor states the bilateral condition and no row carries it | Mutual friendship in Chetty et al. (`ChettyEtAl2022a.cross_links_balance`) |
| E4 | The outsider group $O$ | State | None. Its standing $\bar c_O$ and within-group variance $\sigma_O^2$ | P3. The standing is in the map, the variance is not | The group of lower mean in every source |
| E5 | The host group $I$ | State | None. Its standing $\bar c_I$ and variance $\sigma_I^2$ | P4. As E4 | |
| E6 | A low-class member of the host group, $c_j$ well below $\bar c_I$ | Player, a case of E2 | Whether to prefer outsiders to the host's own members | Not in the Players table. The author's direction (ii) | Members unlike the rest of a voluntary group leave it (MSC-Q13). The least skilled incumbents are the most exclusionary (EG-Q18) |
| E7 | The passer, a member of E4 who suppresses the marker | Player, a case of E1 | Pass or stay | P2. The gain and cost are never observed (map) | Passing selected on income and schooling (DNQ-Q9, MLS-Q13) |
| E8 | The budget, the consumption bundle that class $c$ affords | Instrument of E1 and E2 | None. A constraint on which activities a person can share | Not in the map. The predecessor says class "determines consumption bundle" and uses it nowhere | Budgets pooled in matching models (CM-Q2, MP-Q5). Different allocation at equal spending (CH-Q19, CH-Q20) |
| E9 | The activity, the shared engagement with a price | Instrument | None. What a pair does when it engages | Not in the map. The engagement utility $v-\kappa d$ names no activity | Fraternity dues and lunch prices (CB-Q10, CB-Q12). Budget-laden class signals (KPT-Q8) |
| E10 | The venue or platform | Player | The price, location and atmosphere, and the admission policy $\sigma(m)$ | P5. Only $\sigma$ is a choice, and the objective is assumed (A13) | The landlord who screens for tenants (HH-Q12, HH-Q13). The club priced above market (CJ-Q10) |
| E11 | The venue's clients, $N$ host clients with objective weight $\alpha$ | Players in the predecessor, a state in M15 | Which venue to visit, by self-selection on atmosphere | In the map as inputs $N$ and $\alpha$. Their experience utility and self-selection have no row | Tenants whose displeasure the landlord bears (HH-Q13). An intermediary valued by its clients' mean type (PF-Q15) |
| E12 | The marker $m$, evidence about group alone | Instrument | None | In the map. Group sufficiency (A4) restricts it to the group | Names, accents, dress (BM-Q3, AB-Q16) |
| E13 | The class signal, evidence about class within a group | Instrument | None. What "class shown" means in the vanishing-penalty property | Not in the map. A4 says the marker carries none, and no object carries a second signal | The e-mail of Hanson and Hawley (HH-Q22), the address of Bertrand and Mullainathan (BM-Q18), reviews on Airbnb (LR-Q2) |
| E14 | The law, with strictness $\lambda$ | Constraint on E10 | None. It bars $\sigma$ from depending on $g$ in the contracting channel | Not in the map. The predecessor's "legal constraint" and `prop:shadow` (iii) | Class cues sought where race cues would be illegal (KT-Q15). Small hosts outside the law (ELS-Q16). Disparate impact reaches some neutral rules (BHI-Q20) |
| E15 | Shadow information $\Omega$, the value of knowing the right venue | Instrument of E11 and E1 | None. Its value is what E11 invests in | Not in the map. The predecessor's $S^\ast$ and $S^{\mathrm{rand}}$ are undefined (Appendix D, "The platform channel") | Accent as a proxy the law does not protect (AB-Q17, ST-Q16) |
| E16 | The setting of exposure, a school, college or neighbourhood | Constraint on E3 | None. Fixes who can meet whom and the group shares | Not in the map. The puzzle is measured within settings | Friending bias conditional on exposure (CB-Q1), higher in diverse settings (CB-Q6) |
| E17 | The record, what an observer of the model writes down | Record | None | The map's Directly observed column. The refusal share, the callback, the friendship count | Audit outcomes (HH-Q16), friendship counts (CB-Q1) |

## 2. Relationships to be understood

Each row names the entities, the question a row of the paper would have to
answer, the status now, and the sources that bear on it. The last column
names the heading of the author's account that the relationship serves,
among the puzzle, budget compatibility, avoidance that depends on the
observer's own class, the vanishing penalty and the shadow market.

| ID | Entities | What has to be understood | Status | Sources | Heading |
|---|---|---|---|---|---|
| U1 | E2 reads E1 | How the observer's own class and the belief about the person give the engagement decision. The expected distance to a group is $(\bar c_g-c_j)^2+\sigma_g^2$, and the observer engages when it is under the threshold. A row deriving $F$ from the threshold would replace the assumed $F$ of M14 (TODO Q12) | Stated. The belief is Settled (M1 to M3) and the decision is a rule | The $\sigma^2$ term is the risk premium (AC-Q6, `AignerCain1977.contrast_quadratic_ce`). The decomposition is `McPhersonEtAl2001.niche_decomposition`. The claim qualified is PS-Q4 | Avoidance |
| U2 | E3, both members of the pair | Who engages with whom when both must agree. A Nash equilibrium of engagement, where each person's set of partners is a best reply to everyone else's, and where the standing imputed to a group equals the actual mean of its visible members | Open. The predecessor states the bilateral condition and no row carries it | Avoidance in both groups (HHA-Q16, CB-Q16). The consistency condition (KLI-Q10, `CorneoJeanne1994.eq8`). Mutual ties pool the two directions (`ChettyEtAl2022a.cross_links_balance`) | Avoidance, the puzzle |
| U3 | E6 against E4 and E5 | When a host member below the host mean prefers outsiders. The condition in the threshold form is $(\bar c_O-c_j)^2+\sigma_O^2<(\bar c_I-c_j)^2+\sigma_I^2$, and the question is where in the host's class distribution it holds and what the host's closure does to it | Open | Support in MSC-Q13. Limits in EG-Q18, CM-Q16 and MLS-Q8. Passing is the mirror case (DNQ-Q9) | Avoidance (ii) |
| U4 | E8, E9, E1 and E2 | How separate budgets and a priced activity produce a cost that rises with the class gap. The quadratic $\kappa(c_i-c_j)^2$ is assumed, and budget compatibility is to be its micro-foundation. The budgets must not be pooled, since pooling turns a wealth gap into a preference for the richer partner | Open. No symbol in the map | Against pooling (CM-Q2, MP-Q5, `MailathPostlewaite2006.prop3`). Willingness against ability to pay (PF-Q14, `Pesendorfer1995.separating_price_iff`). Support (CH-Q19, CH-Q20, CB-Q10, CB-Q12, KPT-Q8). Limit (HHA-Q5) | Budget compatibility |
| U5 | E12 and E13 read by E2 | How a second signal, about class within the group, enters the posterior beside the marker, and when the group penalty in engagement vanishes. The property holds in the limit of a fully shown class and at the margin of the threshold with a partial signal. A1 has to say which class of statistical discrimination it shares on the return to shown class, the Phelps class or the Aigner and Cain class | Open. M1 to M3 carry the marker and no row carries a class signal | AC-Q13, BIR-Q7, LR-Q2, FM-Q18. The threshold reading (HH-D1, `HansonHawley2011.threshold_equal_above`). The two classes on returns (BM-Q13, BM-Q14, `BertrandMullainathan2004.phelps_larger_update`) | Vanishing penalty |
| U6 | E7, E4 and E2 | What a member gains by passing, once the gain is the engagement won by being read as the host, and so depends on every observer's rule from U1. The passing decision and the visible mean are then one fixed point | Stated. M14 settles the composition and takes gain and cost as given | KLI-Q10, KLI-D2, `Eguia2013.sD_lt_half`. Selection on income (DNQ-Q9) | Avoidance, the puzzle |
| U7 | E10 and E11 | How the venue's admission margin follows from its clients' engagement preferences. The willingness to pay and the composition externality of M15 are inputs, and a row would derive them from the clients' experience utility, in which social quality is a belief about the room | Stated. M15 takes the margin as given (A13) | The landlord bears the tenants' displeasure (HH-Q12, HH-Q13, `HansonHawley2011.table9_nets`). The intermediary's value (PF-Q15) | Shadow market |
| U8 | E10 under E14, through E12 | Which instruments the law leaves the venue. Admission cannot depend on the group, and the price, the location and the atmosphere screen on class and are correlated with the group. A row would state the instrument set as a function of strictness $\lambda$, and name the contact settings disparate-impact doctrine does not reach | Open. $\lambda$ is not in the map | BHI-Q7, BHI-Q14 and the limit BHI-Q20. KT-Q15, ELS-Q16, AB-Q17, ST-Q16. An anonymous price cannot separate look-alikes (`RothschildWhite1993.anonymous_same_ranking`) | Shadow market |
| U9 | E11 and E1 choose E10, through E15 | What an informed client gains over an uninformed one from knowing which venue is right, and how strictness raises that gain by degrading the uninformed fallback. The predecessor's $\Omega_j$, $S^\ast$ and $S^{\mathrm{rand}}$ are undefined, and a row would define them from the venue choice of U7 | Open | The predecessor's `prop:shadow`. No source read models strictness raising the value of shadow information (scan, "The shadow market") | Shadow market |
| U10 | E1 and E10, through E8 | What an outsider applicant pays, and whether the price certifies class. The venue-level form of U4. Price screening of an unseen type is in print, and A1's part is only the price as a budget constraint on contact | Stated. $W$ is an input | PF-Q2, CJ-Q5, CJ-Q14 | Budget compatibility, shadow market |
| U11 | E16, E3 and E17 | How the engagement equilibrium of U2 within a setting of given group shares and class distributions gives friending bias as Chetty et al. measure it, and why the bias rises with racial diversity. The rival accounts have to be ruled out by the same object | Open. The puzzle is stated in the scan and no row carries it | CB-Q1, CB-Q6, CB-Q7, `ChettyEtAl2022b.raceOnlyBias_formula`. Rivals FJ-Q12, CJP-Q2, MSC-Q7 | The puzzle |
| U12 | E4 and E5 | How the two groups' standings and variances enter every observer's decision at once. The group with the larger spread is avoided by every observer through $\sigma^2$, and the gap in standings is avoided in both directions through the squared gap. The joint determination is U2 | Settled in part. The decomposition is proved and the joint equilibrium is Open | `McPhersonEtAl2001.niche_decomposition`, `Pesendorfer1995.quadratic_avoidance_both_ways` | Avoidance |

## 3. What the relationships need from each other

U1 is the base. U2, U3, U6 and U12 are the same equilibrium seen from the
pair, from the low-class host member, from the passer and from the two
groups, so one row family settles them. U4 sits under U1 and gives the
quadratic its reason. U5 extends U1 by a second signal. U7, U8, U9 and U10
are the venue, and U7 rests on U1 through the clients' beliefs. U11 is the
puzzle, and it rests on U2 within a setting.

The order that follows is U4 and U1 first, since the utility is the basis
the author named, then U2 with U6 and U12 as one equilibrium, then U3 and U5
as its two properties, then U7 to U10 as the venue, and U11 last, written
from the rows above it.

## 4. What this means for the Players table

- E1 becomes a player in its own right, with the venue choice and the return
  engagement, and not only the passer.
- E2's engagement decision becomes a derived choice when U1 is a row, and $F$
  leaves the map.
- E3 is a new entry, the pair, with U2 as the row that settles it.
- E6 is a case of E2 and gets its own entry only if U3 is a row.
- E11 become players, with the venue choice, when U7 and U9 are rows.
- E8, E9, E13, E14, E15 and E16 enter the measurement map as inputs with
  their status words, and $\sigma_g^2$ joins the standing as a state of each
  group.
