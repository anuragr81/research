# Motivation scan, 15 to 16 October 2026

The author asked what research problem Paper A1 solves, and how class matters
in social interaction in the existing literature. Every source with a cached
full text was read in full on 16 October 2026 and given a record under `lit/`,
39 records in all (`lit/README.md`). Hanson and Hawley (2011) was read on 17
October 2026 and is the fortieth. Goffman (1951), read only in the Spanish
translation, is set aside at the author's direction on 17 October 2026 and is
not cited below. In a second pass each record's Lean file
was extended to implement the source's model, with a coverage table in
`CLAIMS.md` that lists every numbered result as proved in general, proved in a
special case, or not formalised with a reason. Quotation IDs below, such as
PS-Q4, are rows of those records, and Lean names are theorems in
`lean/mathlib/`.

Access marks: **[F]** full text read with a record, **[C]** known only through
a citing source that was read.

## Paper A1 as the author describes it

The author corrected the coordinator's account on 16 October 2026. A1 solves an
empirical puzzle with a theoretical model that touches sociology and probably
philosophy, and offers no empirical test of its own. The model is a static Nash
equilibrium with no law of motion. Class is unseen and inferred from markers,
and the engagement utility is v − κ(c_i − c_j)². Budget compatibility is the
micro-foundation of class homophily. Avoidance depends on the observer's own
class, in two directions the author accepts: avoidance between groups runs both
ways, and a low-class member of the high group may prefer outsiders. The group
penalty vanishes when class is shown, a property to be linked to the
literature on statistical discrimination. Venues and platforms provide a
shadow market of discrimination, in the sense of the predecessor
(`main/manuscript.tex`, the platform section and line 1636). Law constrains
formal screening on group, the contracting channel, and discrimination moves
to the contact channel through social information.

## The puzzle

Chetty et al. (2022) define friending bias as "the tendency for people with low
SES to befriend people with high SES at lower rates even conditional on
exposure" (CB-Q1). They find it higher in more racially diverse schools,
colleges and neighbourhoods (CB-Q6, CB-Q8).

- **The level.** Low-SES friending bias lies above the benchmark with no
  homophily in all six settings (`ChettyEtAl2022b.excess_low`, CB-D18), by about
  0.10 in religious groups, 0.13 to 0.17 in schools, colleges, workplaces and
  recreational groups, and 0.30 in neighbourhoods. High-SES people over-befriend
  high-SES people in five settings (CB-Q16).
- **The gradient.** Across high schools friending bias by parental SES runs from
  about −1% in the least diverse ventile to about 2% in the most diverse, and
  across neighbourhoods from about 2% to about 21% (CB-D7, read from binned
  plots). No coefficient is reported.
- **What the gradient does not explain.** The authors' own explanation is that
  low- and high-SES students of different races are less likely to be friends
  (CB-Q7). Formalised as friendship only within race, it gives mean bias
  πAπB(hA − hB)²/(w(1 − w)), proportional to diversity and zero when the races
  do not differ in SES (`ChettyEtAl2022b.raceOnlyBias_formula`,
  `ChettyEtAl2022b.control_raceOnly_no_gap`). The correlation says that race
  organises friendship where race and class differ, and not why. A1 supplies
  the reason.
- **Rival accounts A1 sets itself against.** Categorisation predicts that more
  contact with a group lowers discrimination, the opposite sign (FJ-Q12).
  Biased meeting through friends, clubs and activities produces inbreeding
  homophily highest in mid-sized groups (CJP-Q2, CJP-Q12). Selective exit
  makes acting white stronger in integrated schools (EG-Q13, EG-Q19).
  Consolidation of race with class produces racial homophily from ties formed
  on class alone (MSC-Q7, `McPhersonEtAl2001.consolidation_pos`). Contact
  between groups increases the differentiation of speech rather than wearing it
  down (LV-Q19, LV-Q20).

## Budget compatibility

No source read models it. What each source assumes instead as the origin of
assortative matching or homophily:

- Vertical preference over a partner's wealth with pooled consumption, so that
  everyone prefers a richer partner (CM-Q2, CM-Q15, MP-Q5). Pooling turns a
  wealth gap into competition for the rich, and concave utility makes mixed
  couples valuable as insurance (`MailathPostlewaite2006.prop3`). Budget
  compatibility needs budgets that are not pooled.
- Complementary human capital (PF-Q8), which Pesendorfer doubts for pooled
  wealth (PF-Q14). His price screens by willingness to pay under quasi-linear
  money, never by ability to pay (`Pesendorfer1995.separating_price_iff`).
- Cultural rather than economic homophily (MSC-Q11), preference heterogeneity
  (HHA-Q18), a cost in production (RW-Q18), coordination on a public signal
  (EE-Q1, EE-Q6), and complementarity across the two sides (CF-Q21).

What supports it, in pieces:

- At equal total spending budgets are allocated differently, and visible
  spending is drawn from other categories (CH-Q19, CH-Q20). Visible spending is
  drawn from the hidden budget (`MoavNeeman2012.x_anti_floor`).
- Chetty et al. suggest priced shared activities, fraternity dues and lunch
  prices (CB-Q10, CB-Q12). The class signals in the Côté study cited by Kraus,
  Park and Tan are budget-laden (KPT-Q8).
- Aigner and Cain read the curvature of their utility as the cost of mistakes
  in assignment (AC-Q18), a mismatch cost parallel to A1's.

What limits it: in dating, own income "matters only slightly" in valuing a
partner's (HHA-Q5).

## Avoidance that depends on the observer's own class

Every source read with preferences over partners gives all observers one
ranking (PF-Q6, CM-Q2, MP-Q5, EG-Q6, CF-Q10, CJP-Q5, BIR-Q3). Pęski and Szentes
write that mutual bias "is inconsistent with statistical discrimination because
the signal value of colour must be the same for any employer, regardless of his
own colour" (PS-Q4). Under class homophily the posterior is common to all
observers and the decision it supports depends on the observer's own class.
Under A1's quadratic utility avoidance runs both ways
(`Pesendorfer1995.quadratic_avoidance_both_ways`). The decomposition of mean
squared distance to a group into the squared gap plus the within variance is
proved as `McPhersonEtAl2001.niche_decomposition`.

- **Direction (i), avoidance both ways.** Supported by racial inbreeding in
  both groups, stronger for the minority (MSC-Q20, [C] for Shrum et al.), by
  all four ethnic groups on a dating site (HHA-Q16), by the
  Côté study (KPT-Q8), and by high-SES over-befriending in Chetty et al.
  (CB-Q16). Mutual friendship
  makes the two directions one fact in pooled counts
  (`ChettyEtAl2022a.cross_links_balance`).
- **Direction (ii), a low-class member of the high group preferring
  outsiders.** Supported in voluntary groups, whose members unlike the rest
  leave (MSC-Q13), and by analogues in Pęski and Szentes (PS-Q19, PS-Q20) and
  Eeckhout (EE-Q17, EE-Q18). Limited by Eguia, whose least skilled incumbents
  are the most exclusionary (EG-Q18), by the hybrid of Cole, Mailath and
  Postlewaite, where members below the group mean gain from closure (CM-Q16),
  and by Myrdal's teacher, who stayed
  for his standing among the black elite (MLS-Q8). Passing is the mirror case,
  a high-class member of the low group joining the high group, and is
  selected on income and schooling (DNQ-Q9, MLS-Q13).
- **Evidence on the observer's class.** The accent penalty is larger among
  higher-class listeners (AB-Q8). Listeners of both races rank a dialect the
  same way (GRG-Q5), and Labov's listeners share one occupational scale
  (LV-Q6, LV-Q9).

## The penalty vanishing when class is shown

Exact statements, with their conditions, to which A1 links its property:

- Aigner and Cain. At perfect reliability the group difference disappears, and
  short of it the gap is (1 − γ)(α_W − α_B) (AC-Q13).
- Bohren, Imas and Rosenberg, Proposition 1. As the signal becomes perfectly
  objective there is initial discrimination if and only if the evaluator has a
  taste (BIR-Q7), and a taste survives (BIR-Q9). It holds for biased beliefs as
  well as correct ones.
- Laouénan and Rathelot. The ethnic gap is fully explained by statistical
  discrimination and vanishes as reviews reveal quality, for listings of the
  same quality (LR-Q2, LR-Q3, LR-Q5).
- Kim and Loury. Once the type is revealed assignment follows qualification
  alone (KLB-Q5, KLB-Q6), and fast market learning removes the group disparity
  (KLA-Q6, KLA-Q14).
- Phelps through Fang and Moro: "given skills, color does not play any
  additional role" (FM-Q18, `FangMoro2010.phelps_precise_signal`). Craig and
  Fryer give the weight on the group mean as zero for a noiseless signal
  (`CraigFryer2017.betaj_perfect_signal`).
- Hanson and Hawley, the nearest empirical statement, in rentals. Between two
  high-class e-mails the net share of landlords who answer only the White name
  is 1.77 percent and not significant, against 5.96 between two low-class
  e-mails (HH-Q1, HH-Q7, `table6_high_high`). The scope is narrower than the
  abstract says. Over all e-mails the gap between high-class e-mails is still
  4.01 points and significant, and a high-class African American name gets
  fewer replies than a low-class White name (HH-Q8, HH-D1). A reply is a
  threshold, and a high enough partial signal puts both names over it although
  the predictions still differ (`HansonHawley2011.threshold_equal_above`, our
  reading). A1's engagement rule is a threshold too, so A1 can state the
  property at the margin of engagement as well as in the limit of shown class.
  The class e-mail moves the reply rate for African American names only
  (`HansonHawley2011.table6_class_moves_one_group`), as normal updating
  predicts for the group with the wider prior and as A1's σ² term suggests.
  That link is our inference, and a ceiling on White replies would give the
  same pattern.

Limits on the property. A taste survives (BIR-Q9). A premium on a visible
attribute survives with income shown in one equilibrium of Mailath and
Postlewaite (MP-Q3). Firms search one group even under full information in
Mailath, Samuelson and Shaked [C, through Fang and Moro]. Categorical
discrimination survives where statistical discrimination would not (FJ-Q11). A
penalty written into law does not vanish when class is shown (DNQ-Q16). Class
consolidated with
race produces the same pattern without inference
(`McPhersonEtAl2001.consolidation_pos`). The empirical facts that a penalty
survives shown income in dating (HHA-Q3), shown class in speech (LV-Q14) and
skill controls (GRG-Q9) are facts A1 has to accommodate. Two rental studies
that Hanson and Hawley cite find that positive information does not close the
gap (HH-Q20, HH-Q21, [C]).

## The shadow market

No source read models the predecessor's claim that legal strictness raises the
value of shadow information. The pieces it rests on are present.

- **Neutral rules on correlated variables.** Bohren, Hull and Imas make a rule
  on a variable correlated with group carry zero direct and only systemic
  discrimination (BHI-Q7, BHI-Q14). With class as the reference, instruments
  that screen on class alone give no discrimination at all
  (`BohrenEtAl2024.neutral_rule_same_signal_no_gap`), the predecessor's remark
  that racial correlation is "a consequence rather than an input". They
  record Mayhew's argument that organisations accepted colour-blind law
  knowing that systemic discrimination would preserve the status quo
  (BHI-Q19). They also limit the predecessor. Disparate-impact doctrine reaches
  some facially neutral practices (BHI-Q20), so A1 names the contact settings
  the doctrine does not cover.
- **A banned criterion replaced by a legal one.** Class cues are sought "in ways
  that would be illegal if racial or gender cues were utilized" (KT-Q15).
  Accent is legally unprotected and a proxy for protected grounds (AB-Q16,
  ST-Q17), "poshness tests" do the screening (ST-Q16), and informal interaction
  is not covered (AB-Q17). Lundberg's employers use correlated variables to
  evade monitoring [C, FM-Q22].
- **Settings the law does not reach.** The law likely does not reach small
  Airbnb hosts (ELS-Q16), and Airbnb kept names and photos visible where hotels
  and some sellers pre-commit (ELS-Q17 to ELS-Q21). A visible proxy carries
  the penalty in proportion to how well it predicts the group once the marker
  is hidden (`LaouenanRathelot2022.proxy_gap`, our extension, LR-D10).
- **Prices and venues as social information.** A price signals the consumer
  rather than the product (CJ-Q14), clubs price above market (CJ-Q10,
  CJ-Q11), and an intermediary is valued by its clients' average type (PF-Q15).
  An anonymous price cannot separate clients who look alike
  (`RothschildWhite1993.anonymous_same_ranking`), so a variable correlated with
  group gains value once pricing on group is barred.
- **The venue operator screens for its clients' contact.** Landlords of
  apartments and condominiums discriminate and landlords of single-family homes
  do not (`HansonHawley2011.table9_nets`), because the former bear the cost if
  other tenants are displeased (HH-Q12, HH-Q13). The landlord is bound by the
  law (HH-Q19), and the tenants' contact preferences reach the decision through
  the landlord.
- **Discrimination where it leaves no record.** The venue warns that stating a
  discriminatory preference is illegal (HH-Q18). The difference lies in
  whether a landlord replies, and once a landlord replies the reply treats both
  names alike on availability and viewings (HH-Q16). Our reading is that the
  unanswered e-mail is the shadow channel. Landlords behind an anonymous
  address do not discriminate more (HH-Q17), a limit on the claim that less
  exposure raises discrimination.
- **The mirror cases.** Pesendorfer has sumptuary law substitute for fashion
  where class is identifiable (PF-Q16, PF-Q17), so allowing a formal rule
  removes the need for the signal. In the segregated South the law mandated
  discrimination on unseen ancestry, enforcement ran on markers and local
  knowledge, and passing evaded it (DNQ-Q16, DNQ-Q17).
- **Substitution after a rule.** Under a quota the gap moves into wages
  (FM-Q21). An offer rule moves the gap to applications it does not reach
  (CF-Q14). A large penalty "should activate a market for test instruments"
  (AC-Q16).

## Premises of A1 that sources read do not share

- **Group sufficiency.** Every source that measures it finds class information
  within a group: names (CR-Q8), speech within an occupation and within ethnic
  groups (Labov), speech tracking income within race (GRG-Q6), enumerators
  recording better-off Black men as mulatto (DNQ-Q12, MLS-Q6), and graded accent
  strength (ST-Q3). Class is read weakly, at 55% against a 50% chance line
  (KT-Q3), and a judge using race alone also beats chance
  (`KrausEtAl2019.race_judge_gain`). A1 states that its marker is the group as
  seen and that these cues are outside the model.
- **Consistent beliefs.** Guests' beliefs about minority listings lie below the
  actual mean, carrying 2.5 of the 3.4-point statistical penalty (LR-Q6,
  LR-Q10). Hosts use "different statistical models" (ELS-Q11).
- **Group size and gradation.** Eeckhout writes that size does not matter in
  statistical discrimination (EE-Q3). For race McPherson et al. report a
  same-different pattern rather than a graded one (MSC-Q9, [C] for Marsden).

## Not new

- The σ² term is Aigner and Cain's risk premium in a contact setting (AC-Q6,
  `AignerCain1977.contrast_quadratic_ce`).
- Price screening of an unseen type (PF-Q2, CJ-Q5).
- The condition that a group's imputed standing equal its members' actual mean
  (KLI-Q10, AS-Q5, `CorneoJeanne1994.eq8`, CF-Q4, CF-Q5).
- Passing lowering the visible mean, in Kim and Loury toward the group with the
  greater return to skill rather than the better reputation (KLI-Q11,
  KLI-D2), and in Eguia (`Eguia2013.sD_lt_half`).
- "Soft evidence" in one update (`PengEtAl2010.eq9_soft_eq_virtual`).

## Not read

Loury (2002); Loury (1998), open access but refused to scripts; Epple and
Romano (1998); Rothschild and White (1995); Lang (1986); May and
Goldsmith (2018); Burdett and Coles (1997); Schwab (1986); Epple, Romano and
Sieg (2008); Mailath, Samuelson and Shaked (2000); Marsden (1988); the books
listed in `source_inventory.md`. Akerlof (1997) was read on 17 October 2026
(`lit/akerlof_1997/`).
