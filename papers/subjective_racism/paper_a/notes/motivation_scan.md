# Motivation scan, 15 and 16 October 2026

The author asked what research problem Paper A1 solves, and how class matters
in social interaction in the existing literature. Three web searches on 15
October 2026 found the sources. On 16 October 2026 every source with a cached
full text was read in full and given a record under `lit/`, 39 records in all
(`lit/README.md`). Quotation IDs below, such as PS-Q4, are rows of those
records. Any other source still needs a full read and a record before a
manuscript row rests on it.

Access marks: **[F]** full text read with a record, **[C]** known only through
a citing source that was read, **[A]** abstract or summary only, **[S]** quoted
through a search result only.

## What the literature already has

| Point | Sources |
|---|---|
| Class is read from signs such as speech, dress and manners, the reading sets treatment, and the signs can be imitated. | Goffman (1951, Spanish translation) [F]; Labov (1964 dissertation) [F]; Kraus et al. (2019) [F]; Kraus, Park and Tan (2017) [F]; Levon et al. (2020, 2022) [F] |
| Racially distinctive names are read as class. | Crabtree et al. (2022) [F] |
| Observers impute a group's characteristics to its members, and a risk-averse observer discounts a noisier group. | Phelps (1972) [F]; Aigner and Cain (1977) [F]; Fang and Moro (2010 WP) [F] |
| Signals and payoff-irrelevant attributes sort partners in social matching. | Pesendorfer (1995) [F]; Cole, Mailath and Postlewaite (1991 WP) [F]; Mailath and Postlewaite (2006 WP) [F]; Eeckhout (2006) [F]; Pęski and Szentes (2012 WP) [F] |
| Passing selects the abler members into the group with the greater return to skill, and the better-off passed more in the census. | Kim and Loury (2012) [F]; Eguia (2013 WP) [F]; Dahis, Nix and Qian (2019) [F]; Mill and Stein (2012 WP) [F] |
| Homophily by status and by race, with race the strongest, and the separation of baseline from inbreeding homophily. | McPherson, Smith-Lovin and Cook (2001) [F]; Currarini, Jackson and Pin (2008 WP) [F] |
| Venues price on the composition of their clientele. | Rothschild and White (1993 chapter) [F]; Epple and Romano (1998) [A] |
| Price screening certifies an unseen type. | Pesendorfer (1995) [F]; Corneo and Jeanne (1994 DP) [F]; Goffman (1951) [F] in words |
| A belief-based group penalty vanishes once the type is shown, and a taste-based one survives. | Aigner and Cain (1977) [F]; Bohren, Imas and Rosenberg (2019 WP) [F]; Laouénan and Rathelot (2022) [F]; Kim and Loury (2009, 2014 WPs) [F] |
| Jeffrey's rule in one update equals Bayes with a virtual likelihood. | Peng, Zhang and Pan (2010) [F]; Chan and Darwiche (2005) [C] |

## The empirical hook

Chetty et al. (2022, Social Capital II) define friending bias as "the tendency
for people with low SES to befriend people with high SES at lower rates even
conditional on exposure" (CB-Q1). They report that "Friending bias is also
higher in more racially diverse schools" (CB-Q6), and the same for colleges
and neighbourhoods (CB-Q8). The authors suggest a mechanism from race to class
(CB-Q7). A1 proposes the reverse, class inferred from race. Both predict the
same sign, so the correlation alone does not separate them.

The size of the gradient is read from binned plots, since no coefficient is
reported (CB-D7). In high schools friending bias by parental SES runs from
about −1% in the least diverse ventile to about 2% in the most diverse. The
authors' own benchmark with no homophily is −10% in a school half of high SES
(p. 16, `ChettyEtAl2022b.benchmark_values`). Friending bias in the least
diverse schools is therefore about 9 points above the benchmark, and racial
diversity adds about 3 more. A marker that is race alone can account for at
most the increment, not the level. The same figure shows friending bias rising
with cohort size, which is correlated with diversity, and with tracking into
advanced classes. These readings are ours, from Extended Data Fig. 1, and are
not stated by the authors.

Chetty et al. offer two costs as suggestions, fraternity dues (CB-Q10) and
lunchrooms split by "options for low-cost and free lunch" (CB-Q12). The NBER
version adds prom tickets (CB-D8 to CB-D12). Social Capital I measures class
homophily directly (CA-Q3), and friendship falls with distance in SES
(CA-Q5).

## Whether the evidence supports A1's premises

1. **Class is unseen.** Class is read from brief speech at 55% against a 50%
   chance line, and less accurately than race (KT-Q3, KT-Q4). Correlations of
   perceived with actual class run from .21 to .28 (Kraus, Park and Tan). The
   premise holds as an approximation. A judge who uses race alone also beats
   chance whenever class differs by race (`KrausEtAl2019.race_judge_gain`), so
   accuracy above chance cannot separate the two channels.
2. **Group sufficiency.** Every source that measures it finds class
   information within a group. Names within one group are read at different
   class levels (CR-Q8). Speech is stratified by class within an occupation
   and within each ethnic group (Labov), and within race it tracks income and
   schooling (GRG-Q6). Census enumerators recorded better-off Black men as
   mulatto (DNQ-Q12, MLS-Q6). Accent strength is graded (ST-Q3). A1 has to
   state that its marker is the group as seen, and that these cues are outside
   the model, or else add an individual cue as Phelps does.
3. **The marker is inherited.** Goffman places inherited traits used as
   status symbols in caste rather than class society (GF-Q12). A1 cites him for
   class read from signs, not for an inherited marker.

## Candidate contributions after the full reads

1. **Group avoidance from class homophily.** The expected class distance to a
   group is the squared gap between the group's mean and the observer's own
   class, plus the group's within variance.
   - **The variance half is not new.** Aigner and Cain have a risk-averse
     employer who hires from the group with the higher expected productivity
     "discounted for risk" (AC-Q6). A1's σ² term is the risk premium of its own
     quadratic utility (`AignerCain1977.contrast_quadratic_ce`), the same
     mechanism in a contact setting. Rothschild and White suppose a college's
     benefit falls with its students' variance (RW-Q7).
   - **The dependence on the observer's own class is not in any source read.**
     Every source with preferences over partners gives all observers one
     ranking (PF-Q6, CM-Q2, MP-Q5, EG-Q6, CF-Q10, CJP-Q5, BIR-Q3). Pęski and
     Szentes write that mutual bias "is inconsistent with statistical
     discrimination because the signal value of colour must be the same for any
     employer, regardless of his own colour" (PS-Q4). Under class homophily the
     posterior is common to all observers, but the decision it supports depends
     on the observer's own class. That avoidance of each group by the other
     follows is our inference, not yet checked in Lean.
   - **Evidence on the observer's class is thin.** The accent penalty is
     larger among higher-class listeners (AB-Q8). Edelman, Luca and Svirsky
     find little racial homophily among hosts (ELS-Q7, ELS-Q8), which fits a
     common inference about the guest rather than a host-dependent one.
2. **Budget compatibility as the reason class matters.** No source read has a
   model of it. The rival accounts are vertical preference with stable
   matching (MP-Q5, CM-Q6) and cultural homophily "rather than the economic"
   (MSC-Q11). Hitsch, Hortaçsu and Ariely find that own income "matters only
   slightly" in valuing a partner's (HHA-Q5), which runs against homophily in
   income in dating.
3. **Venue exclusion and price screening.** Price screening is not new
   (PF-Q2, CJ-Q5, GF-Q6), nor is a venue pricing on client composition
   (RW-Q4, RW-Q5). Venue exclusion built up from clients' inferences from a
   group marker is in no source read. Two cautions follow. Under A1's utility a
   low class does not want a high pool, so A1 needs its own reason for a price.
   With preferences over composition an equilibrium may not exist (RW-Q9), so
   A1 needs an existence argument. Schwab (1986) and Epple, Romano and Sieg
   (2008) are named in the Fang and Moro survey and are unread [C].
4. **A test of the class channel.** The logic is not new. A belief-based
   penalty vanishes when the type is shown and a taste survives, in Aigner and
   Cain (AC-Q13), Bohren, Imas and Rosenberg (BIR-Q7, BIR-Q9) and Laouénan and
   Rathelot (LR-Q2, LR-Q5). A1 can claim only its application to contact with
   class as the unseen trait. The evidence in contact is mixed.
   - **Against.** On a dating site that shows income and education, the
     ethnic penalty survives (HHA-Q3, HHA-Q4), and a Black man needs $154,000
     more income for equal success (HHA-Q8). Labov's class-9 speaker was
     refused a post for his speech (LV-Q14). The speech premium survives skill
     controls (GRG-Q9).
   - **For.** Hanson and Hawley (2011) find that a rental penalty falls when
     the message signals higher class (ELS-Q6, [C]). It is unread and is the
     nearest precedent. Crabtree et al. find that the perceived gap narrows,
     but does not close, at an Ivy League degree (CR-Q9, CR-Q10).
   - **What the test cannot separate.** Ties formed on seen class alone, with
     race correlated with class, produce racial homophily and also pass the
     test (`McPhersonEtAl2001.consolidation_pos`). Showing class cannot
     separate accurate from biased beliefs about class (Bohren, Imas and
     Rosenberg). Bertrand and Mullainathan's test of class signals asks about
     class beyond race (BM-Q8), so it does not test A1's channel.

## The equilibrium condition

The condition that a group's imputed standing equal its members' actual mean is
in Kim and Loury (KLI-Q10), Austen-Smith and Fryer (AS-Q5), Corneo and Jeanne
(eq. 8), Eguia (eq. 3) and Craig and Fryer (CF-Q4, CF-Q5). A1 states it for an
exogenous class rather than a chosen skill or good, and cites them. Laouénan and
Rathelot find guests' beliefs about minority listings below the actual mean,
and that error carries 2.5 of the 3.4-point statistical penalty (LR-Q6,
LR-Q10). A1 has to say whether its condition is a premise or a long-run state.

## Not new

- Row M14, passing lowers the visible mean, is in Kim and Loury (2012) toward
  the group with the greater return to skill rather than the better reputation
  (KLI-Q11, KLI-D2). Selective exit lowering a group's average skill is in
  Eguia (`Eguia2013.sD_lt_half`).
- The σ² term, as above.
- "Soft evidence" and "soft marker" have no formal content in one update
  (`PengEtAl2010.eq9_soft_eq_virtual`). What remains is a noisy group marker
  under group sufficiency.

## Premises of A1 that sources read do not share

- Eguia gives both backgrounds the same distribution of ability (EG-Q3), so
  his argument against barriers on immutable traits (EG-Q14) does not carry
  over to A1.
- In Charles, Hurst and Roussanov a richer own group lowers a member's status
  (CH-Q6). Moav and Neeman describe that paper as showing race to signal
  income (MN-Q3), which the working paper read does not support (MN-D5).
- Eeckhout writes that "in the standard model of statistical discrimination,
  size does not matter" (EE-Q3). Currarini, Jackson and Pin find inbreeding
  homophily highest for middle-sized groups and assign it to biased meeting
  (CJP-Q2, CJP-Q12). A1 has to say whether group size enters its equilibrium.
- For race, McPherson et al. report a same-different pattern rather than a
  graded one (MSC-Q9, [C] for Marsden 1988). With more than two groups, A1
  predicts avoidance graded by the gap between group means.

## Not read

Loury (2002); Loury (1998), open access but refused to scripts; Epple and
Romano (1998); Rothschild and White (1995); Akerlof (1997); Lang (1986); May and
Goldsmith (2018); Burdett and Coles (1997), which Eeckhout names for adding
heterogeneity (EE-Q14); Hanson and Hawley (2011); Schwab (1986); Epple, Romano
and Sieg (2008); Marsden (1988); the books listed in `source_inventory.md`.
