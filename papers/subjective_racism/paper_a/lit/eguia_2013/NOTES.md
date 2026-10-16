# Eguia (2013), notes

## What was verified

- Fifteen quotations are found on the pages they cite in the publisher's text
  layer, by exact normalised containment (LC-2).
- Eq. (3) in both of its printed forms, the effect of assimilation on the two
  group means and on the mean endowment, the derivative on p. 32 and the
  screening argument of p. 27 are Lean theorems in the paper's notation
  (LC-4). Our reading is consistent with the source on these points, except
  for the value at `s = 1` recorded as EG-D1.

## What could not be verified

- Propositions 1 to 3 in general. Their proofs rest on continuity in `λ` and
  on `c′/c` being very negative, with no explicit bounds, and they are not
  formalised.
- The formal versions of the three generalizations, which are in an unread
  add-on file (EG-D6).
- The published 2017 article (EG-D5).

## The bearing on Paper A1

Nothing in the source contradicts the A1 model. One premise differs in a way
that matters. Ability has the same distribution in both backgrounds (EG-Q3),
so the background is no evidence about ability, and the paper's case against
barriers on immutable traits (EG-Q14) rests on that. In A1 the group class
distributions differ, so the marker is informative and that case does not
carry over.

The three questions put to this source have these answers.
- **An unseen type inferred from a group marker.** No. Ability is private
  (EG-Q2), and nobody forms a belief about another person from the person's
  group or from anything else (EG-Q10). The group enters payoffs through its
  realised mean endowment and mean skill (eq. 1, EG-Q5, EG-Q6).
- **A cost of assimilating.** Yes. Joining the advantaged group costs
  `d c(s_i)`, falling in skill (EG-Q4), with `d` set by incumbents to screen
  (EG-Q1, EG-Q9).
- **An equilibrium condition on group composition.** Yes. The group means are
  those of the members that the cutoff admits (eq. 3, `eq3_sA`, `eq3_sD`),
  and the cutoff agent is indifferent at the composition the cutoff produces
  (EG-Q7). This is a fixed point on realised group means, the same kind of
  condition as A1's requirement that the imputed group standing equal the
  actual mean of the visible members. In Eguia the mean is paid out directly
  as an externality rather than imputed to individuals.

The candidate contributions of A1 are taken in turn.
1. **Group avoidance from class homophily under inference.** Not in the
   source. Preferences over group composition are vertical. Every member gains
   from higher mean skill and endowment (EG-Q6), which is the reverse of
   homophily. The incumbent's preferred barrier does depend on the incumbent's
   own skill (EG-Q8), but through an assumed complementarity `v_{s_i s_J} ≥ 0`
   and over a group's mean, not over a dyadic partner. The externality
   depends on group means only (EG-Q11), so nothing penalises within-group
   spread, and there is no counterpart of A1's σ² term. Homophily on the
   marker appears only as a taste, in words (EG-Q15).
2. **Budget compatibility as the source of class homophily.** Not in the
   source. The endowment enters as an externality to the group (EG-Q6).
3. **Venue exclusion and price screening as certification of class.**
   Partly. Incumbents choose an entry cost that separates the able from the
   rest without observing anyone's ability (EG-Q1, EG-Q9), and those who pay
   it are certified as above the cutoff. The instrument is a learning cost
   that falls with the type (EG-Q4), not a price, and it is chosen by a
   pivotal incumbent under a strategy-proof rule, not aggregated from client
   inference. The source's control is useful to A1. A cost that does not vary
   with the type cannot screen (`control_lump_sum_no_screen`, EG-Q14), so a
   price screens on class in A1 only because its burden varies with class.
4. **A test of the class channel.** Not in the source. There is no inference
   about persons, hence no group penalty that showing the type could remove.

On the point known not to be new. Selective exit from the disadvantaged group
lowers that group's mean skill (`sD_lt_half`) and raises the mean skill of the
group joined (`sA_gt_half`), while diluting its mean endowment (`eAbar_lt`).
The passing effect is recorded as known (Kim and Loury 2012), and Eguia (2013) states
the same composition effect in a static model of assimilation. On the
empirical hook, the source reads the acting-white evidence as stronger in
integrated schools (EG-Q13). That is a different outcome from friending bias,
but it is a second reason, besides A1's, why cross-group ties are scarcer in
mixed schools.

## Controls

- LC-2. A fabricated quotation, "Individual ability is publicly observed by
  every agent.", the opposite of EG-Q2, is not found. A true quotation is not
  found on a neighbouring page.
- Lean. `control_no_mobility` shows `sD_lt_half` needs `λ > 0`,
  `control_cutoff_zero` that `sA_gt_half` needs `s > 0`, and
  `control_lump_sum_no_screen` that screening needs a cost that varies with
  the type.
