# Eeckhout (2006), notes

## What was verified

- Twenty quotations are found on the PDF pages they cite in the cached text
  layer, by exact normalised containment (LC-2).
- The model is implemented in Lean in the paper's notation. Of 26 numbered
  results and displayed claims, 17 are formalised in general, 5 in a special
  case and 4 not at all (the coverage table in `CLAIMS.md`).
- Our reading is consistent with the article on these points, apart from
  one entry of Table 2 that disagrees with its own formula (EE-D8) and a
  truncated value (EE-D9).

## What could not be verified

- The genericity step of Proposition 1 and the interior case of
  Proposition 2.
- Tables 1 and 2 beyond the rows recomputed, the age-dependent example of
  §4.1.1, §4.1.2, and the claim that `Π` rises with `π`.
- Nine readings are recorded (EE-D1 to EE-D9). None changes a proposition.

## The bearing on Paper A1

Nothing in the source contradicts the corrected account of A1. The source
has no unseen attribute, so it contrasts with A1's inference channel rather
than preceding it.

### The puzzle

The source explains segregation by colour in marriage (EE-Q15), not contact
across class, so it neither explains friending bias across class lines nor
conflicts with it. It does show that group shares change the pay-off from
contact across groups. With unequal shares, symmetric type-dependent
strategies treat the two colours differently (`inclusive_majority_gains`)
and cannot be constrained efficient (`prop3_step`), and under exclusive
segregation the minority loses once the majority's share among singles
exceeds `1/(1 + δ)` (EE-Q12, `minority_worse`). The author contrasts this
with statistical discrimination, where he says size does not matter
(EE-Q3).

### Budget compatibility

The source assumes partners identical apart from a colour that enters no
pay-off (EE-Q5, EE-Q7). Assortative matching by colour comes from
coordination, colour serving as a public randomisation device (EE-Q1,
EE-Q9), and any other public device would serve as well (EE-Q6). The author
names selection on heterogeneity as a rival origin of assortative matching,
citing an estimate that 64 per cent of matches would be mixed after
selection (EE-Q2, EE-Q16), and leaves heterogeneity to future work
(EE-Q14). The only cost of contact in the model is the stage game's loss
`l` from cooperating with a defector (`stage_dominant`). Nothing bears on
budgets, affordability or shared consumption.

### Avoidance that depends on the observer's class

Treatment depends on whether the partner shares one's colour (EE-Q10).
Direction (i) is supported as one equilibrium. Exclusive segregation is
symmetric, so both colours stop cooperating in mixed matches (EE-Q11).
Direction (ii) has two equilibrium analogues. One segregation strategy has
players treat their own colour worse than the other (EE-Q17), and minority
bias has the majority cooperate with outsiders at a cost to itself (EE-Q18,
`minority_bias_vb_gt_vw`). Which direction obtains is equilibrium selection
in the source, because agents are homogeneous (EE-Q5) and nothing about the
observer other than colour can enter. The source therefore says nothing
about a low-class member of the high group.

### The penalty vanishing when class is shown

The source has no such statement. Colour carries no information (EE-Q7) and
could be replaced by a coin toss (EE-Q6), so what is shown about a partner
beyond colour cannot change the penalty. For the link A1 draws to the
statistical-discrimination literature, the source is the contrast case of a
group penalty that does not rest on inference.

### The shadow market

The source has no law, no ban and no venues. Three passages bear on the
shadow market. Choosing where to meet is named as future research, in the
form of whites searching only among whites (EE-Q13). A device available in
the population is argued to be more plausible than a central one (EE-Q20).
Policy is cast as moving society between equilibria rather than
constraining a choice (EE-Q19). Our inference, not the source's, is that a
ban on conditioning on colour would leave coordination on any other public
signal in place, since any such signal can play colour's role (EE-Q6).

### Premises of A1 the source does not share

- An unseen attribute inferred from a marker. The source's agents are
  homogeneous and colour is payoff irrelevant (EE-Q5, EE-Q7).
- A preference over the partner. Pay-offs come from a prisoner's dilemma
  that is the same for every pair (`stage_dominant`).
- A static equilibrium. The source studies stationary perfect public
  equilibria of a repeated matching game and selects among them by
  efficiency.
- A consistency condition on group standing. The source's consistency
  condition fixes the white share among singles (`singles_quadratic`),
  which concerns who is in the pool rather than what an observer believes
  about them.

## Controls

- LC-2. A fabricated quotation, "colour is correlated with the unobserved
  quality of a partner", the opposite of EE-Q7, is not found on any page. A
  true quotation is not found on a neighbouring page.
- Lean. `control_prop3_equal_shares` shows Proposition 3 needs unequal
  shares. `control_equal_shares_no_loss` shows the minority's loss under
  exclusive segregation needs unequal shares. `control_no_same_colour_no_gain`
  shows Proposition 2's gain needs same-colour matches.
  `control_lemmaA1_needs_v_lt_one` shows Lemma A1 needs `v < 1`.
  `control_prop1_boundary` shows Proposition 1's strict inequality needs
  `g < δ`. `control_prop4_threshold` shows Proposition 4 needs
  `π > 1/(2 + g − l)`.
- The axiom audit of LC-4 was run with the module compiled to a scratch
  olean, as the second-pass brief prescribes. Every cited theorem depends
  only on `propext`, `Classical.choice` and `Quot.sound`.

## Page images read

PDF pages 4, 10, 13, 14, 15, 20 and 22 were rendered at 110 dpi with
`pdftoppm` from the pinned PDF. They are reproducible from it and not pinned
separately.
