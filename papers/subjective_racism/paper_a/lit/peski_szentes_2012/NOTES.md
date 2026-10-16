# Pęski and Szentes (2012), notes

## What was verified

- Twenty-one quotations are found on the PDF pages they cite in the cached
  text layer, by exact normalised containment (LC-2).
- The model is implemented in Lean in the paper's notation. Of 26 numbered
  results, 15 are formalised in general, 5 in a special case and 6 not at
  all (the coverage table in `CLAIMS.md`). The general ones include Lemmas 1
  to 4, Proposition 1, Remark 1, Lemma 6, eq. (15) and Lemma 7.
- Our reading is consistent with the working paper on these points, except
  for the readings PS-D3, PS-D7, PS-D8 and PS-D10, where the formalisation
  shows a printed constant, domain or condition needs correcting.

## What could not be verified

- Proposition 2, Lemmas 8 and 9, Definition 1 and Theorems 1 to 3 are read
  and not formalised. Theorems 1 and 2 are covered only by the sign of the
  drift on the axis (`drift_neg_below`, `drift_pos_near_zero`).
- Lemma 5 is covered in part (`lemma5_shift`, `lemma5_H_neg`,
  `lemma5_H_limits`, `lemma5_B_zero`).
- The constant `K`. Our derivation gives `K = Mδγ/(2r + δγ)` and the paper
  prints `Mδγ/2r` (PS-D3). Both are positive, and every formalised result
  holds for any positive `K`.
- The AER 2013 version is not read (PS-D1).

## A premise to correct first

The first brief described this paper as having agents who care about the
unobserved types of partners. The working paper has no unobserved type. The
employer observes the worker's whole type (PS-Q5), neither colour enters any
payoff (PS-Q1), and agents have no preference over colours (PS-Q3). Social
colour is a signal about the partner's past partners (PS-Q6). Nothing in the
source contradicts the corrected account of A1.

## The bearing on Paper A1

### The puzzle

The source does not address contact across class, so it neither explains
friending bias across class lines nor conflicts with it. Its nearest content
is about group shares. A larger white share makes discrimination against
blacks cheaper and discrimination against whites dearer (PS-Q21), and in a
one-sided equilibrium the discriminating colour's cutoff rises with its own
share (`compstat_one_sided`). Discrimination also rises with how visible
contacts are (PS-Q16). Both bear on how diversity moves contact across
colours, not on contact across class within or between colours.

### Budget compatibility

The source assumes no preference over partners (PS-Q3) and partners who are
identical in everything a payoff depends on (PS-Q1). Same-colour contact
arises from the value of one's own label, since hiring across colours moves
one's social colour and so one's future employment (PS-Q9, PS-Q15,
`lemma1_hire`). The only cost of contact across colours is that lost future
value. The wage is a constant, so nothing in the source bears on budgets,
affordability or shared consumption (`lemma2`, where `M` enters as a
constant).

### Avoidance that depends on the observer's class

The source has avoidance that depends on the observer's own physical colour
(PS-Q8). Direction (i) is supported as one equilibrium among several. One
stable equilibrium has each colour discriminating against the other
(PS-Q18), and two have only one colour discriminating (PS-Q11, `remark1`),
so the source does not force avoidance to run both ways. Direction (ii) has
an analogue in a different channel. An agent can gain from association with
the other colour (PS-Q19), specifically when his own colour does not
discriminate and the other does (PS-Q20, `lemma3_ii`). That gain depends on
what the other colour does rather than on the agent's position within his
own colour, since agents of one colour are identical. The source also states
that statistical discrimination cannot depend on the observer's colour
(PS-Q4, PS-Q14). Under A1's engagement utility the posterior is common to
all observers while `(c̄_g − c_i)² + σ_g²` depends on `c_i`, so A1 qualifies
that statement.

### The penalty vanishing when class is shown

The source has no such statement, because nothing payoff relevant is hidden
(PS-Q1). Its nearest statements are that discrimination vanishes when
partnerships are unobserved (PS-Q12) and that without social colour hiring
is unconditional (PS-Q7, `control_no_social_colour`). In the source the
penalty disappears when the association signal is removed, and showing a
partner's class would leave it in place. The authors place their mechanism
outside statistical discrimination (PS-Q2), so for the link A1 draws to that
literature the source is the contrast case of a group penalty that does not
rest on inference.

### The shadow market

The source has no law, no ban and no venues or platforms. It does have
three of the elements A1's shadow market needs. Information about whom an
agent has met acquires value in equilibrium (PS-Q15). Employers condition on
a label correlated with group, so that a white worker with a black social
colour faces a higher cutoff than one with a white social colour (PS-Q13).
Discrimination rises with how visible contacts are (PS-Q16). The authors'
policy acts on the distribution of labels rather than on a prohibition
(PS-Q17). Our inference, not the source's, is that a ban on conditioning on
physical colour would leave discrimination through the correlated label in
place.

### Premises of A1 the source does not share

- An unseen attribute inferred from a marker. The source's types are fully
  observed and payoff irrelevant (PS-Q1, PS-Q5).
- A preference over the partner. The source's agents care only about money
  (PS-Q3).
- Differences within a group. Agents of one colour are identical in the
  source, so it has no analogue of class within a group.
- A static equilibrium with an imputed group statistic. The source's
  equilibrium is a steady state in cutoffs, selected by stability, and the
  best response does not depend on the distribution of labels (PS-Q10).

## Controls

- LC-2. A fabricated quotation, "the employer infers the productivity of the
  worker from his physical colour", the opposite of PS-Q1, is not found on
  any page. A true quotation is not found on a neighbouring page.
- Lean. `control_no_social_colour` shows that without movement of social
  colour (`γ = 0`) nobody discriminates, the paper's own statement PS-Q7.
  `control_below_threshold_no_discrimination` shows Proposition 1 (ii)
  fails at or below `λ_0`. `control_lemma3_iii_needs_alpha` shows the
  equality in Lemma 3 (iii) fails at `α = 0`. `control_lemma6_needs_nonneg`
  shows Lemma 6 fails on the printed domain at `ξ = −1`. `lemma4_ii` is the
  control for Lemma 4 (i) and Remark 1, since below the threshold no
  positive fixed point exists.
- The axiom audit of LC-4 was run with the module compiled to a scratch
  olean, as the second-pass brief prescribes. Every cited theorem depends
  only on `propext`, `Classical.choice` and `Quot.sound`.

## Page images read

PDF pages 6 to 13 and 25 to 28 were rendered at 110 dpi with `pdftoppm` from the pinned
PDF, and page 1 at 60 dpi to read the cover. They are reproducible from the
PDF and not pinned separately.
