# Pęski and Szentes (2012), notes

## What was verified

- Fourteen quotations are found on the PDF pages they cite in the cached
  text layer, by exact normalised containment (LC-2).
- Lemma 1 for one worker type, the cancellation of the employer's own social
  colour, Lemma 3 (ii) and (iii), Lemma 4 (ii) and Proposition 1 (i) and (ii)
  are Lean theorems in the paper's notation (LC-4). Proposition 1 (ii) is
  proved for every parameter set above the threshold, with no special case.
- Our reading is consistent with the working paper on these points.

## What could not be verified

- Proposition 2 and Theorems 1 to 3 (stability under myopic best-response
  dynamics) are read and not formalised.
- The constant `K`. Our derivation of the bias below eq. (7) gives
  `K = Mδγ/(2r + δγ)` and the paper prints `Mδγ/2r` (PS-D3). Both are
  positive, and every result we use holds for any positive `K`.
- The AER 2013 version is not read (PS-D1).

## A premise to correct first

The brief describes this paper as having agents who care about the
unobserved types of partners. The working paper has no unobserved type. The
employer observes the worker's whole type (PS-Q5), neither colour enters any
payoff (PS-Q1), and agents have no preference over colours (PS-Q3). Social
colour is a signal about the partner's past partners (PS-Q6), and it matters
only because hiring moves the employer's own social colour and so his own
future employment (PS-Q9). Nothing in the source contradicts the A1 model,
but the source is a contrast to A1 rather than a predecessor of its
inference channel.

## The bearing on Paper A1

1. **Contribution 1 (avoidance derived from class homophily under
   inference).** Not in the source. The source does have avoidance that
   depends on the observer's own type, since cutoffs depend on the
   employer's physical colour (PS-Q8) and one stable equilibrium has each
   colour discriminating against the other (PDF p. 19). That dependence
   comes from the value of one's own social colour (PS-Q9), with no inference
   and no preference over the partner. The source also states that
   statistical discrimination cannot produce own-colour dependence, because
   "the signal value of colour must be the same for any employer" (PS-Q4,
   PS-Q14). A1's contribution 1 qualifies that statement. Under group
   sufficiency the posterior on class given the marker is common to every
   observer, as PS-Q4 says, but with `U = v − κ(c_i − c_j)²` the expected
   loss `(c̄_g − c_i)² + σ_g²` depends on the observer's own class. A1
   should cite PS-Q4 as the statement it qualifies and the source as the
   alternative channel for own-type-dependent avoidance. The falsifier for
   any claim that own-type dependence identifies A1's channel is this
   source, where the dependence arises with no inference at all. The source
   has no variance term. Its payoffs do not depend on any attribute of the
   partner, so a group's spread cannot matter.
2. **Contribution 2 (budget compatibility).** Not in the source. The wage is
   a constant `M`, and the authors leave endogenous wages to future work
   (PDF p. 23).
3. **Contribution 3 (venues and price screening).** Not in the source.
   Matching is random and there are no venues, clubs or prices.
4. **Contribution 4 (the group penalty vanishes when class is shown).** Not
   in the source. The source's own tests are different. Discrimination
   vanishes when partnerships are unobserved (PS-Q12), and a white employer's
   profit from a white worker rises with the black partners in the worker's
   history (PS-Q13). Under the source's channel, showing a partner's class
   would leave the group penalty intact, because the penalty does not rest on
   inference about any attribute. A1's test therefore separates the two
   channels. The penalty vanishes under A1's channel and persists under the
   source's.
5. **Equilibrium condition on groups.** None that bears on A1. The
   equilibrium is a fixed point in cutoffs, and the best response does not
   depend on the distribution of social colours (PS-Q10). Nothing requires
   an imputed group statistic to equal an actual one.

## Controls

- LC-2. A fabricated quotation, "the employer infers the productivity of the
  worker from his physical colour", the opposite of PS-Q1, is not found on
  any page. A true quotation is not found on a neighbouring page.
- Lean. `control_no_social_colour` shows that without movement of social
  colour (`γ = 0`) nobody discriminates, the paper's own statement PS-Q7.
  `control_below_threshold_no_discrimination` shows Proposition 1 (ii)
  fails at or below `λ_0`.
- The axiom audit of LC-4 imports the module, so it needs the module to be a
  build root, which the coordinator adds. Until then the audit was run on 16
  October 2026 on a copy of the file with `#print axioms` for each of the 11
  cited theorems. Each depends only on `propext`, `Classical.choice` and
  `Quot.sound`.

## Page images read

PDF pages 6 to 13 were rendered at 110 dpi with `pdftoppm` from the pinned
PDF, and page 1 at 60 dpi to read the cover. They are reproducible from the
PDF and not pinned separately.
