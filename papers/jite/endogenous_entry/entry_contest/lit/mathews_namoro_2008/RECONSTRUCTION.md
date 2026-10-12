# Reconstruction of Mathews and Namoro, "Participation incentives in rank order tournaments with endogenous entry"

**Source read.** Timothy Mathews and Soiliou Namoro, working paper of
29 March 2005, 31 pages, uploaded by the author on Drive; read in full [F].
The published version is *Journal of Economics* 95(1) (2008), 1-23, not
checked. The text was extracted with `pdftotext` and cached at
`~/.cache/entry_contest/mathews_namoro_2008.txt`; pages are the working
paper's printed pages (the printed page is the PDF page for pages 1-29).

**Why this paper.** It is cited in our other documents as an entry paper and
was not read, so C5, C6 and C13 carried it as unverified.

## Model (Sections 2 to 4)

Two events with two places each. Strategic agents H (high ability) and L (low
ability) each enter exactly one event; non-strategic agents N fill the
remaining places. A winner takes `p`, a runner-up `alpha p`, `0 <= alpha < 1`;
Event 1 pays `p1 = tau p2` with `tau >= 1`. The entry choice is sequential,
H first. In a tournament the success probability is the ratio form
`delta e_A^z / ((1 - delta) e_B^z + delta e_A^z)` with `z` in `[0,1]` and
cost `c(e) = e`, so the equilibrium effort is `z p (1 - alpha) delta (1 - delta)`
for both. Base probabilities: `lambda` = P(H beats L), `omega` = P(H beats N),
`rho` = P(L beats N), with `omega > lambda > 1/2` and `omega > rho > 1/2`
(the intuitive restrictions).

## Results

1. **Allocations (pp.9-12).** Backward induction gives four allocations: a
   both strategic agents in Event 1 (pooling); b H in Event 1, L in Event 2;
   c H in Event 2, L in Event 1; d both in Event 2. With cutoffs
   `Psi = top(rho)/bot(lambda)` and `Theta = top(omega)/top(lambda)`: a iff
   `tau >= max(Psi, Theta)`; b iff `tau < Psi`; c iff `Psi <= tau < Theta`;
   d never (Observation 1).
2. **The counterintuitive allocation c (Sections 4 and 5).** At c the stronger
   agent joins the event with the smaller prizes. If c arises at some `tau`,
   b arises at a smaller one, so an event can attract a stronger field by
   cutting its prize. Example (p.14): `omega = 1`, `lambda = rho = 251/400`,
   `alpha = 1/2`, `z = 1`; prizes 30,000 and 25,000 give b, 30,000 and 24,000
   give c.
3. **When c cannot arise (Section 5.1).** `eta = (Psi - Theta) top(lambda)
   bot(lambda)` is a quadratic in `alpha`. c never arises iff `eta'(1) <= 0`,
   which for every `z` is Condition 2, `lambda >= (omega^2 - rho^2 + 1)/2`,
   and for some `z` Condition 3, `lambda >= (omega - rho + 1)/2`. Section 5.2
   has three cases for the sign of `eta`, each possible (pp.16-17).
4. **Section 6.** First-order stochastic dominance does not rule c out (a
   worked example); power functions `F = x^H, x^L, x^N` satisfy Condition 2,
   so c never arises.
5. **Appendix B.** An organiser minimising total prizes for a separating
   allocation can find c cheaper than b (3.8228 against 3.8566 in the
   example).

## What could not be reconstructed

Nothing material for our use. The paper has no entry cost, no count of
entrants and no wealth: footnote 5 (p.5) mentions a reservation wage and
nothing else about the cost of taking part.
