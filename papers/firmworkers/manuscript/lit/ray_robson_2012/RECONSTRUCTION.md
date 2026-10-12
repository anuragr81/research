# Reconstruction of Ray and Robson, 2012

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| RR | "Status, Intertemporal Choice, and Risk-Taking", *Econometrica* 80(4), July 2012, pp.1505–1531 | Econometric Society PDF with text layer, 28 pages (a cover sheet, then pp.1505–1531), Drive `RayRobsonStatus.pdf`, id `1OMiiO4pio_a56fH1ECa_6ifhcIrGj7eD`, uploaded 12 Oct 2026; cached `~/.cache/firmworkers/ray_robson_2012.pdf`, sha256 `755339cfe6f4e1401b704fa8c027c8aaa802716a53afd319b02bbf33aaf5a71e` | [F], published; the Supplemental Material with the full proofs is not read |

Text extracted with `pdftotext -layout`; form-feed block `i ≥ 1` is printed
page `1504 + i`, and every block's running head carries its page number. Read
in full on 12 Oct 2026, main text and Appendix.

**Why this paper.** The third precedent for the reference-dependent
sub-section (decision of 12 Oct 2026): status as consumption rank produces the
Friedman–Savage pattern with no assumption on curvature in status, in a growth
model, and it names the use of income distributions to create endogenous
reference points as a related strand (RR-7). Needed for the novelty check.

## Primitives (§2, pp.1508–1510)

- A continuum of dynasties; wealth `w`, any fair gamble with mean `w`, then a
  split into consumption `c` and bequest `k`; `w′ = f(k)`.
- Status (2): `s = ηF⁻(c) + (1 − η)F(c)`, the share consuming strictly less
  plus a share `1 − η` of those consuming the same; `η = ½` makes total status
  ½ (footnote 10).
- Utility `u(c, s)`, strictly increasing and concave in `c`, increasing in `s`,
  no curvature restriction in `s` (Assumption 2); dynastic discounting `δ`.
- All fair gambles available, supplied at zero profit.

## Derivation chain

1. **Within-period equilibrium (§3.1).** Realised consumption `F` is a
   mean-preserving spread of budgets `H` (R1); reduced-form utility
   `μ(c) = u(c, F̄(c))` is concave, else gambling pays (R2); `μ` is affine over
   any range of randomisation (R3). Proposition 1: a unique such `F`.
2. **Corollary 1 (p.1512).** A common budget `b` spreads into `F` on `[a, d]`
   with `u(c, F(c)) = u(a, 0) + α(c − a)`, `α = u_c(a, 0)`, `a > 0`.
3. **Steady state (Proposition 2).** `δf′(k∗) = 1`, common wealth
   `w∗ = f(k∗)`, budget `b∗ = f(k∗) − k∗`, consumption spread as in Corollary 1.
4. **Convergence and existence (Proposition 3).** A turnpike argument bunches
   capital stocks; concavity of `μ` then forces gambling into a common affine
   segment; paths coincide and converge to `k∗`.
5. **Convex production (§4, Proposition 4).** With pure status utility and
   convex `f`, the policy `c = (1 − δ)w` is an equilibrium with no risk-taking,
   and the only strict smooth deterministic one; its first-order condition
   gives `c′ = 1 − δ`.
6. **Friedman–Savage property (§5.1).** Insurance against outcomes below `a∗`
   or above `d∗`, gambling in between.
7. **Scale-neutrality (§5.2)** and **Pareto-inefficiency (§5.3,
   Proposition 5).** With `u` jointly strictly concave, banning gambling at the
   steady state gives everyone `u(b∗, ½)`, more than the Jensen-bounded expected
   utility under gambling. In general, efficiency would need `u_c(c, F∗(c))`
   constant, but at `a` `F∗′(a) = 0`, so its slope is `u_cc < 0`.
8. **Mean below median (§5.4).** With `u` jointly strictly concave,
   `u(b∗, F∗(b∗)) < u(b∗, ½)`, so `F∗(b∗) < ½`.
9. **Extensions (§6)** and **conclusion (§7):** occupations, entrepreneurship
   and promotion in a gang as real gambles; footnote 25 poses entry into an
   activity with aggregate risk, where stayers' status depends on how many
   enter.

## What was reconstructed

The status rule's total for any `η` (finite version) and its value ½ exactly
when `η = ½`; the Jensen step of item 7 for finite lotteries over
consumption–status pairs; the logic of item 8 from its two premises; the
first-order algebra of Proposition 5's proof; the first-order condition of
Proposition 4(ii); the mean of the uniform example in footnote 18. See
`CLAIMS.md`.

## What was not reconstructed

Proposition 1 (construction of the reduced-form utility), Propositions 2 and 3
(steady state, turnpike convergence, existence), Proposition 4(i) and Lemma 20,
Remark 1, and everything in the Supplemental Material.
