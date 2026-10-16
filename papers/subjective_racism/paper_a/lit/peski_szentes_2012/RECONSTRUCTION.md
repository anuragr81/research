# Pęski and Szentes (2012), reconstruction

Marcin Pęski and Balázs Szentes, "Spontaneous Discrimination", Becker
Friedman Institute Working Paper No. 2012-005, University of Chicago, 7 May
2012. The paper was later published in the *American Economic Review* 103(6),
2013. The published version is not read, and this record cites the working
paper as such (PS-D1).

## Source read

- The BFI working paper, 34 PDF pages. PDF page 1 is the BFI cover, PDF
  pages 2 to 34 carry the paper's own page numbers 1 to 33, so printed page
  `n` is PDF page `n + 1`. Quotations cite PDF pages.
- Downloaded on 15 October 2026 from
  `https://bfi.uchicago.edu/wp-content/uploads/BFI_2012-005.pdf` (recorded in
  `~/.cache/paper_a/sources/urls4.txt`).
- Cached at `~/.cache/paper_a/sources/peski_szentes_bfi.pdf`, 456,309 bytes,
  sha256 `2e311476…d876`. The PDF has a text layer (pdfTeX). The text was
  extracted to `~/.cache/paper_a/sources/peski_szentes_bfi.txt`, sha256
  `de72ee18…9ceb`, one form feed per page.
- Every page was read in full from the text on 16 October 2026. The text
  layer drops Greek letters and garbles displayed equations, so PDF pages 6
  to 13 and 25 to 28 were also rendered at 110 dpi and read as images, and
  the equations below are taken from those images. The evidence tag is [F].

## The model, from its own primitives

1. **Population and time** (PDF p. 6). A unit mass of infinitely lived,
   risk-neutral agents in continuous time, discount rate `r`. Production
   opportunities arrive at Poisson rate `δ`. Agents with an opportunity are
   paired, and each is employer or worker with probability 1/2.
2. **Payoffs** (PDF p. 6). A match shock `s` has CDF `G(s) = 1 − e^{−λs}`
   and is seen by both. If the employer hires, he gets `s` and the worker
   gets a fixed wage `M > 0`. Otherwise both get zero.
3. **Types** (PDF pp. 6 to 7). A type is a pair (physical colour, social
   colour), each black or white. Physical colour is fixed, with white share
   `μ_w` and black share `μ_b = 1 − μ_w`. A worker's social colour does not
   change. An employer who hires a worker of type `(c_1, c_2)` keeps his
   social colour with probability `1 − γ`, takes `c_1` with probability
   `γα` and takes `c_2` with probability `γ(1 − α)`. An employer who does
   not hire keeps it with probability `1 − γ` and takes his own physical
   colour with probability `γ`.
4. **Information** (PDF p. 7). Before deciding, the employer observes the
   worker's full type and nothing else (PS-Q5). Social colour carries
   information about the worker's past hiring (PS-Q6).
5. **What is payoff relevant** (PDF pp. 4, 7). Neither colour enters any
   payoff. Physical colour says nothing about a partner's profitability
   (PS-Q1), and agents have no taste over colours (PS-Q3). The only thing
   that makes a type matter is that hiring moves the employer's own social
   colour, which moves how others will treat him as a worker.
6. **Equilibrium** (PDF pp. 7, 13). Steady-state equilibria, with
   strategies that depend on neither time nor history. A cutoff profile
   `(x^c, x^{−c})` is an equilibrium when each colour's cutoff is a best
   response to the profile, eq. (10).

## Derivation chain

- **Lemma 1** (PDF pp. 8 to 9). Comparing (1) with (2), hiring a `(b, w)`
  worker pays when `s ≥ γα (V_{w,w} − V_{w,b})`. The employer's own social
  colour enters (1) and (2) only through `(1 − γ) V_{w,c}`, which cancels
  (PS-Q8). The four cutoffs of a colour-`c` employer are proportional to
  the bias `V_{c,c} − V_{c,−c}`, the value of a social colour equal to one's
  physical colour (PS-Q9). With `x^c := x^c_{−c,−c}`, eq. (3) gives
  `x^c_{c,−c} = (1 − α) x^c`, `x^c_{−c,c} = α x^c` and `x^c_{c,c} = 0`.
  Formalised as `hire_iff`, `cutoff_indep_own_social_colour`,
  `lemma1_hire`, `lemma1_cutoffs` and `eq3`.
- **Lemma 2** (PDF pp. 9 to 11). The worker's values (6) and (7) and the
  value equation (5) give the bias as a function of the cutoff
  distributions, and with it the best response
  `b̃^c(x^c, x^{−c}) = K [μ_c G((1 − α)x^c) + μ_{−c}(G(αx^{−c}) − G(x^{−c}))]`,
  eq. (9), with `K = Mδγ/2r`, truncated at zero in eq. (4). The best
  response does not depend on the distribution of social colours (PS-Q10).
  Formalised as `lemma2` from the printed bias line, and as
  `lemma2_our_reading` on our reading (PS-D3).
- **Lemma 3** (PDF p. 11, proof PDF p. 25). (i) `b̃^c` is concave and strictly
  increasing in `x^c`. (ii) `b^c(0, x^{−c}) = 0`, because `G(αy) ≤ G(y)`.
  (iii) `b^c(x^c, 0)` is the limit of `b^c(x^c, x^{−c})` as `x^{−c} → ∞` and
  bounds it from above. Formalised as `lemma3_i_strictMono`,
  `lemma3_i_concave`, `lemma3_ii`, `lemma3_iii` and `lemma3_iii_limit`.
- **Lemma 4** (PDF p. 11, proof PDF pp. 25 to 26). With
  `λ_0 = 1/(K(1 − α)μ_c)`, the curve `b^c(·, 0)` has a unique positive fixed
  point if `λ > λ_0` and lies below the 45-degree line if `λ ≤ λ_0`.
  Formalised as `lemma4_i`, by strict concavity along chords from the
  origin (`fp_chord`), and `lemma4_ii`, using `1 − e^{−t} < t`.
- **Proposition 1** (PDF p. 13). `(0, 0)` is always an equilibrium. It is
  the only one if `λ ≤ λ_0` for both colours, and if `λ > λ_0` for colour
  `c` there is an equilibrium `(x^c, 0)` with `x^c > 0` (Remark 1).
  Formalised as `prop1_zero`, `prop1_i`, `prop1_ii` and `remark1`, with
  `prop1_ii` proved by the intermediate
  value theorem on `K μ_c(1 − e^{−λ(1−α)x}) − x` and the bound
  `1 − e^{−t} ≥ t/(1 + t)`.
- **Lemma 5** (PDF p. 15, proof PDF pp. 26 to 27). The roots of
  `B^{x^{−c}}(x^c) = b̃^c(x^c, x^{−c}) − x^c` move with a vertical shift
  `H(x^{−c}) = Kμ_{−c}(G(αx^{−c}) − G(x^{−c}))`, negative, zero at both ends
  and minimal at `x̂^{−c} = −log α/(λ(1 − α))`. Formalised in part as
  `lemma5_shift`, `lemma5_H_neg`, `lemma5_H_limits` and `lemma5_B_zero`.
- **Proposition 2 and Lemmas 6 to 9** (PDF p. 16, proofs PDF pp. 27 to 30).
  For large `λ` every equilibrium cutoff is zero, small, or near
  `x^c_max = Kμ_c`. Lemma 6 is `1 − e^{−ξ} ≥ ξ/2` on `[0, log 2]`. Lemma 7
  uses eq. (15) to split equilibria into small and large. Lemma 6, eq. (15)
  and Lemma 7 are formalised as `lemma6`, `eq15` and `lemma7`, the last
  with corrected constants (PS-D8).
- **Stability, Theorems 1 to 3** (PDF pp. 17 to 22, proofs PDF pp. 30 to
  33). Under the myopic dynamics (12) the colour-blind profile is unstable
  above the threshold, and there are exactly three stable equilibria, two
  one-sided and one mutual (PS-Q11, PS-Q18). The sign of the drift on the
  axis is formalised as `drift_neg_below` and `drift_pos_near_zero`.
- **Comparative statics** (PDF p. 21). Cutoffs rise with `M`, `λ`, `δ`, `γ`
  and fall with `r`, and `μ_w` raises `x^w` and lowers `x^b`. The paper
  omits the proof. Formalised for the one-sided equilibria as
  `compstat_one_sided`.

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every numbered result. The gaps are
these.

- **Lemma 5** in full. The two root branches `x^c_1`, `x^c_2`, their shapes,
  and properties (3), (5), (8) and (9) are not formalised.
- **Proposition 2, Lemmas 8 and 9.** The asymptotic classification of
  equilibria for large `λ` is read and not formalised. Lemma 7, on which
  it rests, is formalised.
- **Definition 1 and Theorems 1 to 3.** Stability rests on the phase diagram
  and the Hartman–Grobman theorem, which Mathlib lacks. Only the sign of
  the drift along the axis `x^{−c} = 0` is formalised. Theorem 3, for a
  general concave `G`, is not set up.
- **Comparative statics for the mutual equilibrium.** Only the one-sided
  equilibria are covered.
- **The constant `K`.** Our reading of the bias equation below eq. (7)
  differs from the printed line (PS-D3). Being an employer changes
  `V_{c,c} − V_{c,−c}` only through the event, of probability `1 − γ`, that
  the social colour stays, so the bias is `δ M A/(2r + δγ)`
  (`bias_with_employer_branch`) against the printed `M δ A/2r`
  (`bias_as_printed`). Only the positive scale of `K` changes.
- **Constants in the appendix.** Lemma 6 holds only for `ξ ≥ 0` (PS-D7), and
  Lemma 7's printed constants are off by factors 2 and 4 (PS-D8). The
  formalised versions use corrected constants.
- **The published version.** The AER 2013 article may differ in model,
  notation or results, and is not read.
