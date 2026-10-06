# Claims about Suen (2007)

**Paper.** Wing Suen, "The comparative statics of differential rents in
two-sided matching markets", *J Econ Inequal* 5 (2007), pp. 149–158.
Evidence level **[F]**, published version.

## Claims as stated in `LITERATURE.tex` §sec:assignment and §Verdict

| ID | Claim | Source locator | Relevance claimed to `PROOFS.tex` |
|---|---|---|---|
| SU-A | One-to-one transferable-utility matching with `Q = θ(x)φ(y)`, positive assortative matching, matching function `μ = G⁻¹∘F`, "which they note can itself be read as a distribution function". | §2, eq. (1), p.151 | Same map class as our `T = Λ_β⁻¹∘Λ_α` |
| SU-B | **Proposition 2**: if `G₁` is more dispersed than `G₀` (same mean, second-order), total earnings on the other side fall, under concavity of `θ, φ` and log-concavity of `1−F`. | Prop 2, p.154 | Template for signing an aggregate under a general spread |
| SU-C | The proof handles a **general** RS spread with an arbitrary finite number of crossings `k₀ < k₁ < … < k_{2n}` of the quantile functions: the quantile difference alternates in sign, each partial integral is signed by second-order dominance, concavity controls the recombination, and the result is integrated by parts against `ρ = (1−F)/f`. | Proof of Prop 2, eqs. (4)–(6) | **Open item 2** — the technique to transplant |
| SU-D | Proposition 3 and its corollaries give conditions under which every agent loses. | Prop 3, Cors 1–2 | Stronger conclusion, unused |
| SU-E | Suen's `μ` is cross-side (the other market's distribution moves) whereas our `T` is same-side (before and after), "but the map class is identical". | Survey's comparison | Positioning of P9-gen |
| SU-F | **§Verdict / open item 2:** "Two **independent** published sources — Suen's Proposition 2 and Costrell–Loury's Proposition 5 — sign an aggregate under a general mean-preserving spread using the same technique [...]" and the proposed extension is to "replace the pivot hypothesis with log-concavity of `1 − Λ` and apply the Suen integration to `k*`". | Survey's synthesis | Drives the whole proposed route for open item 2 |

SU-F quotes a superseded draft. The current `LITERATURE.tex` no longer
contains "Two independent published sources"; SU-G records the text that
replaced it.

## Claims in the current `LITERATURE.tex` and `PROOFS.tex` (added 2026-10-06)

Quoted verbatim from the LaTeX source. Line numbers are those of 2026-10-06
and go stale; the section anchor is the stable locator.

| ID | Document, anchor | Verbatim text | Suen locator |
|---|---|---|---|
| SU-G | `LITERATURE.tex` §sec:assignment, paragraph "Consequence for open item 2" (l.782–787) | "Two published sources sign an aggregate under a general mean-preserving spread, by the same first move but under \emph{different} hypotheses --- and they are not independent: Suen's proof imports the quantile-reversal step from Costrell--Loury by citation (``see, for example, [2]''), and his footnote~1 engages their rank-rescaling device directly." | p.154 below (6); p.151 fn. 1 |
| SU-H | `LITERATURE.tex` same paragraph, second bullet (l.799–802) | "\citet{Suen2007}'s Proposition~2 integrates by parts against $\rho = (1-F)/f$ and needs concavity of $\theta,\phi$ \emph{and} log-concavity of $1-F$ --- the latter being exactly equivalent to $\rho' \leq 0$ [...]" | Prop 2 and eq. (4), p.154; remark p.155 |
| SU-I | `LITERATURE.tex` same paragraph, obstacle 2 "The rescaling trap" (l.815–819) | "[...] a rank-space rescaling, which is precisely what Suen's footnote~1 declines to do: ``unless the density function $g$ is increasing, concavity of $\phi$ does not imply concavity of $\hat\phi$''. Curvature does not survive the change of variable." | p.151 fn. 1 |
| SU-J | `PROOFS.tex` §prop:tail, closing paragraph (l.1433–1439) | "\citet{Suen2007} and \citet{CostrellLoury2004} sign smooth aggregates and must therefore control the quantile difference at every rank, which is what forces integration by parts and a log-concavity or monotone-weight condition. The entry count reads the quantile function only down to the margin, so one-sided information suffices and no such condition is needed." | Proof of Prop 2, pp.154–155 |
| SU-K | `PROOFS.tex` "What genuinely remains", item 2 (l.1792–1797) | "Those techniques sign a smooth \emph{aggregate} under a general spread and therefore need integration by parts and, in Suen's case, log-concavity of $1-F$." | Prop 2 and eq. (4), p.154 |

## What is checkable here

- **SU-A** — `μ = G⁻¹∘F` increasing with `μ(0)=0`, `μ(1)=1` (SU-1); the wage
  ODE `(2)` and its SOC gap (SU-2).
- **SU-B/SU-C** — the operative role of the log-concavity hypothesis: `1−F`
  log-concave is *exactly* `ρ' ≤ 0` (SU-3); and the quantile-SOSD step the
  proof imports (SU-4).
- **SU-F is where the survey errs**, on the word *independent*. See
  `NOTES.md`.
- Two things the paper says that the survey does not carry, both checked:
  concavity is **sufficient but not necessary** (SU-6), and **footnote 1's
  warning that rank-rescaling destroys concavity** (SU-5). The second bears
  directly on the proposed route.
- **SU-D, SU-E** are textual/structural; confirmed by reading, unchecked.
- **SU-H** — whether concavity of `φ` can be dropped is checked in SU-7
  (continuum) and `control_convex_phi` (Lean).

## Lean claims (`Suen2007.lean`, added 2026-10-06)

Each row is a statement about the discrete skeleton of a step our documents
attribute to Suen. Analytic facts enter as named hypotheses; the order and
arithmetic content is proved over `Int` in core Lean 4. The index map is in
`NOTES.md`, section "Lean formalisation".

| ID | Statement proved | Theorems | Survey claim served | Suen locator |
|---|---|---|---|---|
| SU-L1 | Summation by parts. A weighted sum equals the last weight times the last partial sum plus the weight drops times the earlier partial sums. Second line of (6) with the sign corrected. | `abel_end`, `eq6_identity` | SU-C "concavity controls the recombination" | eq. (6), p.154 |
| SU-L2 | The printed second line of (6) is not an identity. A one-block witness has left side −1 and printed right side +1. | `eq6_printed_not_identity` | SU-C | eq. (6), p.154 |
| SU-L3 | Non-negative, non-increasing block weights and non-positive partial integrals at every even crossing give `Ĥ(k_{2i}) ≤ 0`. | `dsum_nonpos`, `eq6_sign` | SU-C "each partial integral is signed by second-order dominance" | eq. (6) and the sentence after it, p.154 |
| SU-L4 | First line of (6). The tangent bound, alternation of the quantile difference, and a non-increasing slope give `Ĥ(k_{2i}) ≤ Σ_j φ'(G₀⁻¹(k_{2j−1})) ∫_{block j}(G₁⁻¹−G₀⁻¹)`, and with SU-L3 `Ĥ(k_{2i}) ≤ 0`. | `recombination`, `range_bound`, `eq6_inequality`, `telescope`, `e_mono`, `eq6_hhat_nonpos` | SU-C "the quantile difference alternates in sign" | eq. (6), "where the inequality stems from the concavity of φ", p.154 |
| SU-L5 | Local-maximum step, weak form. `Ĥ ≤ 0` at the even crossings gives `Ĥ ≤ 0` at every rank up to the top crossing. | `psum_antitone_on`, `psum_monotone_on`, `block_cover`, `hhat_nonpos_between`, `cl_at_even_crossings` | SU-C | p.154 last lines |
| SU-L6 | Eq. (5) as summation by parts against `θ'`, and `H ≤ 0` from `θ' ≥ 0` non-increasing and `Ĥ ≤ 0`. | `eq5_identity`, `eq5_H_nonpos`, `psum_mul_eq_wsum` | SU-B (concavity of `θ`) | eq. (5), p.154; p.155 first line |
| SU-L7 | Eq. (4). First equality as a discrete Fubini step, last equality as summation by parts against `ρ` with `ρ` zero at the top, and the sign from `ρ` non-increasing and `H ≤ 0`. | `wsum_const_sub`, `product_rule`, `eq4_first_equality`, `eq4_identity`, `eq4_sign` | SU-C "integrated by parts against ρ"; SU-H | eq. (4), p.154; p.155 |
| SU-L8 | Proposition 2, weak inequality, chained through (6), the local-maximum step, (5) and (4); and the same chain with the Costrell–Loury step discharged from a shortfall (second-order) ordering of the two distributions. | `prop2_skeleton`, `prop2_from_sosd`, `psum_sub` | SU-B, SU-C | Prop 2 and proof, pp.154–155 |
| SU-L9 | The step Suen imports from [2]. A shortfall ordering of two distributions, with the dominated one given by its sorted (quantile) vector, orders every lower partial sum of the quantile vectors. | `psum_le_psum`, `psum_le_of_nonneg`, `shortfall_ge`, `shortfall_below`, `shortfall_above`, `shortfall_at_quantile`, `psum_const_sub`, `cl_quantile_reversal` | SU-G "imports the quantile-reversal step from Costrell--Loury by citation" | p.154, sentence citing [2] |
| SU-L10 | Our own observation, not attributed to Suen. A weighted sum is non-positive when the weight is non-increasing and non-negative at the top and the partial sums are non-positive wherever the weight drops; with pointwise slope weights `Ĥ ≤ 0` follows with no crossing points at all. | `dsum_nonpos_of_drops`, `wsum_nonpos_of_drops`, `psum_le_wsum`, `hhat_nonpos_direct` | SU-C "arbitrary finite number of crossings"; SU-J "control the quantile difference at every rank" | none (ours) |
| SU-L11 | Footnote 1, discrete form. A concave non-decreasing `φ` composed with a concave non-decreasing quantile map is concave; concavity of the quantile map is also necessary for this to hold for every such `φ`; a strictly concave `φ` composed with a convex quantile map is not concave. | `diff_anti`, `incr_shift`, `mono_shift`, `fn1_concave_comp`, `fn1_converse`, `fn1_iff`, `fn1_counterexample` | SU-I | p.151 fn. 1 |
| SU-LC1 | Control. An increasing weight with every partial sum non-positive gives a positive weighted sum. | `control_weight_increasing` | SU-B (concavity) | eq. (6), (5) |
| SU-LC2 | Control. A convex `φ = y²` on a mean-preserving spread, with every other hypothesis of the chain holding, gives `W₁ − W₀ = 2 > 0`; the tangent hypothesis is the one that fails. | `control_convex_phi` | SU-H | Prop 2, p.154 |
| SU-LC3 | Control. Weights dropping at every rank and one positive interior partial sum give a positive weighted sum. | `control_interior_rank` | SU-J | eq. (4), (6) |
| SU-LC4 | Control. A `ρ` that rises once, with `H ≤ 0` at every rank and `ρ` zero at the top, gives `W₁ − W₀ = 1 > 0`. | `control_rho_increasing` | SU-H, SU-K (log-concavity) | eq. (4), p.154 |
| SU-LC5 | Control. An unsorted vector satisfies the shortfall ordering yet reverses the first partial sum, so the quantile reading (sortedness) is load-bearing in SU-L9. | `control_cl_unsorted` | SU-G | p.154, sentence citing [2] |
