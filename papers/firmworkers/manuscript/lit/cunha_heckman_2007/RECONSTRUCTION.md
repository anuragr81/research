# Reconstruction of Cunha and Heckman, "The Technology of Skill Formation"

**Source read.** Flavio Cunha and James Heckman, "The Technology of Skill
Formation", NBER Working Paper 12840, January 2007, 38 pages (cover, abstract
page, text pp.1–24, references pp.25–31, notes pp.32–34, Table 1, Figures 1–2).
Uploaded by the author on 10 Oct 2026 as `w12840.pdf` on Drive (id
`1S3862WYM57yaC821ss3XMxx_0p4XGHUr`). Read in full [F]. The ledger cites the
published version, *American Economic Review* 97(2), 31–47 (Papers and
Proceedings, May 2007), which was **not** read. By the author's decision of 10 Oct 2026 the working paper
is cited as the version read, and page numbers here are its own. Cached at
`~/.cache/firmworkers/cunha_heckman_2007_wp.pdf`, sha256
`b78d465c373fd588ac86dd8a4b98faad32aa4ad1093bb2c76138a43027015d84`; text
extracted with `pdftotext -layout` to `cunha_heckman_2007_wp.txt`, one form
feed per PDF page, printed page `p` in block `p + 2`. Displayed equations on
pp.12–18 were read from page images at 100 dpi.

**Why this paper.** Ledger row L1. It is the candidate law of motion for the
human-capital stock `h` in the firmworkers micro state (TODO.md, pending
decisions), and it defines self-productivity and dynamic complementarity.

## Primitives (Section II, pp.6–12)

- Overlapping generations; an individual lives `2T` years, a child for the
  first `T`. Parental investment `I_t` at child age `t`, parental
  characteristics `h`, skill vector `θ_t`, initial condition `θ_1`.
- Technology (1): `θ_{t+1} = f_t(h, θ_t, I_t)`, strictly increasing and
  strictly concave in `I_t`, twice differentiable. Recursive form (2):
  `θ_{t+1} = m_t(h, θ_1, I_1, …, I_t)`.
- Self-productivity: `∂f_t/∂θ_t > 0` (p.10). Dynamic complementarity:
  `∂²f_t/∂θ_t∂I_t' > 0` (p.9).
- Critical and sensitive periods (p.10), defined through the partial
  derivatives of `m_t` in each `I_s`.
- Two-period case `T = 2`, adult skill `h' = m_2(h, θ_1, I_1, I_2)` (3). The
  one-period literature is (4), `m_2(h, θ_1, γI_1 + (1 − γ)I_2)` with `γ = 1/2`;
  Leontief is (5), `m_2(h, θ_1, min{I_1, I_2})`; the CES nesting both is (6),
  `m_2(h, θ_1, [γI_1^φ + (1 − γ)I_2^φ]^{1/φ})`, `φ ≤ 1`, `0 ≤ γ ≤ 1`.

## Derivation chain

1. **Timing under perfect substitutes (p.11).** With (4) and `γ = 1/2`,
   children with profiles `(0, I)` and `(I, 0)` reach the same adult skill.
   Lean: `ces_perfect_substitutes`, `timing_irrelevant_at_half`.
2. **Leontief (pp.11–12).** With `I_1 = 0` the aggregate is zero whatever
   `I_2`; under a budget `I_1 + I_2/(1 + r) = E` the aggregate is at most its
   value at `I_1 = I_2`. Lean: `leontief_no_early_no_output`,
   `leontief_even_is_optimal`, `leontief_even_attains`. The increasing
   differences of `min` are the discrete form of dynamic complementarity, and
   perfect substitutes have none. Lean: `leontief_dynamic_complementarity`,
   `substitutes_no_complementarity`.
3. **Perfect substitutes with prices (p.14).** Along the budget, output is
   linear in `I_1` with slope `γ − (1 − γ)(1 + r)`, so all-early beats all-late
   iff `γ > (1 − γ)(1 + r)`. Lean: `output_along_budget`, `invest_early_iff`.
4. **Optimal ratio (9) (p.14).** At an interior optimum the marginal rate of
   transformation equals the price ratio,
   `γI_1^{φ−1} = (1 + r)(1 − γ)I_2^{φ−1}`, whose unique positive solution is
   `I_1/I_2 = [γ/((1 − γ)(1 + r))]^{1/(1−φ)}`. At `φ = 0` the ratio is
   `γ/((1 − γ)(1 + r))`; for every `φ < 1` it rises in `γ`. Lean:
   `optimal_ratio_of_foc`, `optimal_ratio_cobbDouglas`,
   `optimal_ratio_strictMono_gamma`.
5. **Third credit constraint (pp.17–18).** With `s ≥ 0` and `b' ≥ 0` binding,
   `c_1 = wh + b − I_1` and `c_2 = w(1 + α)h − I_2`. From (8) the conditions for
   `I_1` and `I_2` are `u'(c_1) = Kγ I_1^{φ−1}` and
   `βu'(c_2) = K(1 − γ)I_2^{φ−1}` with a common `K > 0` (the discounted value of
   adult skill times the derivative of the aggregator). With
   `u'(c) = c^{σ−1}` they give
   `(I_1/I_2)^{1−φ} = (γβ/(1 − γ))(c_1/c_2)^{1−σ}`. Lean:
   `constrained_ratio_power_of_focs`, `constrained_ratio_of_focs`. The printed
   display differs (CLAIMS.md, CH-D1).
6. **Section V–VI.** Estimates and simulations are cited from Cunha and Heckman
   (2006a, b) and Cunha, Heckman and Schennach (2006b); Table 1 reports the
   simulated outcomes of four policies.

## What could not be reconstructed

- The first-order conditions of (8) and their interiority are taken as
  hypotheses; existence of an optimum is not shown.
- The p.15 claim that a binding `b' ≥ 0` lowers both investments
  (`Î_1 < I_1^*`, `Î_2 < I_2^*`) is stated without derivation; not formalised.
- The general CES cross-partial for `φ < 1` is not formalised; only the
  Leontief and linear limits are.
- The estimates and simulations of Sections V–VI and Table 1 come from other
  papers; only the arithmetic of the text against Table 1 is checked.
