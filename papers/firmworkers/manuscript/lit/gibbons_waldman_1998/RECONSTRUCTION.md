# Reconstruction of Gibbons and Waldman, "A Theory of Wage and Promotion Dynamics in Internal Labor Markets"

**Source read.** Robert Gibbons and Michael Waldman, "A Theory of Wage and
Promotion Dynamics in Internal Labor Markets", NBER Working Paper 6454, March
1998, 44 PDF pages (cover, acknowledgements, abstract, text pp.1–31, appendix
pp.31–37, references pp.37–40). Uploaded by the author on 10 Oct 2026 as
`w6454.pdf` on Drive (id `1Spu8Ubj5cgNJ7LYYSJ3z2L0dr1J8Gf7V`). The ledger cites
the published version, "A Theory of Wage and Promotion Dynamics Inside Firms",
*Quarterly Journal of Economics* 114(4), 1999, 1321–1358, which was **not**
read. By the author's decision of 10 Oct 2026 the working paper is cited as
the version read, and pages here are its own. The PDF is a scan with no text layer. It is cached at
`~/.cache/firmworkers/gibbons_waldman_1998_wp.pdf`, sha256
`c2532455b6317169b726a2577cd2e8bf7d101e326f1ffe2e6845065e659d0bcd`. Text comes
from Drive's own OCR of the same file, cached at
`~/.cache/firmworkers/gibbons_waldman_1998_wp.ocr.txt`, sha256
`c12dfbbb1c174582f13fb69b59e49d963bf3e5363717eef4d55f4c55f35942a8`. Printed
page numbers sit at the top of each page (PDF page `p + 3` is printed page `p`)
and survive in the OCR as standalone lines except on pp.1, 2, 4, 5 and 9. Read
in full from the OCR [F], with pp.8, 9, 10, 11, 16, 20, 31, 32 and 33 also read
from page images at 90 dpi for the equations.

**Why this paper.** Ledger row L2. It is the precedent for a micro state that
joins a job ladder with on-the-job human capital and learning, the state
`(r, h)` the firmworkers model may adopt (TODO.md, pending decisions).

## Primitives (Section III.A, pp.8–9)

- Free entry, identical firms, labor the only input; careers of `T ≥ 5`
  periods; risk neutrality, zero discount rate, spot contracts, wages paid in
  advance; ties in wage offers broken in favour of the current employer, so no
  turnover in equilibrium.
- Innate ability `θ_i ∈ {θ_H, θ_L}`; effective ability (1)
  `η_it = θ_i f(x_it)`, `f' > 0`, `f'' ≤ 0`, `x_it` prior experience.
- Three jobs. Output (2) `y_ijt = d_j + c_j(η_it + ε_ijt)`, `ε ~ N(0, σ²)`,
  `c_3 > c_2 > c_1`, `d_3 < d_2 < d_1`. Thresholds: `η'` solves
  `d_1 + c_1η = d_2 + c_2η`, `η''` solves `d_2 + c_2η = d_3 + c_3η`, with
  `η'' > η'`.
- Parameter restrictions (p.9): `θ_H f(1) < η'`;
  `η'' − η' > θ_H[f(3) − f(1)]`; `θ_L f(T − 1) > η''`.

## Derivation chain

1. **Thresholds.** Job `B` beats job `A` (with `c_A < c_B`) iff `η` is at least
   `(d_A − d_B)/(c_B − c_A)`. Lean: `prefer_higher_iff`, `prefer_lower_iff`.
2. **Proposition 1 (p.10).** Under full information the wage is
   `max_j (d_j + c_jη)`, equal to job 1's output below `η'`, job 2's on
   `[η', η'')`, job 3's from `η''`. Lean: `prop1_job1`, `prop1_job2`,
   `prop1_job3`.
3. **No demotions, rising wages (p.10).** The wage is strictly increasing in
   `η`, and `η = θf(x)` rises with experience. A fall in job level requires a
   fall in `η`, hence in the wage. Lean: `wage_strictMono`,
   `full_info_wage_rises`, `jobOf_mono`, `demotion_implies_wage_decrease`.
4. **Serial correlation and promotion (pp.11–12).** Within job 1 the wage
   increase from `x` to `x + 1` is `c_1θ[f(x + 1) − f(x)]`, larger for `θ_H`; job
   level is monotone in `η`, so high-ability workers are promoted no later.
   Lean: `serial_correlation_full_info`, `high_ability_promoted_no_later`.
5. **Raise at promotion (pp.12–13).** The raise from job 1 to job 2 is
   `c_1(η_2 − η_1)` plus the gain from reassignment, `(d_2 + c_2η_2) −
   (d_1 + c_1η_2) ≥ 0`. Lean: `promotion_raise_decomposition`,
   `promotion_raise_exceeds_within_job`.
6. **Symmetric learning (Section IV, pp.14–15; Appendix pp.32–34).** Normalized
   output `z = (y − d_j)/c_j = η + ε` is independent of the job. With prior `p`
   on `θ_H`, Bayes' rule (A1) gives `q = p/(p + (1 − p)L)`, `L` the likelihood
   ratio (A2), `exp[−(1/2σ²){(z − θ_L f)² − (z − θ_H f)²}]`, which is strictly
   decreasing in `z`. So `q` rises in `z` and in `p`, tends to 1 as `z → ∞` and
   to 0 as `z → −∞`, and has expectation `p`. Lean: `likelihoodRatio_A2`,
   `likelihoodRatio_strictAnti`, `posterior_strictAnti_ratio`,
   `posterior_strictMono_prior`, `posterior_strictMono_signal`,
   `likelihoodRatio_affine`, `belief_tends_to_high`, `belief_tends_to_low`,
   `beliefs_martingale`.
7. **Corollary 2.3 (p.19, proof p.31–32).** `f(x)/f(x + 1)` is increasing for
   concave increasing `f`, so once `θ_L f(x + 1) < θ_H f(x)` it stays so. Before
   that point expected effective ability cannot fall, so no wage decreases;
   after it, beliefs moving from near `θ_H` to near `θ_L` make a wage decrease
   possible. Lean: `ratio_increasing_of_concave`,
   `wage_decrease_threshold_persists`, `no_wage_decrease_before_threshold`,
   `wage_decrease_possible`.
8. **Corollary 2.4 (p.22, proof pp.32–33).** The probability that `z` clears a
   cutoff is linear in `p` with positive slope (A4). Lean:
   `promotion_probability_A4`, `promotion_probability_increasing`.
9. **Section V.** Performance evaluations read as measures of expected innate
   ability `θ^e` rather than of productivity `η^e`; four BGH findings the model
   does not capture (cohort effects, nominal rigidity, overlapping wage
   distributions, green-card effect).

## What could not be reconstructed

- Corollaries 2.1 and 2.2 and the full probability statement of Corollary 2.4
  need the distribution of `z` and first-order stochastic dominance arguments;
  only their deterministic cores (monotone posterior, linear (A4)) are
  formalised.
- The BGH percentages quoted in Sections II–V are data, not checked.
- The QJE version may differ in numbering and content.
