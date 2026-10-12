# Claims about Kőszegi and Rabin (2006)

**Source.** KR, *QJE* 121(4) pp.1133–1165, read in full [F]. See
`RECONSTRUCTION.md` for the copy, sha256 and cache path. Cited in
`claims.yaml` as row L19 and in `refs.bib` as `KoszegiRabin2006`. Pages are the
printed pages. Quotations are checked verbatim on their page by `verify_kr.py`.
Results are proved in `lean/Lit/KoszegiRabin2006.lean` (see `LEAN`), namespace
`KoszegiRabin2006`.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| KR-1 | 1133 | "the reference point is determined endogenously by the economic environment" | An endogenous reference |
| KR-2 | 1133 | "We assume that a person’s reference point is her rational expectations held in the recent past about outcomes" | The reference is own expectations |
| KR-3 | 1133 | "In deterministic environments, choices maximize consumption utility, but gain–loss utility influences behavior when there is uncertainty." | Reference dependence acts only under uncertainty |
| KR-4 | 1135 | "a salary of $50,000 to an employee who expected $60,000 will not be assessed as a large gain relative to status-quo wealth, but rather as a loss relative to expectations of wealth" | Labour-market losses are relative to expected pay |
| KR-5 | 1135 | "to our knowledge our paper is the first to formalize the idea that expectations determine the reference point and to specify a rule for deriving them endogenously in any environment" | Their novelty claim |
| KR-6 | 1136 | "a universally applicable, zero-degrees-of-freedom way to translate any existing" | No free reference parameter |
| KR-7 | 1139 | "Assumption A3 captures another important feature of gain–loss utility, diminishing sensitivity" | Convexity in losses |
| KR-8 | 1139 | "Loss aversion is captured by A2 for large stakes and A4 for small stakes." | A2's role |
| KR-9 | 1141 | "when large losses in consumption or wealth are involved, diminishing marginal utility of wealth as economists conventionally conceive of it is likely to counteract the diminishing sensitivity in losses emphasized in prospect theory" | Large losses temper convexity below the reference |
| KR-10 | 1141 | "our model makes the extreme assumption that the reference point is fully determined by the expectations a person held in the recent past" | Expectations only |
| KR-11 | 1141 | "The most common assumption, of course, has been that the reference point is the status quo." | The status-quo alternative |
| KR-12 | 1142 | "a decrease in salary is not a reduction in the status-quo level of wealth—it is a reduction from the expected rate of increase in wealth" | Wage cuts as losses against expectations |
| KR-13 | 1145 | "When a loss-averse decision-maker’s choice set is deterministic and all choices in it are deterministic, the predictions of PPE are identical to those of a model based solely on consumption utility" | Proposition 3 in words |
| KR-14 | 1146 | "in environments with uncertainty even applying PPE reference dependence can hugely influence behavior" | Uncertainty is where it bites |
| KR-15 | 1148 | "in violation of the law of demand, a decrease in the price distribution decreases the consumer’s willingness to pay for the shoes" | The comparison effect |
| KR-16 | 1150 | "A person’s target, however, is likely to be endogenous to her situation." | Targets are endogenous |
| KR-17 | 1150 | "If a driver generally makes around $200 a day, her target is probably close to $200" | The target follows own typical income |
| KR-18 | 1156 | "Our model takes as given the set of consumption dimensions over which gain–loss utility is separately defined." | A stated limitation |
| KR-19 | 1157 | "the nature and scope of reference-dependent choices seems to reflect mistakes our fully rational model does not capture" | A stated limitation |

## Results in Lean (`lean/Lit/KoszegiRabin2006.lean`)

| Result | Lean | What is proved |
|---|---|---|
| A0 and A2 imply loss aversion at every stake (p.1139; used on p.1157) | `KoszegiRabin2006.symmetric_sum_nonpos`, `KoszegiRabin2006.symmetric_sum_neg` | with `μ(0) = 0`, `μ` continuous at 0 and A2, `μ(y) + μ(−y) < 0` for every `y > 0` |
| Proposition 1.2, one dimension (p.1140) | `KoszegiRabin2006.status_quo_bias` | with `m` strictly increasing and `c ≠ c′`, `u(c|c′) ≥ u(c′|c′)` implies `u(c|c) > u(c′|c)` |
| Piecewise-linear `μ` of §§IV–V (p.1146) | `KoszegiRabin2006.mu_symmetric_sum`, `KoszegiRabin2006.mu_satisfies_A2` | `μ(x) + μ(−x) = −η(λ − 1)|x|`, so A2 holds when `η > 0` and `λ > 1` |
| Proposition 3's core (pp.1145, 1161) | `KoszegiRabin2006.expected_gain_loss`, `KoszegiRabin2006.expected_gain_loss_nonpos` | ex ante expected gain–loss utility of a finite lottery is `−(η(λ − 1)/2) ΣΣ p_i p_j |x_i − x_j|`, nonpositive |
| Proposition 2, one dimension (p.1140) | `KoszegiRabin2006.linear_consumption_utility` | with `m` affine, `u(c|r) − u(r|r)` is a function of `c − r` alone |
| Shopping thresholds (p.1146) | `KoszegiRabin2006.pmin_lt_one_lt_pmax` | `p_min < 1 < p_max` |
| Inequalities (6) and (7) (p.1148) | `KoszegiRabin2006.attachment_effect`, `KoszegiRabin2006.comparison_effect` | from (4) and (5), buying at `p_M` beats not buying exactly under (6) when `p_L = 0`, and under (7) when `q_L = 1` |

Controls: `KoszegiRabin2006.control_A2_needs_loss_aversion`,
`KoszegiRabin2006.control_effects_need_loss_aversion`.

## Readings recorded

- KR-D1 (p.1157): the proof of Proposition 1.2 ends "By A2 ... this is a
  contradiction", which needs `μ(x) + μ(−x) < 0`. A2 compares stakes and does not
  give this alone; with A0's continuity at 0 it does (`symmetric_sum_neg`). A
  gap in the stated argument, not an error.
- KR-D2: footnote 15 reads "if and only if x < y" as "if x < y and only if
  x ≤ y"; the Lean versions of (6) and (7) state the strict comparison both ways.
