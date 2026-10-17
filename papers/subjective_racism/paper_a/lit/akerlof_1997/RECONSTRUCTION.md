# Akerlof (1997), reconstruction

George A. Akerlof, "Social Distance and Social Decisions", *Econometrica* 65(5),
September 1997, 1005–1027. The Fisher-Schultz Lecture of 1995.

## Source read

The JSTOR copy the author placed on Drive on 17 October 2026, cached at
`~/.cache/paper_a/sources/akerlof_1997.pdf` with its `pdftotext -layout` text,
sha256 pinned in `verify_akerlof_1997.py`. All 24 PDF pages were read, the
article, the appendix and the references.

## The model, from its own primitives

**Two representative-agent models, Section 2.** Each agent chooses `x`, and
`x̄` is everyone else's choice.

- Status, eq. (1). `U = −d(x̄ − x) − a x² + b x + c`. Falling behind costs at
  rate `d`. With `a > 0` the best reply is `(b + d)/2a` whatever `x̄` is,
  eq. (2), so in equilibrium `x` exceeds the intrinsic optimum `b/2a` by
  `d/2a`.
- Conformity, eq. (3). `U = −d|x − x̄| − a x² + b x + c`. Distance from
  others in either direction costs at rate `d`. Every `x̄` in
  `[(b − d)/2a, (b + d)/2a]` is a best reply to itself, eq. (4), a zone of
  equilibria, because the marginal utility of `x` jumps at `x̄`.
- Quadratic, eq. (5). `U = −a x² + b x + c − d(x − x̄)²`. The marginal loss
  of distance vanishes at zero, so the only equilibrium is `b/2a`. The paper
  calls this "a special case" (p. 7).

**Social distance, Section 3.** Each person `i` has an inherited position
`x₀ᵢ` and chooses an acquired position `x₁ᵢ`. Contact is "mutually beneficial
trade" whose value falls with distance. The benefit between `i` and `j` is
`e/((f + d₀ᵢⱼ)(g + dᵉᵢⱼ))`, with `d₀` the inherited distance and `dᵉ` the
expected acquired distance. It is a gravity model with two changes, a finite
benefit at zero distance and a dependence on both distances. The form follows
"the common practice" of the trade literature (p. 8). Under static
expectations `dᵉᵢⱼ = |x₁ᵢ − x₀ⱼ|`, and person `i` maximises eq. (6), the sum
of the benefits over the others plus the intrinsic value `−a x₁ᵢ² + b x₁ᵢ + c`.

**Class stability, Section 4.** With three people, two close and one far, and
a high value of exchange, the two close people choose each other's inherited
positions and the far one chooses near `b/2a`. The proof is in the Appendix.

**Sections 5 to 7.** Ethnographic and biographical cases (Whyte, Rainwater,
Brown, Anson, Rodriguez, Stack), and two interventions read through the
model (Lang's scholarship class, Comer's schools). The policy conclusion is
that programmes must be judged by their effect on group interaction.

## Derivation chain of the main results

1. Eq. (2) from eq. (1). The objective is concave in `x` with slope
   `d − 2a x + b`, zero at `(b + d)/2a` (`eq2_status_opt`).
2. Eq. (4) from eq. (3). At `x = x̄ + h`, the gain over `x̄` is
   `h(b − 2a x̄) − a h² − d|h|`, which is at most `−a h²` when
   `|b − 2a x̄| ≤ d` (`eq4_zone`).
3. Eq. (5). At `x = x̄ + t` the gain is `t(b − 2a x̄) − (a + d)t²`, which is
   positive for some `t` unless `x̄ = b/2a` (`eq5_unique`).
4. The benefit. It falls in `dᵉ`, is at most `e/((f + d₀)g)` at `dᵉ = 0`,
   and tends to zero (`benefit_antitone`, `benefit_le_at_zero`,
   `benefit_tendsto_zero`).

## What could not be reconstructed

- The three-person example. The Appendix shows a change of sign of the
  one-sided derivatives at `x₀₂` under conditions stated as "sufficiently"
  close, distant and high. No explicit bounds are given, so no special case
  with numbers is claimed.
- How positions update from one round to the next, pp. 11–12. Stated in words
  and outside Paper A1.
