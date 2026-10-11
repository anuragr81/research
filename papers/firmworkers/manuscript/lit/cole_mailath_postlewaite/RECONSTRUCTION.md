# Reconstruction of Cole, Mailath and Postlewaite, 1992 and 1995

**Sources read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| CMP92 | "Social Norms, Savings Behavior, and Growth", *Journal of Political Economy* 100(6), December 1992, Centennial Issue, pp.1092–1125 | JSTOR scan with text layer, 35 PDF pages (cover plus 34), Drive `cole_status_value.pdf`, id `16WUA3v3ofhgiCMQ7QMSjYU7p5tNrTd7K`; cached `~/.cache/firmworkers/cmp_1992.pdf`, sha256 `6dd143cf39b4323b9b184a74cf123a7d6e8a3fa1d22387eda1c5f0a7cd08635d` | [F], published |
| CMP95 | "Incorporating Concern for Relative Wealth into Economic Models", CARESS Working Paper 95-14, University of Pennsylvania, 1 August 1995, "Forthcoming, Quarterly Review, Federal Reserve Bank of Minneapolis", text pp.1–18 | scan with OCR text layer, 24 PDF pages, Drive `colemaliath_incorporating_concern.pdf`, id `1E8Q-6SfQTqG742yjCVBhqXD3xYkCizRg`; cached `~/.cache/firmworkers/cmp_1995_wp.pdf`, sha256 `4534ad07a47f68fe35bc29bfa1c82819b18b73f2d91c403f8db59e2a584bc93a` | [F], working paper; the Quarterly Review version is not read |

Text extracted with `pdftotext -layout`. CMP92's page `p` is block `p − 1091`
of the text; its headers are garbled on pp.1103, 1106, 1108 and every display
is garbled, so §III.D (pp.1108–1112) was read from page images at 100 dpi.
CMP95's printed page number closes each block; the CARESS paper list at the
back reuses 2–4, so pages are taken in increasing order only. Both papers were
read on 11 Oct 2026. The entry-contest bundle read both earlier for a
different use (`papers/jite/endogenous_entry/entry_contest/lit/cole_mailath_postlewaite`),
reconstructed §IV.A and Property 1, and said plainly that §III.D was not
reconstructed. This record reconstructs §III.D, which is what the firmworkers
model needs.

**Why these papers.** The author's decision of 11 Oct 2026: human capital `h`
in the firmworkers micro state is social standing, which firms observe but do
not price, and the Weberian line between contractual (market) and
non-contractual (status) relations must hold. CMP92 is the formal model of
exactly that line. Its aristocratic equilibrium is the case in which status is
assigned outside the market and market decisions are undistorted; its
wealth-is-status equilibrium is the case in which status answers to market
outcomes and distorts them.

## CMP92, primitives (§III, pp.1098–1100)

- Overlapping generations, a continuum of men `i ∈ [0, 1]` and women
  `j ∈ [0, 1]`, one-period lives, one son and one daughter per pair. Woman `j`
  holds a non-traded, non-storable endowment `j`, uniform on `[0, 1]`.
- CRRA utility over joint consumption, `u(c) = (1 − γ)^{-1} c^{1−γ}`; the
  woman's endowment enters linearly; the son's utility enters with discount
  `β ∈ (0, 1)`. Technology (1): `c = Ak − k′`, `A > 1`.
- Status ranks the men and decides matching (p.1099). A social norm is a
  status-assignment rule plus status-dependent matching; an equilibrium is a
  norm plus market decisions from which no one deviates.
- Voluntary matching: no pair prefers each other to their matches; any set of
  matched men is matched with a set of women of the same measure.

## CMP92, derivation chain used here

1. **Status as the ranking device for the non-market sphere** (abstract,
   p.1093, p.1096). Formalised as a rank: `rankBelow` is monotone in the
   ranked quantity, the bottom agent has rank zero, and removing agents from
   above leaves a rank unchanged (the "competition from below" of CMP95 p.7).
2. **Two norms** (p.1097): status inherited, or status by relative wealth.
3. **Wealth-is-status** (§IV.C, pp.1105–1107). Matching by rank in capital;
   the first-order condition (10) carries a matching term `m′ ≥ 0`, so the
   bequest exceeds the exogenous-matching bequest (Property 2) and no family
   changes rank (Property 1). Formalised in sign form:
   `wealth_is_status_foc_distorts`.
4. **Aristocratic** (§III.D, pp.1108–1113). Status rule (12):
   `n(s, j) = s` if `s = j`, else `0`. Status is kept along a path of
   appropriate matches and is zero forever after one deviation
   (`statusRule_keep`, `statusRule_lose`, `status_zero_absorbing`,
   `status_path_keep`, `status_path_lose`). With matching by status the value
   is `V(k, s) = V*(k) + s/(1 − β)`: the status stream sums to `s/(1 − β)`
   (`status_stream_value`), and a constant added to the objective leaves the
   bequest choice unchanged (`argmax_unchanged_by_constant`,
   `aristocratic_separable`), so the capital path is the exogenous-matching
   path and the aristocratic first-order condition is (4)
   (`foc_aristocratic_is_exogenous`).
5. **Who deviates** (13), p.1110. Man `i` will not match with woman `j` iff
   `(1 − β) j ≤ i` (`no_deviation_iff`); men with `i < (1 − β) j` want to
   deviate (`low_status_deviates`); men with `i ≥ 1 − β` never do
   (`high_status_never_deviates`).
6. **The indifference level** `L_t(j)`, pp.1109–1110. With the exogenous value
   `V*(k) = C k^{1−γ}`, the woman's indifference
   `V*(L) + j = V*(k) + j/(1 − β)` solves to
   `L^{1−γ} = k^{1−γ} + (β/(1 − β)) j / C` (`indifference_level`), and with
   `C = (λ*)^{-γ} A^{1−γ}/(1 − γ)` this is the printed form
   `L_t(j)^{1−γ} = k_t(j)^{1−γ} + (β/(1−β)) j (1−γ)(λ*)^γ A^{γ−1}`
   (`indifference_printed_form`). The constant `C` comes from (8) with the
   denominator `1 − β[(1 − λ*)A]^{1−γ} = λ*` (`lambda_star_identity`,
   `value_closed_form_denominator`). For `γ > 1` and `j > 0`, `L > k`
   (`L_exceeds_k`); at `j = 0`, `L = k` (`L_equals_k_at_zero_status`).
7. **Spread-out capital blocks deviation**, p.1111: if `L_t(j) ≥ A k_{t−1}(j)`
   no feasible bequest reaches `L_t(j)` (`spread_out_blocks_deviation`).
   Proposition 2's derivative condition (15) is not formalised.

## CMP95, what is used

- The concern for relative standing is instrumental (abstract, p.2) and
  arises from the absence of a market for the prize (fn.4 p.3, §4 p.15).
- Effort model §2: `m(y)` is the distribution function of output; the
  closed form of §2.1 gives `l(j) = a(j)(1 + g)/2` from the first-order
  condition (`cmp95_effort_closed_form`), the matching consistency
  `m(y(j)) = j` reduces to `g(1 + g)α² = 1` (`cmp95_match_consistent`), the
  slope `g` falls as productivities disperse
  (`cmp95_slope_falls_with_dispersion`), and the bottom agent is undistorted
  (`bottom_effort_undistorted`, with `rankBelow_bottom`).
- Footnote 1 (p.1) places the view in Weber's *Protestant Ethic*.

## What could not be reconstructed

- CMP92 Proposition 1 (existence of wealth-is-status equilibria, Appendix A)
  and Properties 3–6; the Lemma and Proposition 2 of §III.D beyond the
  indifference algebra, in particular the derivative inequality (15).
- CMP92 §IV.A is reconstructed in the entry-contest record, not repeated here.
- CMP95 §3 (signaling) and its Appendix.
- The Quarterly Review version of CMP95.
