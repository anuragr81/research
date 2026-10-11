# Notes — Cole, Mailath and Postlewaite (1992, 1995), pass 1 (both read in full)

Run `python3 lit/cole_mailath_postlewaite/verify_cmp.py` from the bundle
root. It checks the pinned sha256 of both scans, the title pages, that CMP92's
text blocks line up with the printed pages and that CMP95's pages 1–18 are
recovered in increasing order, each quotation verbatim on its page, three
controls, and measures `lean/Lit/ColeMailathPostlewaite.lean` (build root,
build, no `sorry`, every cited name declared, every declared theorem audited,
allowed axioms only, controls present).

## What was verified

Our reading of CMP92 §II, §III.D and the sign content of §IV.C, and of CMP95
§2 and §4, is consistent with the sources:

- Status rule (12) keeps status along appropriate matches and sends it to
  zero, forever, after one deviation.
- With matching by status the value separates, `V(k, s) = V*(k) + s/(1 − β)`,
  so the bequest choice and the first-order condition are those of the
  exogenous-matching model. This is the sentence we rely on most (CMP92-19).
- Condition (13), `(1 − β) j ≤ i`, is exactly the no-deviation condition, and
  men of status below `(1 − β) j` want to deviate.
- The indifference level `L_t(j)` has the printed closed form once the
  denominator of (8) is shown to equal `λ*`; `L_t(j) > k_t(j)` for `γ > 1` and
  `j > 0`, with equality at `j = 0`.
- Under wealth-is-status the matching term in (10) raises saving above the
  exogenous level (the sign of Property 2).
- CMP95's closed form: `l(j) = a(j)(1 + g)/2`, consistency `g(1 + g)α² = 1`,
  and `g` falling in `α`.

## What could not be verified

- Existence of wealth-is-status equilibria (Proposition 1) and Properties 3–6.
- The Lemma of §III.D and the derivative inequality (15) of Proposition 2;
  only the one-step feasibility statement is formalised.
- CMP95 §3 and its Appendix; the Quarterly Review version of CMP95.

## Discrepancies and readings

- CMP-D1 (p.1110): the text layer prints `β` as "3"; the quotation starts after
  it. The sentence itself is consistent with (13).
- CMP-D2 (p.1109): `V(k, s) = V*(k) + s/(1 − β)` needs the per-period match
  value to equal the status index, which (12) and the matching rule give on
  the equilibrium path.
- CMP-D3: existence of the aristocratic equilibrium is shown only for `γ > 1`
  and a spread-out capital distribution.
- CMP-D4: garbled headers on pp.1103, 1106, 1108 of the JSTOR text layer.
- CMP95 OCR: "offemales" (p.5), "wealtb" (p.9), soft hyphens at line ends;
  the normaliser now treats a soft hyphen as a hyphen, and quotations avoid
  the other errors.

## Findings from the formalisation

1. The no-deviation condition (13) does not involve wealth: `V*(k)` cancels,
   so under inherited status a rich low-status man is kept from a better
   match only by feasibility (`spread_out_blocks_deviation`), never by
   preference. For the firmworkers model this is the point: once `h` is
   inherited, the block on crossing status lines must come from outside the
   market, which is where categorical friction enters.
2. Separability needs the match value to be independent of the bequest
   (`control_separability_needs_match_independent_of_capital`): with any
   wealth-dependence of status, the market decision is distorted. That is the
   formal content of the author's objection to merging `h` into `b`.
3. The denominator of (8) is `λ*` exactly (`lambda_star_identity`), which the
   paper uses without stating.
4. `L_t(j) > k_t(j)` is proved from `γ > 1` and `j > 0`
   (`L_exceeds_k`); the paper's "L_t(j) exceeds k_t(j)" is stated without the
   `γ > 1` qualifier on p.1110, though Proposition 2 assumes it.

## Controls

| Check | Control |
|---|---|
| Quotations | CMP-C1, CMP92-19 with "identical" replaced by "differs", absent; CMP-C2, CMP95-5 with "below" replaced by "above", absent; CMP-C3, CMP92-15 looked up on p.1109, absent |
| Quotation suite | mutation-tested 11 Oct 2026: an altered word in each paper, a wrong page in each paper, an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | six `control_*` theorems, one per cluster (rank, status rule, separability, deviation, L_t(j), CMP95 slope) |

## Relevance to the firmworkers ledger

By the author's decision of 11 Oct 2026, `h` is social standing, observed by
firms but not priced. CMP92's aristocratic equilibrium is the formal model of
that: status assigned outside the market, kept by a norm, and leaving market
decisions undistorted. Its wealth-is-status equilibrium is the case the
decision rejects, in which status answers to market outcomes and distorts
them. The status rule (12) is a candidate law of motion for `h` within the
model: constant on the equilibrium path, with any evolution left to the
intergenerational layer. No model row rests on these papers yet.
