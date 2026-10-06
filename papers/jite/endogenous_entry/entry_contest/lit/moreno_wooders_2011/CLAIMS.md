# Claims about Moreno & Wooders (2011)

**Paper.** Diego Moreno, John Wooders, "Auctions with heterogeneous entry
costs", *RAND J. Econ.* 42(2), 2011, pp. 313–336. Evidence level **[F]**,
published version.

This is the only literature paper cited in `PROOFS.tex` for a substantive
modelling contrast rather than for context.

## Claims as stated in `PROOFS.tex` §Primitives and `LITERATURE.tex`

| ID | Claim | Source locator | Relevance claimed to `PROOFS.tex` |
|---|---|---|---|
| MW-A | Privately known entry costs yield a **symmetric Bayesian threshold in cost space**. | §4, p.320 | `PROOFS.tex` §Primitives, the P5 contrast |
| MW-B | The entrant count is **binomial**; the number of entrants is random. | §4, p.320, `B(N,H(t))` | Same |
| MW-C | "the identities are not determined" | `PROOFS.tex` §Primitives | Same |
| MW-D | What separates P5 from a private-cost threshold is the **complete-information assumption** on the wealth profile — which `PROOFS.tex` uses but (before the rebuild) did not state. | Survey's Verdict, "second sharpening" | P5 (`prop:P5`); §Primitives *Information* paragraph |
| MW-E | Proposition 2 gives a unique symmetric entry equilibrium `t*(v,φ)` solving `U(v,H(t)) = t + φ`, decreasing in `v` and `φ`. | Prop 2 | Background |
| MW-F | Proposition 3: zero screening value and zero admission fee maximise social surplus. | Prop 3 | **Welfare section** (deferred) |

## What is checkable here

- **MW-A, MW-B** confirmed verbatim from p.320 and checked: the count has
  variance `N·H(t)(1−H(t)) > 0` (MW-1), so it is genuinely random.
- **MW-E** checked by implicit differentiation of `(3)` (MW-2).
- **MW-D** is the load-bearing separation. MW-3 contrasts the two counts by
  variance; **MW-4 states the sharper mechanical version** — MW's threshold is
  *flat*, P5's is *rank-dependent* — which the survey does not draw out.
- **MW-F** placed against Levin–Smith's two benchmarks (MW-5): cost
  heterogeneity does **not** move the welfare branch.
- **MW-C** needs care and is discussed in `NOTES.md`; as written it is
  imprecise.
