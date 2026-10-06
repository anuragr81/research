# Claims about Moreno & Wooders (2011)

**Paper.** Diego Moreno and John Wooders, "Auctions with heterogeneous entry
costs", *RAND Journal of Economics* 42(2), Summer 2011, pp. 313–336. Evidence
level **[F]**, published version, read in full on 2026-10-06 from the Drive
file `RANDMorenoWooders2011.pdf` (id `1VtK3cH_x1tuX598QIKAxebSo97dvDKSH`). The
article states that it "is based on Moreno and Wooders (2006)", Universidad
Carlos III Working Paper 06-18, which was not read. Proposition numbers are
those of the published article.

Moreno–Wooders is the only literature paper `PROOFS.tex` cites for a
substantive modelling contrast rather than for context.

Line numbers are as read on 2026-10-06 and will go stale. Each row also quotes
a short anchor from our text so the row can be found without them.

## Claims our documents make about the paper

| ID | Claim, as worded in our documents | Where we say it | Source locator | Lean theorems |
|---|---|---|---|---|
| MW-A | privately known entry costs yield "a symmetric Bayesian threshold in cost space" | `PROOFS.tex` §Primitives l.553–554; `LITERATURE.tex` "Information." l.528–529 | §4, pp.319–320 (threshold strategy), p.320 (definition of a symmetric entry equilibrium) | `mem_entrants`, `eq3_gives_eqLit`, `eqLit_forces_eq3` |
| MW-B | "a *binomial* entrant count"; "The number of bidders is $\mathrm{Binomial}(N, H(t^*))$" | `PROOFS.tex` l.554; `LITERATURE.tex` l.512–513 | §4, p.320 | `count_not_pinned` (non-degeneracy only) |
| MW-C | "there the number of entrants and the identity of the entering set are both random, being determined by unobserved cost draws" | `PROOFS.tex` l.555–556 | p.314 (costs drawn independently and privately observed), pp.319–320 (threshold rule) | `entrant_set_not_pinned` |
| MW-D | complete information on the wealth profile is what separates P5 from a private-cost threshold | `LITERATURE.tex` l.528–536, l.1044–1047, l.1124–1126; `PROOFS.tex` l.533–540 | Our own claim. The source side is p.319, "each buyer *i* has a privately known entry cost *Z_i*" | carried by MW-G and MW-H |
| MW-E | "Their Proposition~2 gives a unique symmetric equilibrium in threshold form: enter iff $z < t^*(v,\phi)$, where $t^*$ solves $U(v, H(t)) = t + \phi$ and is decreasing in both instruments." | `LITERATURE.tex` l.510–512 | Proposition 2, p.320; proof pp.328–329 | `eq3_gives_eqLit`, `eqLit_forces_eq3`, `tstar_strict_anti_phi`, `tstar_strict_anti_v`, `corner_eqTie`, `corner_not_eqLit`, `corner_eq3_fails` |
| MW-F | "Proposition~3: $v = \phi = 0$ maximises social surplus, because the marginal type's private gain equals its social contribution"; used to pre-empt the heterogeneity objection, "heterogeneous private costs leave free entry optimal *in the IPV branch*" | `LITERATURE.tex` l.513–515, l.548–551; `lit/TRACEABILITY.md` "Heterogeneity objection pre-empted" | Proposition 3, p.320; Lemma A1 and proof of Proposition 3, p.329 | `root_unique`, `lemmaA1`, `prop3`, `prop3_needs_monotone_U` |
| MW-G | "their equilibrium threshold is a single number $t$, against which every buyer compares her own cost, because under private information all buyers face the same expected utility" | `PROOFS.tex` l.557–560 | p.318 (symmetric equilibria, common entry probability), p.320 (payoff under a common threshold) | `symmetric_common_utility`, `symmetric_equilibrium_flat_cutoff`, `private_info_without_symmetry` |
| MW-H | "A rank-dependent rule collapses to a flat one only if $\Delta$ is constant, which Corollary~\ref{cor:monotone} forbids." | `PROOFS.tex` l.560–563 | Our own claim. The source side is MW-G | `collapse_iff_const`, `strict_no_collapse`, `Delta3_strict`, `Delta3_no_collapse`, `Delta3_no_collapse_general`, `antitone_constant_collapses`, `realised_decisions_flat`, `Delta3_realised_flat_fit` |
| MW-I | "Proposition~4: inframarginal entrants keep information rents, so the seller no longer captures the whole surplus"; "Their Proposition~4 carries over directly" | `LITERATURE.tex` l.515–516, l.552–553 | eq. (6), pp.320–321; Proposition 4, p.321 | `inframarginal_rent` (the pointwise rent, not the integral) |
| MW-J | Propositions 5–9 and Section 6, from "Propositions~5--8: the revenue-maximising screening value is positive but below the fixed-$n$ level" to "revenue may rise \emph{or} fall in $N$" | `LITERATURE.tex` l.516–521 | Propositions 5–9, pp.321–326; Section 6, pp.325–327 | not formalised |
| MW-K | "There are no comparative statics in the distribution $H$ anywhere in the paper"; "Their $H$ is a primitive and they never vary it." | `LITERATURE.tex` l.521–522, l.538–539 | p.325 and footnote 13, p.326, p.333 | not formalised |

Three further mentions carry no mathematical content and were confirmed
against the source without a check. `LITERATURE.tex` l.565–567 says
Samuelson (1985) is the interim-entry-cost variant MW position against, which
matches p.316. `LITERATURE.tex` l.966–967 lists MW's cost space as one of three
threshold variants, which matches pp.319–320. `LITERATURE.tex` l.1009–1011
speaks of "a \citet{MorenoWooders2011} threshold on the entry structure",
which matches the same pages.

## What is checked, and how

- **MW-A, MW-E, MW-G** are formalised in Lean as the paper states them. The
  equilibrium definition of p.320 is transcribed twice, once literally
  (`EqLit`) and once with the tie at the threshold excluded (`EqTie`), because
  the two readings differ at the corner (`NOTES.md`, finding F4).
- **MW-H** is our claim built on MW-G. Lean proves it at the level of rules,
  shows it fails at the level of a realised profile, and shows that the
  strictness clause of Corollary `cor:monotone` is what blocks the collapse.
- **MW-F** is formalised as Lemma A1 plus the proof of Proposition 3, with the
  analytic steps as named hypotheses and a control that drops the
  monotonicity of `U` in `p`.
- **MW-B** and **MW-C** are formalised only as non-degeneracy. The binomial law
  needs measure theory, which core Lean lacks; the variance arithmetic is
  SymPy check MW-1.
- **MW-I** is formalised as the pointwise rent `t* − z` from eq. (3). The
  integral in eq. (6) is not formalised.
- **MW-D** has no check of its own. Its content is carried by MW-G and MW-H.
- **MW-J** and **MW-K** are read against the source and reported in
  `NOTES.md`. MW-K is contradicted by the source as worded (finding F7).
