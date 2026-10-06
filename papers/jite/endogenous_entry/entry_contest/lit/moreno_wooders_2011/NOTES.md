# Notes — Moreno & Wooders (2011), pass 2

Run `python3 verify_mw.py` from the paper root. Result on 2026-10-06 was
**12 checks, 0 failures**, made of 5 SymPy checks (MW-1 to MW-5, pass 1) and
7 Lean checks (compile, hygiene, sorry, axiom audit, axiom whitelist, claim
mapping, controls).

**Scope.** Our reading is consistent with the source, and the cutoff
structure, the rank-dependent collapse and the logical structure of
Proposition 3 are machine-checked in Lean with the analytic content as
explicit hypotheses. Nothing here reproves a result of the paper. Pass 2 found
ten discrepancies (F1 to F10 below). None of them overturns the separation
`PROOFS.tex` draws between P5 and Moreno–Wooders. F1, F2, F6 and F7 change
what the documents may say.

## Source identity

Drive file `RANDMorenoWooders2011.pdf`, id
`1VtK3cH_x1tuX598QIKAxebSo97dvDKSH`. The title page reads "RAND Journal of
Economics Vol. 42, No. 2, Summer 2011 pp. 313–336 Auctions with heterogeneous
entry costs Diego Moreno and John Wooders". The text layer of all 24 pages was
read in full, from the abstract through the references. The article is the
published version and says it "is based on Moreno and Wooders (2006)", the
Carlos III working paper, which was not read. The text layer drops primes and
bars, so `c` below means the lower bound $\underline c$ and `c̄` the upper
bound wherever the source context fixes which one is meant.

## The Lean file

`MorenoWooders.lean` is core Lean 4 with no Mathlib and no comments. It
compiles under the `lean` on PATH, which is version 4.34.1 (the brief named
4.33.1, and 4.34.1 is what is installed). The file declares 26 theorems with 0
`sorry`. All 26 are axiom-audited by `#print axioms`. Five depend on no axiom
and 21 on `propext` and `Quot.sound` only. None depends on `Classical.choice`;
a first draft did, through `omega` case-splitting on implication hypotheses,
and the proofs were restructured to remove it.

Costs, thresholds, utilities and surplus are `Int`. `G t` stands for
`U(v, H(t))` at a fixed screening value, so `G t = t + phi` is the paper's
equation (3). Facts that the paper derives by real analysis enter as named
hypotheses rather than being proved. The hypotheses cover the monotonicity of `U` in `p`
and in `v`, the derivative identity of Lemma A1, and the step from the sign
of a derivative to monotonicity on an interval. The Lean therefore checks the
order-theoretic and logical structure of each argument, and leaves the
analysis to the paper.

### Theorem, claim, locator, quotation

| Lean theorem | Claim | Locator | Verbatim source text | What is proved |
|---|---|---|---|---|
| `mem_entrants` | MW-A | §4, pp.319–320 | "an entry strategy for a buyer can be described by a threshold t ∈ [c, c̄] indicating the maximum entry cost for which the buyer enters the auction; that is, a buyer enters when her entry cost is less than t, and does not enter if it is greater than t—whether a buyer enters when her entry cost is exactly t is inconsequential." | Under a common threshold `t`, buyer `i` is in the entrant list iff `i < N` and `z i < t`. Each buyer's entry depends on her own cost only. |
| `eq3_gives_eqLit` | MW-A, MW-E | proof of Proposition 2, p.328 | "Hence, U(v, H(t∗(v, φ))) > z + φ implies t∗(v, φ) > z, and U(v, H(t∗(v, φ))) < z + φ implies t∗(v, φ) < z, and therefore t∗(v, φ) is a symmetric entry equilibrium." | A threshold in `[lo, hi]` that solves (3) satisfies the p.320 definition. |
| `eqLit_forces_eq3` | MW-A, MW-E | definition, p.320 | "A symmetric entry equilibrium is a threshold t ∈ [c, c̄] such that for all z ∈ [c, c̄]: U(v, H(t)) > z + φ implies t > z, and U(v, H(t)) < z + φ implies t < z" | Read literally, with `z = t` allowed, the definition forces (3) at every equilibrium. |
| `corner_not_eqLit` | MW-E | proof of Proposition 2, p.328 | "Assume that u(v, 1) ≤ c + φ. [...] Therefore, in equilibrium, no buyer enters, that is, t∗(v, φ) = c is the unique symmetric entry equilibrium." | If `G t < lo + phi` for all `t`, no threshold satisfies the literal definition. |
| `corner_eqTie` | MW-E | p.320 and p.328 | "whether a buyer enters when her entry cost is exactly t is inconsequential" | With the tie `z = t` excluded, `t = lo` is an equilibrium whenever `G lo < lo + phi`. |
| `corner_eq3_fails` | MW-E, control | Proposition 2, p.320 | "When the equilibrium is interior, t∗(v, φ) solves U(v, H(t)) = t + φ, (3)" | A corner equilibrium exists at which (3) fails, so "when interior" is load-bearing. |
| `tstar_strict_anti_phi` | MW-E | Proposition 2, p.320; p.318 | "and is decreasing in both v and φ"; "It is easy to see that U(v, p) is decreasing in p" | If `G` is antitone and `t1`, `t2` solve (3) at `phi1 < phi2`, then `t2 < t1`. |
| `tstar_strict_anti_v` | MW-E | Proposition 2, p.320; p.318 | "because u(v, n) is decreasing in v, then U(v, p) is also decreasing in v" | With `U` antitone in `t` and strictly decreasing in `v`, interior thresholds fall strictly in `v`. |
| `symmetric_common_utility` | MW-G | p.318 | "We study the symmetric equilibria of the entry game. In this game, the payoff to a buyer who enters, when every other buyer enters with the same probability p, is U(v, p) minus her entry costs." | If each buyer's gross utility depends only on rivals' common threshold, a symmetric profile gives every buyer the same utility. |
| `symmetric_equilibrium_flat_cutoff` | MW-G | p.320 | "When all buyers enter according to a common threshold t, then the payoff to a buyer with entry cost z who enters is U(v, H(t)) − z − φ." | In a symmetric profile solving (3), every buyer's comparison `z + φ` versus her utility is exactly `z` versus the single number `t`. |
| `private_info_without_symmetry` | MW-G, control | p.318 | "We study the symmetric equilibria of the entry game." | A two-buyer utility satisfying the private-information hypothesis, with an asymmetric profile under which the buyers' utilities are 8 and 9. |
| `count_not_pinned` | MW-B | p.320 | "If all buyers employ the same threshold t, then the number of bidders follows a binomial distribution B(N, H(t))." | Same threshold, different cost draws, counts 2 and 0. |
| `entrant_set_not_pinned` | MW-C | p.314 | "each buyer's entry cost is an independent draw from a common distribution, and is privately observed prior to entry" | Same threshold and same count 1, entering sets `[0]` and `[1]`. |
| `collapse_iff_const` | MW-H | `PROOFS.tex` l.560–563 | (our claim) "A rank-dependent rule collapses to a flat one only if $\Delta$ is constant" | For any reflexive antisymmetric order, a rank-dependent rule coincides with a flat rule for every cost value iff the schedule is constant on the ranks. |
| `strict_no_collapse` | MW-H | as above | as above | A schedule strictly decreasing across two or more ranks never collapses. |
| `Delta3_strict` | MW-H | `PROOFS.tex` l.544 | (our example) "$\Delta=(12,10,1)$ and $\kappa=(2,5,8)$" | The example schedule is strictly decreasing. |
| `Delta3_no_collapse` | MW-H | as above | as above | Direct counterexample. Cost 11 passes rank 0 (11 ≤ 12) and fails rank 1 (11 > 10), so no single `t` works. |
| `Delta3_no_collapse_general` | MW-H | as above | as above | The same conclusion obtained from `strict_no_collapse`. |
| `antitone_constant_collapses` | MW-H, control | `PROOFS.tex` Corollary `cor:monotone` | (our corollary) "$\Delta$ is non-increasing in $m$, strictly wherever $\varphi\not\equiv0$ on $\operatorname{supp}(dK)$." | A non-increasing constant schedule collapses, so non-increase alone does not forbid collapse. |
| `realised_decisions_flat` | MW-H, scope | as above | as above | If `κ` rises strictly and `Δ` weakly falls in rank, the realised rank-dependent decisions always coincide with some flat threshold. |
| `Delta3_realised_flat_fit` | MW-H, scope | `PROOFS.tex` l.544 | as above | On the example profile the flat threshold 6 reproduces every realised decision. |
| `root_unique` | MW-F | Lemma A1 proof, p.329 | "Because U is continuous and decreasing in p, there is a unique tW ∈ (c, c̄) such that U(0, H(t)) − t = 0." | Antitone `G` has at most one solution of `G t = t`. |
| `lemmaA1` | MW-F | Lemma A1, p.329 | "Lemma A1. W∗ = W(0, tW), where tW ∈ (c, c̄) uniquely solves U(0, H(t)) − t = 0." | Under the hypotheses below, `W 0 tW` is the maximum of `W` on `[0, vbar] × [lo, hi]`. |
| `prop3` | MW-F | Proposition 3, p.320; proof, p.329 | "Proposition 3. A screening value and an admission fee both equal to zero maximize social surplus, that is, W(0, t∗(0, 0)) = W∗." and "the equation U(0, H(t)) − t = 0 is identical to equation (3) for v = φ = 0; that is, tW = t∗(0, 0)." | If `t*` solves (3) at `v = φ = 0`, then `t* = tW` and `W 0 t*` is the maximum. |
| `prop3_needs_monotone_U` | MW-F, control | p.318 | "It is easy to see that U(v, p) is decreasing in p" | With every other hypothesis of `prop3` satisfied and `G` not antitone, a solution of (3) is not the maximiser. |
| `inframarginal_rent` | MW-I | p.320 | "If the entry equilibrium is interior, then U(v, H(t∗(v, φ))) − φ = t∗(v, φ)." | A buyer with `z < t*` has rent `t* − z > 0`, and the marginal buyer has rent 0. |

**The hypotheses of `prop3`, and the source text for each.**

- `hv` stands for "Because W(v, t) is decreasing in v" (p.329, proof of Lemma A1).
- `hG` stands for "Because U is continuous and decreasing in p" (p.329),
  combined with "we assume also that H is increasing" (p.319).
- `hinc` and `hdec` stand for the derivative identity
  "dW(0, t)/dt = Nh(t)(U(0, H(t)) − t)" together with "because h(t) > 0 on
  [c, c̄], then dW(0, t)/dt > 0 for t ∈ [c, tW) and dW(0, t)/dt < 0 for
  t ∈ (tW, c̄]" (p.329). `hinc` and `hdec` say that `W(0, ·)` rises across any interval on
  which `U(0, H(s)) − s` is positive and falls across any interval on which it
  is negative. The mean-value step is part of the hypothesis.
- `hW` stands for "tW ∈ (c, c̄) uniquely solves U(0, H(t)) − t = 0"
  (Lemma A1). Uniqueness is derived in `root_unique` rather than assumed.
- `h3` stands for Proposition 2's equation (3) at `v = φ = 0`.
- The conclusion `IsMaxOn W vbar lo hi (W 0 tstar)` is "W(0, t∗(0, 0)) = W∗"
  with `W∗` the maximum in eq. (5), p.320. Eq. (5) writes the screening range
  as `[0, ω]`, while every other range in the paper is `[0, v̄]`. We read
  `ω` as a typesetting slip for `v̄`.
- Lemma A1's "unique maximizer" is not formalised. The weak maximum is all
  that `W(0, t∗(0,0)) = W∗` needs.

### Controls (`lit/README.md` rule 3)

Each control is a theorem showing that a conclusion fails once a named
hypothesis is dropped.

- `corner_eq3_fails` drops "when the equilibrium is interior" from
  Proposition 2. The equation (3) then fails at a corner equilibrium.
- `private_info_without_symmetry` keeps the private-information hypothesis
  (utility depends only on rivals' thresholds) and drops the common
  threshold. Two buyers then face 8 and 9 rather than one common value.
- `antitone_constant_collapses` weakens "strictly decreasing" to
  "non-increasing". A constant schedule then collapses to a flat rule.
- `prop3_needs_monotone_U` drops "U decreasing in p" from Proposition 3. On
  `[0, 2]` with `G = (0, 0, 2)` and `W(0, ·) = (1, 0, 0)`, the threshold 2
  solves (3) and is not the maximiser.

The suite itself was mutation-tested in a scratch copy. Adding a theorem with
`sorry`, a theorem using `Classical.em`, an unmapped theorem, or a false
theorem each made `verify_mw.py` report failures.

## Findings

**F1. MW-G, the reason given for the common expected utility.** `PROOFS.tex`
l.557–560 says the threshold is a single number "because under private
information all buyers face the same expected utility". In the source the
expected utility is common because every buyer's rivals use the same
threshold. The p.320 text is "When all buyers enter according to a common
threshold t, then the payoff to a buyer with entry cost z who enters is
U(v, H(t)) − z − φ", and the equilibrium concept is symmetric by definition
("We study the symmetric equilibria of the entry game", p.318). Proposition 2's
uniqueness is uniqueness among symmetric equilibria. The paper does not say
whether its entry game has asymmetric equilibria. The paper reports only that Tan and
Yilankaya (2006), in Samuelson's different setting, "provide conditions under
which the entry equilibrium is unique (and symmetric), and under which there
are other (asymmetric) equilibria" (p.316). Private information supplies one
half of the common value, since a buyer's utility then depends on rivals'
strategies and not on their realised costs. The common threshold supplies the
other half. `private_info_without_symmetry` has the first half without the
second. The falsifier of the sentence as written is an asymmetric threshold
profile under private costs, and the paper does not exclude one as an
equilibrium. A wording that matches the source is "because in the symmetric
equilibrium they study every buyer faces the same expected utility
$U(v,H(t))$". The contrast with P5 survives this change, since it concerns
attribution only. The word "Bayesian" is ours and does not occur in the paper.
As a description of an equilibrium with privately known costs it is accurate.

**F2. MW-H holds for rules and fails for realised decisions.**
`collapse_iff_const` proves the `PROOFS.tex` sentence when "collapses" means
that the rule, as a map from rank and own cost to a decision, coincides with a
flat rule at every cost value. When "collapses" means that the decisions on a
realised profile coincide with some flat threshold, the sentence is false.
`realised_decisions_flat` shows such a threshold always exists when `κ` rises
strictly and `Δ` weakly falls, and `PROOFS.tex`'s own example
$\Delta=(12,10,1)$, $\kappa=(2,5,8)$ is fit by the threshold 6
(`Delta3_realised_flat_fit`). That realised-profile threshold depends on every
agent's cost. Moreno–Wooders' `t*` depends only on `(v, φ, H, N)` (Proposition
2, p.320). The sentence reads naturally as a statement about rules, and the
Lean statement fixes that reading. Separately, "which Corollary forbids"
rests on the strictness clause of the corollary, because
`antitone_constant_collapses` shows that non-increase alone permits a
constant, collapsing schedule. By `collapse_iff_const`, one strict step among
the first `Q` values of `Δ` is enough to block the collapse.

**F3. MW-E drops "when the equilibrium is interior".** `LITERATURE.tex`
l.510–512 says $t^*$ "solves $U(v, H(t)) = t + \phi$ and is decreasing in both
instruments" with no qualifier. Pass-1 CLAIMS.md MW-E did the same.
Proposition 2 says "When the equilibrium is interior, t∗(v, φ) solves [...]
(3) and is decreasing in both v and φ" (p.320). The qualifier binds at the
corner. The proof of Proposition 2 sets `t* = c` whenever `u(v, 1) ≤ c + φ`
(p.328), and with strict inequality (3) fails there (`corner_eq3_fails`). At
the same corner `t*` is locally constant in `φ` rather than decreasing.
Inserting "when interior" in `LITERATURE.tex` would match the source.

**F4. Source-internal, the tie in the equilibrium definition.** The p.320
definition quantifies over every `z ∈ [c, c̄]`, including `z = t`. Taking
`z = t` forces `U(v, H(t)) = t + φ` (`eqLit_forces_eq3`). Read literally, the
definition then has no equilibrium at all in the corner case `u(v, 1) < c + φ`
(`corner_not_eqLit`), although the proof of Proposition 2 says `t* = c` is the
unique one there. With the tie excluded, as "whether a buyer enters when her
entry cost is exactly t is inconsequential" (p.320) licenses, `t = c` is an
equilibrium (`corner_eqTie`). The tie convention affects no claim in our
documents. F4 is recorded so that the two Lean definitions, `EqLit` and
`EqTie`, can be traced to the source.

**F5. MW-F says "equals" where the source says "proportional".**
`LITERATURE.tex` l.513–515 and l.548–549 say the marginal type's private gain
"equals" its social contribution. The source says "the contribution to social
surplus of a marginal increase of the entry threshold is proportional to a
buyer's utility to entering" (p.315), and Lemma A1 gives
"dW(0, t)/dt = Nh(t)(U(0, H(t)) − t)" (p.329). Equality is the
homogeneous-cost statement of Proposition 1, "u(0, n) = s(0, n) − s(0, n − 1)"
(p.317). The two agree in sign, and the sign is all the welfare argument uses.
"Has the sign of" or "is proportional to" would match the source.

**F6. MW-F, Proposition 3 is a constrained optimum.** `W∗` is defined in
eq. (5) as a maximum over symmetric independent threshold rules, "W∗ is a
constrained maximum in the sense that buyers enter independently according to
a symmetric entry rule" (p.320). The paper's own example beats `W∗` with an
entry cap. Table 1, p.325, gives social surplus .13145 in scenario (iv)
against .10714 at `v = φ = 0`, and the text says "Social surplus exceeds the
constrained maximum social surplus (i.e., the social surplus in scenario (i)),
because buyers no longer enter independently". `LITERATURE.tex` l.513–515 and
`lit/TRACEABILITY.md` ("leave free entry optimal *in the IPV branch*") omit
"constrained". Pass-1 NOTES and RECONSTRUCTION.md kept it. The pre-emption of
the heterogeneity objection holds in this form. With heterogeneous private
costs and independent private values, the private and social marginal
incentives at the threshold have the same sign (Lemma A1, p.329), so free entry
is optimal among symmetric independent entry rules. Proposition 3 does not
make free entry optimal against an unconstrained planner, and scenario (iv) is the
paper's own falsifier for that stronger reading. Under complete information
our $k^*$ is not restricted to symmetric independent rules, so the welfare
section must name which benchmark it compares against.

**F7. MW-K is contradicted as worded.** `LITERATURE.tex` l.521–522 says
"There are no comparative statics in the distribution $H$ anywhere in the
paper", and l.538–539 says "Their $H$ is a primitive and they never vary it."
The paper compares distributions of entry costs in three places.

- p.326 says "when entry costs are heterogeneous, seller revenue is
  asymptotically invariant to changes in the distribution of entry costs that
  preserve the lower bound of its support."
- p.325 says "seller revenue and social surplus decrease from N = 1 to N = 2
  when entry costs are uniformly distributed on [.49, .5]", against an increase
  for the uniform distribution on [1/4, 1/2], and footnote 13 says "Which
  effect dominates depends on the distribution of entry costs."
- p.333, in the proof of Proposition 9, says "for each N, the constrained
  maximum social surplus is greater when entry costs are homogeneous than when
  they are heterogeneous."

None of the three is a spread of `H` at fixed `N`. The homogeneous-against-
heterogeneous comparison raises costs above the degenerate distribution at the
lower bound rather than spreading them. The point the survey needs, that P9's
mean-preserving-spread comparative static has no Moreno–Wooders counterpart,
survives if it is worded as "Moreno–Wooders sign no effect of a spread of $H$
at finite $N$". The falsifier of that reworded claim would be a statement in
the paper signing the effect of a mean-preserving or dispersive spread of `H`
on entry, surplus or revenue at fixed `N`. The full read found none.

**F8. Stale text about complete information.** `LITERATURE.tex` l.531–534
says the common-knowledge assumption "pins the identities of the entrants" and
that "This assumption is nowhere stated in \texttt{PROOFS.tex}".
`PROOFS.tex` l.533–540 now states the assumption, and l.542–551 says it "does
\emph{not} pin the identities of the entrants", with the counterexample
$\Delta=(12,10,1)$, $\kappa=(2,5,8)$. `LITERATURE.tex` l.1044–1047 ("uses but
does not state") and outstanding item 1 at l.1124–1126 are stale for the same
reason. Pass-1 CLAIMS.md MW-C quoted the superseded `PROOFS.tex` phrase "the
identities are not determined". `PROOFS.tex` l.555–556 now carries the
tightening pass 1 proposed, and CLAIMS.md now quotes the current text.
`lit/TRACEABILITY.md` row 9 still lists that tightening as pending.

**F9. Locator.** Pass-1 CLAIMS.md and RECONSTRUCTION.md §3 place the threshold
sentence on p.320. The sentence begins on p.319 ("In this setting, an entry
strategy for a buyer can be described by a threshold") and ends on p.320. The
binomial sentence and the definition of a symmetric entry equilibrium are on
p.320.

**F10. Two pass-1 SymPy checks cannot fail.** MW-3 tests
`kstar == len(entered)`, where `kstar` is defined as `len(entered)`, together
with a positive constant. MW-5 tests `V − V == 0`. Neither can fail for any
input, so under `lit/README.md` rule 3 neither verifies anything. MW-4's
content is now carried by the Lean theorems, which include a control. MW-5's
welfare-branch content is a Levin–Smith inference and belongs with LS-7 in the
Levin–Smith suite. The two checks are left unchanged for the author to decide.
MW-3 also compares an ex-ante variance over cost draws with a count
conditional on a known profile. Both counts are deterministic once the profile
is realised, so MW-3 is meaningful only because P5 treats the profile as a
known primitive (`PROOFS.tex` l.533–540). `PROOFS.tex` l.557 already moves the
separation to the mechanical difference ("The mechanical difference is sharper
than the informational one"), which is the version F2 checks.

## SymPy results (pass 1, unchanged)

| ID | Result |
|---|---|
| MW-1 | CONSISTENT. The count is `B(N, H(t))`, and at `N = 10, H = 1/3` its variance is `20/9 > 0`. |
| MW-2 | CONSISTENT. Totally differentiating (3) gives `dt*/dφ = 1/(U_H h − 1)` and `dt*/dv = −U_v/(U_H h − 1)`, the expressions on p.329. Both are negative when `U_H < 0`, `h > 0`, `U_v < 0`. |
| MW-3 | Passes, but cannot fail (F10). |
| MW-4 | Passes on a hard-coded list. Superseded by the MW-H Lean theorems. |
| MW-5 | Passes, but cannot fail (F10). |

## Not formalised, and why

- **The binomial law of MW-B.** Core Lean has no probability. The Lean proves
  non-degeneracy, and SymPy MW-1 does the variance arithmetic.
- **Proposition 1 and the §2 representation** of `U(v, p)` and `S(v, p)`.
  These are integrals and enter the Lean only through the hypotheses of
  `prop3`.
- **Proposition 2's existence, uniqueness and continuity** over the reals.
  These need the intermediate value theorem. The Lean covers the equilibrium
  definition, the interior equation, the corner and the comparative statics.
- **Lemma A1's derivative identity and strict maximiser.** The identity enters
  as `hinc` and `hdec`. Only the weak maximum is proved.
- **The integral in eq. (6) and the rest of Proposition 4.** Only the pointwise
  rent is formalised.
- **Propositions 5 to 11, §5's example and §6 (MW-J).** These are asymptotic or
  numerical results with no discrete core that our documents use. Their
  summary in `LITERATURE.tex` l.516–521 matches pp.321–327.
- **MW-K.** The source passages are asymptotic or numerical, and the finding
  is textual (F7).
- **MW-D.** This is our own claim. Its content is carried by MW-G and MW-H.
