# Notes — Ryvkin & Drugov (2020)

Run: `python3 verify_rd.py` gives **13 checks, 0 failures** (pass 2, 2026-10-06).
Pass 1 had 7 checks. Pass 2 adds RD-8 and the five RD-L checks on
`RyvkinDrugov.lean`.

## Pass 3 (9 Oct 2026). The C7 comparison, for pass 7 of the manuscript plan

The question C7 left open: is the rise-then-fall result for the first
entrant's gain (M16 to M21) an instance of RD's unimodality result, or a new
statement? Answer: M17 is an instance; what is left is narrower.

**The correspondence.** M1 writes the gain as `Δ(0) = V·E[φ(M)]`, with `φ =
G − F` and `M` the best rival's score, whose CDF is `H(z|Q) = C(z)·G(z)^{Q−1}`.
RD's marginal benefit is `b_k = E[f(X_{(k−1:k−1)})]`, the noise density at
the best rival's shock (eqs. (3), (9)). Their Karlin step (p.1597, RD-F)
applies to any `γ(θ) = ∫ u dH(·|θ)`: with `u = φ`, `θ = Q` and this `H`,

- `−H_θ = C·G^{Q−1}·(1−G)` is log supermodular in `(z, Q)`, since `C` does
  not depend on `Q` (RD-5; Lean `weight_tp2` on a grid);
- `u′ = g − f` crosses `+−` exactly when `φ` is single-peaked, which is
  M17's hypothesis (pass 2, finding F1; Lean `pmu_orientation`);
- so `D(Q)` crosses `+−` and `Δ_Q(0)` has no interior minimum, which is
  M17. M17's ratio bound `D(Q2) ≤ G(x0)^{Q2−Q1}·D(Q1)` is the quantitative
  form of the same step (Lean `karlin_ratio` for sums).

So the no-interior-minimum result is RD's Karlin step with the score gap in
place of the noise density and the best rival's law, including the
incumbent, in place of `F^{k−1}`. It is not new as a result.

**What is left, and how far it is RD's.**

1. M21 (for some base laws the gain must eventually fall) makes precise, for
   the entry gain, RD's remark that for many players "the comparative
   statics are determined by the shape of the upper tail" (RD-J, p.1601).
   The precise tail condition is ours; the idea is theirs.
2. M18 (with a uniform base score the gain never falls in `Q`) is the
   monotone case of the same step, with a twist that RD's tournament cannot
   have: the kernel `1 − G` vanishes above the top of the outsiders' support,
   and a uniform base score puts the peak of `φ` exactly there (`x0 = μ`).
   In RD all players draw from one law, so the kernel is positive on the
   whole support.
3. M19 (primitive conditions on `r` and `s` under which `φ` is
   single-peaked) is about the score technology, which RD do not have.

Items 2 and 3 are what C7 may still claim, as statements about entry, and
only if no contest with a binary investment states them. The abstract-level
searches of 9 Oct 2026 found none; that is not a proof of absence, and C7
stays pending on it.

**C12, the weight on talent.** RD's footnotes 23 and 29 (RD-K, RD-L, RD-M)
point to Drugov and Ryvkin (2020), where effort falls as noise becomes more
dispersed in the dispersive order, for arbitrary prize schedules, and to
Morgan, Tumlinson and Vardy, where noise intensity changes who drops out.
Raising `μ` scales the base score up relative to the bought component,
which is a dispersive change of the common component. M28 is therefore
likely the entry-margin analogue of Drugov and Ryvkin's result, and the
contrast between a universal sign in noise dispersion and none in the number
of players is already theirs across the two papers. Both papers are unread;
C12 is narrowed accordingly.

## Pass 2 (2026-10-06). Lean formalisation and a full re-read of the PDF

### Source read

The PDF read is `shape_of_luck.pdf` on Google Drive (file id
`1VCHeXdfuGX5TV3Ri1jKcda0qZHB09gc6`). The title page reads "The shape of luck
and competition in winner-take-all tournaments", Dmitry Ryvkin and Mikhail
Drugov, *Theoretical Economics* 15 (2020), 1587–1626, doi 10.3982/TE3824. The
published version was read in full, all 40 pages from p.1587 to p.1626,
including Appendix A (pp.1609–1611) and Appendix B (pp.1611–1621). Every page
number below is a journal page number. The Drive file `rewards_tails.pdf` was
not used, because Drugov and Ryvkin's "Tournament rewards and heavy tails" is
a different paper.

### Run and audit

`lean RyvkinDrugov.lean` (Lean 4.34.1 from `~/.elan/bin`, core only, no
Mathlib) returns 0 with no messages. The axiom audit in `verify_rd.py` covers
34 of 34 declared theorems. 15 depend on no axiom, 19 depend on `propext`
and/or `Quot.sound` only, and none depends on `Classical.choice`, `sorryAx`
or a user axiom. The source contains no `sorry`, no `axiom`, no
`native_decide` and no comment, and the suite checks all four.

### Encoding

- A CDF value `z = F(x)` is represented on a grid as `z = a/N`, with `a` and
  `N` natural numbers. RD's kernel `z^{k−1}(1−z)` becomes
  `K N a k = a^(k−1)·(N−a)`, which is the kernel multiplied by `N^k`. Both
  sides of the cross-product inequality carry the factor `N^{k1+k2}`, so the
  scaled inequality is equivalent to the unscaled one on the grid.
- `k ≥ 1` is a hypothesis wherever `k − 1` appears, so Lean's truncated
  subtraction at `k = 0` is never used.
- Log supermodularity is stated as TP2 in RD's own determinant form (p.1610,
  the case `l = 2`; the case `l = 1` is nonnegativity, automatic over `Nat`).
- Single crossing `+−` is RD's definition from p.1615, transcribed as
  `SCpm`. `SCmp` is the mirror definition for `−+`.
- Integrals are replaced by finite sums `sumTo`. The Karlin step is therefore
  proved for sums and not for integrals. The continuous statement is Karlin
  (1968) as RD cite it, and it is not formalised here.
- Rationals are compared by cross-multiplying natural numbers with positive
  denominators (`fracLt`).

### Theorem table

| Lean theorem | Claim | RD locator | Verbatim RD text | What the theorem states |
|---|---|---|---|---|
| `negH_theta_eq_kernel` | RD-D | p.1597, paragraph before Cor 1; p.1596 for the convention | "For tournaments with deterministic size k, θ = k and the role of H(z; θ) is played by F(x)^{k−1} (cf. (9)). It is easy to see that −Hθ = F(x)^{k−1} − F(x)^k is log supermodular" (p.1597); "Subscript θ is used to denote both the derivative and the first difference." (p.1596) | Over `Int`, for `a ≤ N` and `k ≥ 1`, `a^{k−1}·N − a^k = K N a k`. On the grid, `F^{k−1} − F^k = F^{k−1}(1−F)`. |
| `kernel_cross` | RD-D | p.1610, App. A.2 | "For deterministic tournament size, Proposition 1 relies on the fact that f′(x) is single-crossing and z^{k−1}(1 − z) is log supermodular in (z, k). Log supermodularity is also known as total positivity of order 2 (TP2)" | For `a1 ≤ a2` and `1 ≤ k1 ≤ k2`, `K(a1,k2)·K(a2,k1) ≤ K(a1,k1)·K(a2,k2)`. Symbolic in `N`, `a`, `k`, with no grid restriction. |
| `kernel_tp2` | RD-D | p.1610, definition of TPr | "Function v : S1 × S2 → R, with S1, S2 ⊆ R, is TPr if for all l = 1, ..., r and all sequences x1 < ··· < xl, y1 < ··· < yl (xi ∈ S1, yj ∈ S2), det [...] ≥ 0" | `K N` is TP2 on `S1 = {a ≤ N}`, `S2 = {k ≥ 1}`. |
| `kernel_cross_strict` | RD-D | as above | as above | Strict inequality for `0 < a1 < a2 < N` and `1 ≤ k1 < k2`, so the reverse (log-submodular) inequality is false at every interior pair. |
| `peak_k2` … `peak_k6` | RD-D | not in RD (finding F3) | none | On the grid `N = 12k`, `a ↦ K N a k` has its unique maximum at `a = 12(k−1)`, that is at `z = (k−1)/k`, for `k = 2, …, 6`. Every grid point is checked by `decide`. |
| `peak_not_at_half_k3` | RD-D, control | none | none | For `k = 3`, `z = 1/2` is not the maximiser. |
| `weight_tp2` | RD-E | p.1615, proof of Prop 1 | "The first inequality follows because ρ̃(z; θ, θ′) is increasing in z, due to the log-supermodularity condition." | For `G` nondecreasing and any `F ≥ 0` indexed by grid point only, `(i, Q) ↦ F_i·K(N, G_i, Q)` is TP2 in `(i, Q)`. This is P-MU's `W` on a grid. |
| `weight_tp2_fails_without_monotone_G` | RD-E, control | none | none | With `G` decreasing (`G_0 = 2`, `G_1 = 1`, `N = 4`) the weight is not TP2. Monotone `G` is a hypothesis that the inheritance needs. |
| `scpm_neg_iff_scmp` | RD-F | p.1615 | "it is sufficient to show that B′p(θ) is single-crossing +−; that is, if B′p(θ) < 0 for some θ, then B′p(θ′) ≤ 0 for all θ′ > θ." | `u` crosses `−+` exactly when `−u` crosses `+−`. |
| `phiPrime_scpm`, `phiPrime_not_scmp` | RD-F | pass-1 example `F = x²`, `G = x` | none | `φ′ = 1 − 2z` (times 4, at `z = i/4`) crosses `+−` and does not cross `−+`. |
| `fMinusG_scmp`, `fMinusG_not_scpm` | RD-F | as above | none | `f − g = 2z − 1` crosses `−+` and does not cross `+−`. |
| `docIntegrand_scmp`, `docIntegrand_not_scpm`, `neg_docIntegrand_scpm` | RD-F | `PROOFS.tex` l.1569 | none | `F(f−g) = z²(2z−1)` crosses `−+`, as `PROOFS.tex` says, and its negative `F·φ′` crosses `+−`. |
| `karlin_ratio` | RD-F, RD-C | p.1615, proof of Prop 1 | "Suppose B′p(θ) < 0 and consider some θ′ > θ. Function f′(x) is single-crossing +−. Let x̂ ∈ int(X) denote a mode of f(x) such that f′(x) ≥ (≤)0 for x ≤ (≥)x̂." | Sum analogue of RD's splitting argument. If `u ≥ 0` up to `c` and `u ≤ 0` after `c`, and the kernel satisfies the TP2 inequalities at the pairs `(x, c)`, then `K(c,y1)·ψ(y2) ≤ K(c,y2)·ψ(y1)` for `ψ(y) = Σ_x K(x,y)·u(x)`. |
| `karlin_step` | RD-F | p.1597 | "Following Karlin (1968), for u′(z) single crossing +− and −Hθ(z; θ) log supermodular, γθ(θ) is also single crossing +− and, hence, γ(θ) is unimodal." | `ψ(y1) < 0`, `K(c,y1) > 0` and `K(c,y2) ≥ 0` give `ψ(y2) ≤ 0`. |
| `karlin_scpm` | RD-F | as above | as above | A kernel that is TP2 and positive at the crossing point `c`, with `u` crossing `+−` at `c`, makes `ψ` single crossing `+−` in `y ≥ 1`. |
| `karlin_fails_without_tp2` | RD-F, control | none | none | A 2×2 kernel with values `1, 2, 2, 1` violates TP2. With `u = (1, −1)` crossing `+−`, the sum is `ψ = (−1, 1)`, which crosses `−+`. Dropping log supermodularity breaks the step. |
| `sumTo_neg`, `Dpmu_eq_psi` | RD-F | `PROOFS.tex` l.1545 | none | `D(Q) = −Σ_i K(N,G_i,Q)·w_i` equals `ψ` with `u = −w`. This is the bookkeeping of the leading minus sign. |
| `pmu_orientation` | RD-F (finding F1) | `PROOFS.tex` l.1545 and l.1569; RD p.1597 | none | If `w = F(f−g)` crosses `−+` about `c`, which is the premise `PROOFS.tex` states, with `0 < G_c < N` and `G` nondecreasing, then `D(Q)` is single crossing `+−` in `Q ≥ 1`. |
| `minus_sum_scpm` | RD-F | none | none | Instance with `n = 2`, `N = 4`, `G = (1, 3)`, `w = (−1, 2)`. `D(1) = 1` and `D(2) = −3`. |
| `plus_sum_not_scpm` | RD-F, control (F1) | none | none | The same data with the minus sign dropped gives sums `(−1, 3)`, which do not cross `+−`. The `−+` reading holds only for the sum without its minus sign. |
| `individual_reversal` | RD-G | p.1589; eq. (8) p.1596; p.1598 | "There is not a single prediction, either for individual or aggregate effort, that cannot be reversed for at least some distribution of noise." (p.1589) | `b_3 < b_2` for Gumbel noise with `r = 1`, and `b_2 < b_3` for the generalised logistic with `a = 1/5`. |
| `gumbel_decreasing` | RD-G | eq. (8) p.1596; p.1601 | "Equation (3) then produces b̂k = r(k − 1)/k²" (p.1596); "individual effort e∗k = r(k−1)/k² is decreasing" (p.1601) | `b_{k+1} < b_k` for `k = 2, …, 40`. |
| `logistic_max_at_khat3` | RD-G | p.1598 | "Then bk = a(k − 1)/[k(ak + 1)]. Since bk+1 − bk ∝ 1 + a − ak(k − 1) is decreasing in k, bk (and, hence, e∗k) is either monotonically decreasing or interior unimodal. In particular, bk reaches its maximum at k̂ if a = 1/(k̂² − k̂ − 1)." | With `a = 1/5`, so `k̂ = 3`, `b_k < b_3` for every `k ∈ [2, 40]` other than 3 and 4, and `b_3 = b_4`. |
| `logistic_symmetric_prop2` | RD-H | p.1600; p.1598 | "For the deterministic case, Gerchak and He (2003) show that e∗2 = e∗3 when f(x) is symmetric. If f(x) is also unimodal, then e∗k is decreasing in k for k ≥ 3." (p.1600); "The standard logistic distribution is obtained for a = 1." (p.1598) | With `a = 1`, `b_2 = b_3` and `b_{k+1} < b_k` for `k = 3, …, 40`. Under quadratic cost `e∗k = bk` (p.1602), so this is the stated pattern. |
| `aggregate_reversal` | RD-G | p.1601; p.1603 | "aggregate effort E∗k = r(k − 1)/k is increasing in k for k ≥ 2" (p.1601); "This gives bk = 2/[k(k + 1)] and, for the quadratic cost function, aggregate effort E∗k = 2/(k + 1) is strictly decreasing in k." (p.1603) | For every `k ≥ 1` the Tullock aggregate rises from `k` to `k+1`, and for every `k` the F(2,2) aggregate falls. Symbolic, with no range bound. |

### Controls (rule 3)

The file carries eight controls. `kernel_cross_strict` makes the reverse
inequality false. `peak_not_at_half_k3` excludes `z = 1/2` for `k = 3`.
`weight_tp2_fails_without_monotone_G` drops monotone `G`.
`phiPrime_not_scmp`, `fMinusG_not_scpm` and `docIntegrand_not_scpm` separate
the two orientations on concrete functions. `karlin_fails_without_tp2` drops
log supermodularity. `plus_sum_not_scpm` drops the leading minus sign.

The suite adds four mutants, and Lean rejects each one. The mutants move the
`k = 3` peak to `a = 23`, set the logistic tie at `a = 1/6`, claim that the
correctly signed P-MU sum does not cross `+−`, and claim that Gumbel `b_k`
increases. On a scratch copy (not part of the suite, run 2026-10-06), a
planted `Classical.em` theorem made the axiom check fail, and a planted
`sorry` made the source, compile and axiom checks fail. The `sorry` count in
the audit line is computed from Lean's output, not hard-coded.

### Findings

**F1. The single-crossing orientation in P-MU matches RD's, contrary to
`PROOFS.tex` and `LITERATURE.tex`.** This finding concerns our documents, not
RD. `PROOFS.tex` l.1545 states
`Δ(0,Q+1) − Δ(0,Q) = −∫ G^{Q−1}(1−G) F (dF − dG)`. RD write the Karlin step
on p.1597 as "γθ(θ) = − ∫ u′(z)Hθ(z; θ)dz", which is `∫ u′·(−Hθ)` with the
nonnegative kernel `−Hθ`. Matching the P-MU difference to that form, with
kernel `G^{Q−1}(1−G)`, puts `F(g−f) = F·φ′` in the role of `u′`. The
documents put `F(f−g) = −F·φ′` in that role, which drops the leading minus
sign. Under the documents' own premise that `φ = G − F` is hump-shaped, the
function `F·φ′` crosses `+−`, the orientation RD require. The function
`F(f−g)` crosses `−+`, as `PROOFS.tex` says, but `F(f−g)` is not the function
the Karlin step uses.

- Lean. `pmu_orientation` proves, for finite sums, that the stated premise
  makes `D(Q)` single crossing `+−` in `Q`. `plus_sum_not_scpm` shows the
  `−+` pattern appears only when the minus sign is dropped.
- Exact witness (SymPy RD-8). For `F = x⁴` and `G = 1 − (1−x)⁵`, `φ ≥ 0`
  has one interior critical point at 0.4663. `D(1) = 1/9009`,
  `D(2) = 106/2909907` and `D(3) = −2791/178474296`, and the sign pattern over
  `Q = 1, …, 12` is `+−`. `Δ(0,Q)` rises up to `Q = 3` and falls after, an
  interior maximum. The power family `F = x²`, `G = x` of pass 1 has
  `D(Q) = −Q/[(Q+2)(Q+3)(Q+4)]` for every `Q`, so no orientation can be read
  from that pair.
- Numerical scan (scratch scripts, not part of the suite). Beta pairs with
  parameters drawn from U(0.3, 9), seed 7, 6000 draws, kept when `F ≤ G` and
  `φ` has one interior critical point, gave 1808 admissible pairs. The sign
  patterns of `D(Q)` over `Q = 1, …, 60` were `−` 922 times, `+−` 883 times,
  `+` 3 times and `−+` never. As a control, `F` was a two-component Beta
  mixture and only pairs whose `φ` has more than one interior critical point
  were kept (seed 11, 3000 draws, 14 admissible). The patterns were `+−` 7
  times, `+−+−` 3 times, `−` 3 times and `−+` once. Pattern `−+` therefore
  occurs once the hump premise fails, so the premise excludes `−+` and the
  design of the scan does not.
- Falsifier. A pair with `F ≤ G` and hump-shaped `φ` for which `D(Q)` goes
  from negative to positive in `Q` would refute F1. `pmu_orientation` rules
  such a pair out for finite sums, RD-8 would fail on one, and the scan found
  none.

Three consequences follow for the text. They are recorded here, and
`PROOFS.tex` and `LITERATURE.tex` are not edited. (i) The sentences "what
fails is the orientation of the single crossing" (`PROOFS.tex` l.1825) and
"the second hypothesis does not" (`LITERATURE.tex` l.1096–1097) do not hold
under the stated premise. (ii) "The plausible target of a reworked argument
is an interior minimum" (`LITERATURE.tex` l.1099–1101, `PROOFS.tex`
l.1826–1828) points the opposite way. With the documents' premise, the Karlin
template makes the difference single crossing `+−`, so `Δ(0,Q)` is either
monotone in `Q` or rises then falls. An interior minimum is excluded, and an
interior maximum is possible. (iii) Two things are not established here, and
the author must check both before any text changes. The first is whether `φ`
is hump-shaped for the model's induced `F` and `G`; `PROOFS.tex` does not
prove it, and the non-hump control shows `D(Q)` can then cross either way.
The second is the continuous variation-diminishing step, which Lean proves
for finite sums only. Under `lit/README.md` rule 1 this is a lead about our
documents and not a result.

**F2. `F` is counted twice in the P-MU sentence.** `PROOFS.tex` l.1567–1570
pairs the kernel `W`, which already contains the factor `F`, with the
integrand `F(f−g)`. Two consistent decompositions exist. One uses kernel
`G^{Q−1}(1−G)` with function `F(g−f)`, the other uses kernel `W` with
function `g−f`. The factor `F ≥ 0` changes no sign pattern, so the
conclusion is the same under either decomposition, but the sentence should
use only one of them.

**F3. RD never state the kernel's peak.** A search of the full PDF text finds
no statement that `F^{k−1}(1−F)` peaks at `(k−1)/k`, and no use of the words
"peak" or "hump" for the kernel. RD's only maximum statement concerns the
logistic `b_k` (p.1598). RD's Proposition 1 uses log supermodularity of the
kernel (p.1597, p.1615), and the location of the kernel's maximum plays no
role in its proof. The peak at `(k−1)/k` is our computation (SymPy RD-3, Lean
`peak_k2` to `peak_k6`). The sentence "the hump-shape is not an artefact of
our construction but the standard object of this literature"
(`LITERATURE.tex` l.1094–1095) therefore overreads RD. The kernel is RD's
object, but RD's argument does not use the kernel's hump.

**F4. `CLAIMS.md` RD-A was stale.** `LITERATURE.tex` l.1077–1083 now says
"density result" and "\emph{not} their hazard-rate result". The pass-1 fix
has been applied in `LITERATURE.tex`, and `CLAIMS.md` now records that.

**F5. Wrong locator in pass 1.** RD-7 in `verify_rd.py` and §4 of
`RECONSTRUCTION.md` placed the hazard-rate form of aggregate effort on
p.1593. Page 1593 holds the model setup and eq. (3). The hazard-rate form is
on p.1589 in the introduction and on pp.1601–1602, where eq. (11) appears and
p.1602 reads "aggregate effort is then E∗k = kbk = E(h(X(k−1:k)))". Both
locators are corrected.

**F6. Attribution of the `e∗2 = e∗3` result.** `LITERATURE.tex` l.685–686
attributes "e∗2 = e∗3 and decreasing thereafter" to Proposition 2. RD (p.1600)
attribute the deterministic statement to Gerchak and He (2003) and present
Proposition 2 as its generalisation to a stochastic number of players.
Proposition 2(i) needs only a symmetric `f` and `supp(K) = {2, 3}`.
Proposition 2(ii) needs `f` unimodal and symmetric and `p1(θ) = 0`. The
logistic check with `a = 1` is consistent with both attributions, so the
check cannot choose between them.

**F7. The Barlow–Proschan lineage is ours, not RD's.** `LITERATURE.tex`
l.982–983 places RD's hazard-rate machinery in "the same Barlow--Proschan
lineage" as Hoppe–Moldovanu–Sela. RD's reference list (pp.1621–1626) has no
Barlow–Proschan entry, and RD cite Shaked and Shanthikumar (2007) for the
orders they use. The lineage is our inference and should be worded as ours.

**F8. A condition is missing from the Corollary 4 paraphrase.**
`LITERATURE.tex` l.687 ("Corollary~4: monotone density gives monotone
effort, constant iff uniform") omits `p1(θ) = 0`, which Corollary 4(ii) and
4(iii) carry (p.1600).

**F9. A tie in RD's logistic example.** This concerns RD, not our documents.
At `a = 1/(k̂² − k̂ − 1)` the factor `1 + a − a·k̂(k̂ − 1)` is zero, so
`b_{k̂+1} = b_k̂` and the maximum is attained at `k̂` and at `k̂ + 1` together.
RD's sentence "bk reaches its maximum at k̂" (p.1598) is true and says
nothing about the tie. `logistic_max_at_khat3` shows `b_3 = b_4` at
`a = 1/5`.

Several attributions were checked and found consistent. The p.1589 sentence
cited at `LITERATURE.tex` l.700–702 is on p.1589. Footnote 29 (p.1610) cites
"Drugov and Ryvkin (2020), How noise affects effort in tournaments, JET 188",
which is `DrugovRyvkin2020a` in `refs.bib`. The paraphrases of Propositions
3, 5 and 7 match pp.1601, 1604 and 1609. The p.1597 quotation in RD-3 is
verbatim.

### Attributed claims not formalised in Lean

- RD-B and RD-I, that is `b_k = E[f(X_(k−1:k−1))]` (eqs. (3), (5), (9)) and
  the hazard-rate form (eq. (11)). These are integral identities and a
  first-order condition, and core Lean has no integral. SymPy RD-1 and RD-7
  cover the algebra.
- Proposition 1 and Corollary 1 in continuous form, which rest on
  integration by parts and Karlin's continuous variation-diminishing
  property. Only the finite-sum step is formalised.
- The logistic closed form `b_k = a(k−1)/[k(ak+1)]` and the difference factor
  `1 + a − ak(k−1)`. Lean takes the closed form as given and checks the
  stated pattern, while SymPy RD-2 derives the closed form.
- The peak at `(k−1)/k` for general `k`. Lean covers `k = 2, …, 6` on grids of
  `12k` points, and SymPy RD-3 solves the derivative symbolically.
- The P-MU identity itself (`PROOFS.tex` S10). The identity is ours and is
  checked in `checks/`, not here.
- The TP∞ property of `z^{k−1}(1−z)` (p.1611, citing Marshall et al.) and the
  multimodal extension in Appendix A.2.

### Files changed in pass 2

`RyvkinDrugov.lean` is new. `verify_rd.py` gains RD-8 and the RD-L block.
Its RD-6 label changed from "our integrand single-crosses −+, RD's needs +−"
to a neutral statement of the arithmetic, because the old label asserted the
role assignment that F1 finds wrong. Its RD-7 locator changed from p.1593 to
p.1589 and p.1602. `CLAIMS.md` gains RD-D to RD-I. `RECONSTRUCTION.md` §4
has its locator corrected and §7 points to F1.

---

## Pass 1 (retained as written; the RD-6 reading is superseded by F1)

**Scope.** Our reading is arithmetically consistent. The handover's
"computation, not a read" is done, with a partial answer. One label in the
Verdict is wrong. Nothing here reproves any RD result, and **nothing here is a
new result about `Δ(0,Q)`**.

---

## The reading is solid, and independently verified

`b_k = E[f(X_{(k−1:k−1)})]` is confirmed (RD-1): `(3)` and `(9)` agree, and
`F^{k−1}` is the CDF of the maximum of `k−1` draws, so `b_k` is the expected
noise density at the best rival's shock. Their interpretation: a marginal
effort increase is pivotal exactly at a tie.

**RD-2 is the strongest check in this directory.** Their worked example — type
I generalized logistic, `F(x) = 1/(1+e^{−x})^a` — was re-derived from the
primitives by changing variable to `u = F`, giving `f = a·u(1−u^{1/a})` and

> `b_k = a(k−1)/[k(ak+1)]`

matching the paper exactly, with `b_{k+1} − b_k` proportional to
`1 + a − ak(k−1)` with **positive** constant of proportionality, also as
stated. Reproducing a non-trivial closed form from scratch is much better
evidence that `(3)` was read correctly than any restatement.

---

## The computation the handover asked for

**Result: the correspondence is exact, and one of the two hypotheses
transfers.**

Our P-MU weight is `W = G^{Q−1}(1−G)·F`. S11 established it is hump-shaped in
`G` with an interior maximum at `G = (Q−1)/Q`. RD's log-supermodular kernel is

> `−H_θ = F^{k−1} − F^k = F^{k−1}(1−F)`,  peaking at `F = (k−1)/k`

Under `G ↔ F`, `Q ↔ k` these are **the same function** (RD-4). Our extra factor
`F` is the incumbent's CDF and carries no `Q`.

So S11 — which we derived independently, to show FOSD alone cannot sign the
comparison — turns out to be Ryvkin and Drugov's kernel. That is worth saying
in `PROOFS.tex` on its own: the hump-shape is not an artefact of our
construction but the standard object in this literature.

**Hypothesis 1 transfers (RD-5).** Log supermodularity requires
`∂² log[z^{n−1}(1−z)]/∂z∂n ≥ 0`; the cross-partial is exactly `1/z > 0` on
`(0,1)`. The factor `F` does not involve `Q`, so it adds nothing to the
cross-partial and the property is inherited by `W`.

*[Superseded by pass-2 finding F1. The paragraph below drops the leading
minus sign of the P-MU identity. With the sign kept, the function in the role
of `u′` is `F·φ′`, which crosses `+−`, RD's orientation.]*

**Hypothesis 2 does not (RD-6).** Karlin's argument also needs the integrand in
the role of `u'` to be single crossing **`+−`**. Ours is
`F·(f−g) = −F·φ'`, where `φ = G−F ≥ 0` vanishes at both endpoints by P2 and
the regularity in §Primitives. `φ` is hump-shaped, so `φ'` crosses `+−` and
`−φ'` crosses **`−+`** — the reverse orientation. Verified concretely on
`F = x²`, `G = x`: `φ' = 1−2x` crosses `+−`, while `f−g = 2x−1` crosses `−+`.

**So the answer to "do the conditions coincide?" is: one does, exactly; the
other is reversed.** That is more informative than a yes or a no, and it
locates the gap precisely.

**What is NOT established.** `Δ(0,Q)` is **not** shown to be unimodal in `Q`.
Karlin's conclusion does not transfer as-is. Two routes remain, neither tried:

1. Work out what a `−+` crossing yields. The machinery is symmetric enough
   that it should give an *anti*-unimodal (single-troughed) conclusion — i.e.
   `Δ(0,Q)` with an interior *minimum* in `Q`. That would be a genuine result,
   and it is consistent with R2's refutation (the sign of the `Q` effect
   reverses at `μ*`), but **it has not been derived**.
2. Reformulate so the `+−` orientation is restored.

Per `lit/README.md` rule 1 and the handover's standing instruction: this is a
lead, not a result, and must not be claimed until derived.

---

## ERROR: the Verdict mislabels which RD result P-MU parallels

`LITERATURE.tex` §Verdict item 3 calls P-MU "the discrete analogue of the
\citet{RyvkinDrugov2020} **hazard-rate** result".

RD have two distinct results with two distinct order statistics:

| object | quantity | order statistic |
|---|---|---|
| **individual** effort | `b_k = E[f(X_{(k−1:k−1)})]` — the **density** | max of `k−1` draws |
| **aggregate** effort (quadratic cost) | `E(h(X_{(k−1:k)}))` — the **hazard rate** | second-highest of `k` draws |

RD-7 confirms these differ: the pdfs stand in ratio `k(1−F)`, not 1.

`Δ(0)` is an **individual** gain — the marginal challenger's incentive — so the
right comparator is `b_k`, the *density* result. The survey itself says this
correctly elsewhere ("their `b_k = E[f(X_(k−1:k−1))]` against our `Δ(0)`"), so
the Verdict's "hazard-rate result" is a slip in the compressed summary,
inconsistent with the survey's own §sec-level text.

**Fix:** in the Verdict, replace "the RyvkinDrugov2020 hazard-rate result" with
"the RyvkinDrugov2020 density result `b_k = E[f(X_{(k−1:k−1)})]`", and keep the
hazard rate for their aggregate-effort statement if it is mentioned at all.

---

## Results

| ID | Result |
|---|---|
| RD-1 | CONSISTENT. `(3)` and `(9)` agree identically; `b_k = E[f(X_{(k−1:k−1)})]`. |
| RD-2 | CONSISTENT, independently derived. `b_k = a(k−1)/[k(ak+1)]` and `b_{k+1}−b_k ∝ 1+a−ak(k−1)`, both matching the paper. |
| RD-3 | CONSISTENT. `−H_θ = z^{k−1}(1−z)`, interior peak at `(k−1)/k`. |
| RD-4 | CONSISTENT. P-MU's weight contains exactly this kernel; S11's peak `(Q−1)/Q` is RD's `(k−1)/k`. |
| RD-5 | CONSISTENT. Cross-partial of the log kernel is `1/z > 0`; log-supermodularity is inherited by `W`. |
| RD-6 | CONSISTENT, and it is the blocker. Our integrand crosses `−+` where RD need `+−`. *(Pass 2: the arithmetic stands, but the role assignment is withdrawn. See F1 and RD-8.)* |
| RD-7 | CONSISTENT. The two order statistics differ by a factor `k(1−F)`; the survey picks the right comparator in its body text. |

## Housekeeping

RD-7 was initially written with a hardcoded `True` — the same vacuity bug as
`verify_p89.py:210` and the HK-5 episode. Replaced with an assertion that the
two order-statistic densities differ by exactly `k(1−F)`. A sweep across all
literature suites and `checks/` now finds **no remaining hardcoded-`True`
checks**.

## Not attempted

- Proposition 1's proof and the Karlin apparatus; Lemmas 1–2; the PSD family.
- Propositions 2–6, Corollaries 3–6, Proposition 7 / Appendix A.1 existence.
- §2.4's multiplicative-shock reduction; Appendix A.2's `TP_r` extension to
  multimodal densities — **this last one may matter**, since it relaxes
  unimodality, and our `φ` is hump-shaped by construction.

Anything leaning on these is **[A]-grade** despite the [F] tag.
