# Notes — Fu & Lu (2010), pass 2 (Lean formalisation)

Run from the repository root `python3 lit/fu_lu_2010/verify_fulu.py`.
Result **10 checks, 0 failures**, of which 4 are Lean checks. `FuLu.lean`
compiles with `lean` on PATH (v4.34.1) and with
`lean +leanprover/lean4:v4.33.1`. 27 theorems, 27 audited, 0 `sorry`, 2
axiom-free and 25 using only `propext` and `Quot.sound`.

**Scope.** Our reading of the source is arithmetically consistent with it.
Nothing here reproves Lemma 1, Lemmas 4 and 5, or the existence half of
Theorem 1. Those enter `FuLu.lean` as explicit hypotheses, in the style of
`../fu_jiao_lu_2015/Accounting.lean`.

## Source confirmation

`fulu_contest_design.pdf` (Drive id `1ykxVKP-O0X6CluhYDcsHx6NSlPw32Oul`) is
the **MPRA working paper**, not the *Economic Inquiry* article. The cover
reads "Contest design and optimal endogenous entry / Fu, Qiang and Lu,
Jingfeng / November 2006 / [...] MPRA Paper No. 945, posted 28 Nov 2006 UTC".
The title page reads "Contest Design and Optimal Endogenous Entry". All 17
printed pages and the cover were read. The text layer drops Greek letters,
so every symbol quoted below was read from rendered page images. A Drive
search on 2026-10-06 found no copy of the published version.

## Corrections to pass 1

1. **The budget is `Γ₀`, not `Π₀`.** Pass 1 quoted eq. (7) as
   `E = Π₀ − N(V*,S*)·C` and marked the quotation verbatim. The source has
   `Γ₀` throughout (p.5 "a fixed budget of Γ₀"), and Theorem 1's value is
   `Ē = Γ₀ − 2C` with an overline on `E`. The suite now uses `Gamma0`.
2. **Old FL-4 could not fail.** It compared `Π₀ − N·C` with itself, which
   breaks README rule 3. FL-4 is now a Tullock-instance check that can fail,
   and the count bound is proved in Lean (`eq7_count_cap`).
3. **The FL-2 negation control was described but not run.** Pass 1 said
   "`N = 3` does not beat all other candidates" was checked. The pass-1
   script did not contain that check. It does now.
4. **"Exact rather than slack" was wrong.** Pass 1 said the bound
   `N·C ≤ Π₀` "is exact rather than slack". Eq. (7) gives `Γ₀ − N·C = E`, so
   the bound is slack by exactly the total effort. See finding F3.

## Lean theorems, claim IDs, locators and quotations

All page numbers are printed page numbers of MPRA 945. "Hyp." names a
hypothesis that is not proved in the file.

| Lean theorem | Claim | Locator | Verbatim source text |
|---|---|---|---|
| `eq7_from_lemmas` | FL-B | p.10, eq. (7); p.9, Lemmas 4, 5; p.6 payoff | "Combining Lemma 4, the following important fact can thus be established: E = Γ₀ − N(V*, S*)C. (7)" Hyp. `hlemma4`, `hlemma5`. |
| `eq7_count_cap` | FL-B, FL-H | p.10, eq. (7); p.6 | eq. (7) as above, and p.6 "f(0) = 0 and f′(0) > 0. We define H(·) ≡ f(·)/f′(·)." Hyp. `heffort : 0 ≤ E`, which is our inference from `H(0) = 0`. |
| `eq7_dissipation_exact` | FL-C | p.10 | "the equilibrium total effort is given by the difference between the total budget of the contest organizer and the total entry costs incurred by participating contestants, regardless of the contest technology." |
| `eq7_rhs_strictly_decreasing` | FL-B | p.10 | "In addition, the right-hand side of Equation (7) strictly decreases with N(V*, S*), the equilibrium number of participating contestants, for any N(V*, S*) ≥ 2." |
| `eq7_effort_bound` | FL-B | p.10 | "Hence, it can be deduced that the equilibrium efforts are bound from above by Ē = Γ₀ − 2C." |
| `eq7_hypotheses_satisfiable` | FL-B | p.11, Theorem 2 | "The optimally designed contest awards a unique equilibrium prize purse of V* = 4H(Γ₀/2 − C)(> 0). When C ≤ Γ₀/2 − H⁻¹(Γ₀/4), the contest organizer charges an entry fee of S* = [Γ₀/2 − 2H(Γ₀/2 − C)](≤ 0) to each contestant." Witness `(N,Γ₀,V,S,C,e,π,E) = (2,10,16,−3,1,4,0,8)`, Theorem 2 under `f(x) = x`. |
| `theorem1_exactly_two` | FL-D | p.10, Theorem 1; p.11, proof | "Theorem 1 The unique optimal contest induces exactly two potential contestants to participate, and induces the total effort of Ē = Γ₀ − 2C." and "Proof. Equation (7) shows that only a contest that attracts two contestants to participate may induce the total effort of Ē." Hyp. `hattain`, the existence half (eq. (8), Lemma 3). |
| `theorem1_needs_positive_cost` | FL-D (control) | p.13, Section 4.2 | "Hence, the optimal contest structure would not be unique, and the optimal number of participating contestants would not necessarily be two." |
| `zero_cost_effort_is_budget` | FL-D | p.13, Theorem 3 | "All these contests induce the same total amount of effort, Γ₀." |
| `fixed_rules_cap` | FL-G | p.6 payoff; p.7, Definition 1, eq. (5); p.7, Lemma 2 proof | "receives an equilibrium payoff of π(N, V, S) = (1/N)V − e(N, V, S) + S − C"; "A contest design (V, S) is feasible if and only if 0 ≤ V ≤ Γ₀ − N(V, S)S. (5)"; "Since contestants Ω_N enter the contest (V, S) if and only if π(N, V, S) ≥ 0". |
| `fixed_rules_count_cap` | FL-G | as above | as above, plus `heffort`. |
| `eq7_iff_lemma4_and_lemma5` | FL-G | p.10 | "From Lemmas 4 and 5, it follows that the effort-maximizing contest exhausts the resources available to the contest organizer, while causing all participating contestants to break even." |
| `eq7_fails_without_lemma4` | FL-G (control) | p.9, Lemma 4 | "Lemma 4 In the optimal feasible contest (V*, S*), every participating contestant breaks even, i.e. π(N(V*, S*), V*, S*) = 0." |
| `eq7_fails_without_lemma5` | FL-G (control) | p.9, Lemma 5 | "Lemma 5 In the optimal feasible contest (V*, S*), the contest organizer must put all the resources available in the prize purse, i.e., V* = Γ₀ − N(V*, S*)S*." |
| `countAfter_succ_of_enters`, `countAfter_succ_of_not_enters`, `countAfter_le`, `countAfter_mono`, `countAfter_invariant` | FL-A | p.5 | "It is assumed that they enter the contest sequentially, and that they are fully aware of the number of current participants." |
| `lemma2_count` | FL-A | p.7, Lemma 2 | "A contest (V, S) attracts a unique number of N(V, S) = arg max {N} [subscript: π(N,V,S) ≥ 0, 1 ≤ N ≤ M] contestants to participate if π(1, V, S) ≥ 0, since π(N, V, S) strictly decreases with N(≥ 1). If π(1, V, S) < 0, the contest will have no participant." |
| `lemma2_count_unique` | FL-A | p.7, Lemma 2 | "attracts a unique number of N(V, S)" |
| `lemma2_needs_decreasing` | FL-A (control) | p.7, Lemma 2 | "since π(N, V, S) strictly decreases with N(≥ 1)" |
| `stay_out_persists`, `entrants_form_prefix`, `countAfter_of_all_enter`, `enters_iff_position` | FL-F | p.5 and fn 6 | "Sequential entry and complete information ensure that potential contestants play pure strategies (0 or 1 probability of entry) in the entry stage of the game." |
| `simultaneous_identity_not_pinned` | FL-F (contrast) | p.5 | "A fixed pool of M(≥ 3) identical risk-neutral potential contestants demonstrate interest in the contest." |

**How the hypotheses encode the source.** `hpay` is the p.6 payoff
multiplied by `N`, which avoids division in `Int`. It agrees with eq. (4)
at `N = 1` because `e(1,V,S) = 0`. `htotal` is the p.7 definition of `E` at
a symmetric equilibrium. `Enters π k` is our reading of the entry rule for
the arrival at position `k`, namely enter if and only if the payoff at the
resulting count is non-negative. `π : Nat → Int` is `N ↦ π(N, V, S)` at fixed
`(V, S)`.

**Arithmetic domain.** Everything is over `Int`. Every money statement is
linear and homogeneous of degree one in `(Γ₀, V, S, C, e, π, E)` at fixed
`N`, so a rational counterexample would scale to an integer one. That
transfer argument is ours and is not machine-checked.

## Controls (README rule 3)

| Control | What is dropped | Witness | What it shows |
|---|---|---|---|
| `theorem1_needs_positive_cost` | `0 < C`, weakened to `0 ≤ C` | `N = 3, Γ₀ = 10, C = 0, E = 10` | Theorem 1's count conclusion fails at `C = 0`, which is the paper's own Section 4.2 world. |
| `eq7_fails_without_lemma4` | `π = 0`, weakened to `π ≥ 0` | Tullock, `Γ₀ = 8, (V,S) = (8,0), N = 2, e = 2, π = 1, E = 4 < 6` | (7) needs break-even. |
| `eq7_fails_without_lemma5` | `V = Γ₀ − NS`, weakened to `≤` | Tullock, `Γ₀ = 10, (V,S) = (4,0), N = 2, e = 1, π = 0, E = 2 < 8` | (7) needs budget exhaustion. |
| `lemma2_needs_decreasing` | strict decrease of `π` in `N` | `π(2) = 5`, `π(n) = −1` otherwise, `M = 3` | the sequential count is 0 while `π(2) ≥ 0`, so it is not Lemma 2's largest admissible count. |
| `simultaneous_identity_not_pinned` | sequential arrival | `π(n) = 5 − 2n`, profiles `{0,1}` and `{1,2}` | with simultaneous pure entry both profiles are Nash equilibria with the same count, so who enters is not pinned, while sequential arrival admits arrivals 0 and 1 only. |

FL-5 confirms in SymPy that both fixed-rules witnesses are feasible contests
at their Lemma-2 count under `f(x) = x`, so the Lean controls are Fu–Lu
contests and not arbitrary integers. Mutation runs of the suite on a scratch
copy, made during this pass, behave as required. Weakening `0 < C` breaks
compilation. Inserting `sorry` fails FL-L2 and FL-L3. A theorem proved with
`Classical.em` is reported as using `Classical.choice` and fails FL-L3. A
comment fails FL-L2. With no `lean` available the suite reports FAIL and
exits 1.

## SymPy and suite results

| ID | Result |
|---|---|
| FL-1 | CONSISTENT. `dE/dN = −C`, matching p.10 "strictly decreases with N(V*, S*)". |
| FL-2 | CONSISTENT. `N = 2` strictly beats `N ∈ {3,4,5,10}`, `E(2) = Γ₀ − 2C`, and `N = 3` is rejected as a maximiser. |
| FL-3 | CONTROL passes. With `C = 0`, `dE/dN = 0` and `E = Γ₀`, matching Theorem 3. |
| FL-4 | CONSISTENT. Theorem 2 under `f(x) = x`, `Γ₀ = 10`, `C = 1` gives `(V*,S*) = (16,−3)`, `N = 2`, `π = 0`, `E = 8 = Γ₀ − 2C`, and `N·C = 2 ≤ 10` with slack exactly `E`. |
| FL-5 | CONSISTENT. Both Lean control witnesses are feasible Lemma-2 equilibria where (7) fails. |
| FL-6 | CONSISTENT and non-vacuous. On a 41 × 25 grid of `(V,S)`, 394 contests are feasible and 631 are not. Of the feasible contests 105 attract no entrant, and the other 289 all satisfy `E ≤ Γ₀ − N·C`, 287 strictly. Equality holds exactly where `π = 0` and `V = Γ₀ − NS`, at `(16,−3)` with `N = 2` and at `(8, 1/2)` with `N = 4`. The maximum `E = 8` is attained only at `(16,−3)`. |
| FL-L1..L3 | `FuLu.lean` compiles; no `sorry`, `native_decide`, user axiom or comment; 27 of 27 theorems audited, core axioms only. |
| FL-L4 | Accounting.lean and FuLu.lean compiled together prove `N·C ≤ Γ₀` by applying `FuJiaoLu.accounting_bound` with count `N`, unit cost `C`, residual `e`, rent and budget both `Γ₀`. Fu–Lu's case is a third instance of the one lemma, beside FJL eq. (2) and P7. |

## Findings against `CLAIMS.md`, `LITERATURE.tex` and `PROOFS.tex`

None of these files outside this directory was edited.

**F1. Symbol (pass 1, corrected here).** The budget is `Γ₀` and Theorem 1's
value is `Ē = Γ₀ − 2C`. Neither `LITERATURE.tex` nor `PROOFS.tex` writes the
symbol, so the error was confined to this directory.

**F2. The count bound is not displayed by Fu and Lu (FL-B, FL-C).**
`PROOFS.tex` ("What is not claimed") says the accounting bound is "already
present in [...] \citet{FuLu2010} (equation~(7))". Eq. (7) is an equation in
total effort, and the bound Fu and Lu display from it is the effort bound
`Ē = Γ₀ − 2C` (p.10). The count bound `N·C ≤ Γ₀` needs one more step, that
`E ≥ 0`, which follows from `H(0) = 0` (p.6) but is not stated. This mirrors
the Fu–Jiao–Lu Definition 2 finding. A citation that reads "eq. (7) with
non-negative effort" survives a referee who opens p.10, and a citation of
eq. (7) alone invites the objection that the attribution is loose.

**F3. "Sharper" holds for the effort accounting and fails for the count bound
(FL-H).** `LITERATURE.tex` says "Fu--Lu's is the sharper case, (7) being an
equality rather than an inequality". Eq. (7) does pin the residual
`Γ₀ − N·C` to the total effort exactly, which FJL's inequality does not. The
count bound it implies, `N·C ≤ Γ₀`, is slack by exactly `E`, and at Fu–Lu's
optimum that slack is `Γ₀ − 2C > 0` whenever `C < Γ₀/2`. In the Tullock
instance `N·C = 2` against `Γ₀ = 10` (FL-4). The designer picks the smallest
admissible count, so the cap is as far from binding as it can be. The
falsifier for "sharper" read as a count bound would be an optimum where
`N·C = Γ₀`, which requires `E = 0` and so `C = Γ₀/2`, the boundary of
Assumption 1.

**F4. Eq. (7) is an optimum property and not an identity (FL-B, FL-C).**
p.10 states (7) "in the optimally designed contest", and the derivation uses
Lemmas 4 and 5. The Fu–Lu paragraph of `LITERATURE.tex` §sec:entry states (7)
without that qualifier and calls it "again the P7 accounting identity". Read
as holding at every contest, (7) is false (`eq7_fails_without_lemma4`,
`eq7_fails_without_lemma5`, FL-5), and P7's cap is an inequality rather
than an identity. The contribution list in `LITERATURE.tex` and
`PROOFS.tex` §P7 both carry the optimum qualifier, so only the §sec:entry
paragraph needs it.

**F5. The fixed-rules cap holds in Fu–Lu's own model (FL-G).** This is the
finding with the most weight for the novelty claim. At any feasible
contest, optimal or not, and at its Lemma-2 count, the p.6 payoff,
Definition 1 and Lemma 2 give `E ≤ Γ₀ − N·C`, hence `N·C ≤ Γ₀`
(`fixed_rules_cap`, `fixed_rules_count_cap`; FL-6 over the 289 feasible
Tullock contests with at least one entrant). With no transfer the inequality reads `N·C ≤ V − E`, the shape of
P7's `(m+1)κ ≤ V` for identical risk-neutral agents. Fu and Lu do not state
this inequality. What the designer adds is the equality.
`eq7_iff_lemma4_and_lemma5` shows that (7) holds exactly when break-even and
budget exhaustion both hold, and FL-6 finds such a contest that is not
optimal, `(V,S) = (8, 1/2)` with `N = 4`. So the survey's "their `N` is the
count in an optimally designed contest" is accurate about what Fu and Lu
display. "Holds under fixed rules with no designer" separates P7 from Fu–Lu's
displayed equation, and does not separate P7 from Fu–Lu's model, because a
referee can derive the fixed-rules cap from Fu–Lu's equations in one line.
The falsifier for this finding is a feasible Fu–Lu contest with
`E > Γ₀ − N·C` at its Lemma-2 count. `fixed_rules_cap` rules it out from p.6,
eq. (5) and Lemma 2 alone.

**F6. Against Fu–Lu, "pure-strategy" and "deterministic" do not separate
P7.** `PROOFS.tex` §P7 lists three distinctions, a pure-strategy count rather
than an expected one, utility units, and fixed rules with no designer.
`LITERATURE.tex` adds "deterministic". Fu–Lu's count is itself a
pure-strategy deterministic count (fn 6, "pure strategies (0 or 1
probability of entry)"; Lemma 2, "a unique number"). The pure-strategy
distinction therefore separates P7 from Fu–Jiao–Lu only. After F5, the
distinctions from Fu–Lu that survive the source are heterogeneous agents
against identical ones (p.5 "identical"; p.14 future research) and `κ` in
utility units against a money cost `C` borne by risk-neutral agents. The
sentence should say which distinction applies to which paper.

**F7. Footnote 6 says pure strategies, not identity (FL-F, FL-A).**
Footnote 6 gives sequential entry the job of making entry strategies pure.
Fu and Lu never mention which identical contestants enter, or any tie. The
reading that arrival order fixes who enters is ours, and it holds under our
explicit entry rule (`enters_iff_position`). The paper does not argue that
the rule is subgame perfect. A faithful attribution reads "sequential entry
with observation of the current count gives pure entry strategies (fn 6),
and arrival order then fixes which identical contestants enter". FL-A's "full
observation of current participants" is slightly stronger than p.5, which
says "fully aware of the number of current participants", meaning the
number and not the identities.

**F8. "Mixed or sequential entry" (contribution list item 1).** The phrase
covers Fu–Jiao–Lu (mixed) and Fu–Lu (sequential) jointly. Read as a claim
about Fu–Lu alone it is wrong, since fn 6 rules out mixing. This is an
ambiguity and not an error.

**F9. FL-D confirmed.** The causal direction is the paper's own (p.11
proof). Theorem 1 also needs `C > 0` (`theorem1_needs_positive_cost`, and
the paper's Section 4.2) and a two-entrant contest that attains `Ē`
(eq. (8) and Lemma 3, hypothesis `hattain`).

**F10. FL-E confirmed verbatim.** p.14, "One possible avenue for further
research is to allow for different types of contestants. Indeed, this is a
future research concern for the authors of this paper."

**F11. Outside this directory, for the caller.** The Fu–Jiao–Lu notes quote
their eq. (2) as holding "in a symmetric equilibrium" for given contest
rules (IJGT p.397). If that reading is right, "no designer" may not separate
P7 from FJL's eq. (2) either. Not checked here.

## Not attempted

- Lemma 1, Lemmas 4 and 5, Lemma 3 and the existence half of Theorem 1, and
  Theorem 2 for general `f`. See `RECONSTRUCTION.md` §7.
- Subgame perfection of the sequential entry rule.
- The published *Economic Inquiry* version, so title, pages and theorem
  numbers remain unchecked (`TODO.md` S1).
