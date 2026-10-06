# Reconstruction of Fu and Lu, "Contest design and optimal endogenous entry"

**Source read.** MPRA Paper No. 945, posted 28 Nov 2006, cover page plus 17
numbered pages, read in full from `fulu_contest_design.pdf` (Drive id
`1ykxVKP-O0X6CluhYDcsHx6NSlPw32Oul`). The title page reads "Contest Design
and Optimal Endogenous Entry", Qiang Fu and Jingfeng Lu, November 2006. Page
numbers below are the paper's printed page numbers, so printed p.10 is PDF
page 11. Symbols were read from rendered page images because the PDF text
layer drops the Greek letters. The *Economic Inquiry* version (2010, 48(1),
80-88) was not available, so every locator here is a working-paper locator
(README rule 5).

**Order of work, stated honestly.** The README asks for this file to be
written before the survey is read. In this pass the survey passages and
`CLAIMS.md` were read first, because the task brief listed them first. The
content below is built from the PDF alone, and every step cites the PDF and
nothing else. A reader should still treat the selection of what to
reconstruct as influenced by the survey.

## 1. Objects, in the paper's notation and ours

| Paper | Meaning | Locator | Our nearest object |
|---|---|---|---|
| `Γ₀` | organiser's fixed budget | p.5 | `V` in P7 when there are no transfers |
| `M (≥ 3)` | identical risk-neutral potential contestants | p.5 | `Q` potential challengers, but heterogeneous in P7 |
| `(V, S)` | prize purse `V ≥ 0`, per-entrant transfer `S ∈ ℜ` (subsidy if `S > 0`, fee if `S < 0`) | p.5, fn 5 | P7 has a fixed prize and no transfer |
| `C > 0` | fixed participation cost, money | p.5 | `κ`, in utility units in P7 |
| `Ω_N`, `N` | set and number of participants | p.5 | `m + 1` entrants in P7 |
| `f`, `H ≡ f/f′` | impact function, strictly increasing and weakly concave, `f(0) = 0`, `f′(0) > 0` | p.6 | none |
| `p_i` | win probability `f(e_i) / Σ f(e_j)`, eq. (1) | p.6 | P7 has no effort stage |
| `π(N, V, S)` | symmetric equilibrium payoff `(1/N)V − e(N,V,S) + S − C` | p.6 | net gain from entry |
| `e(N, V, S)` | equilibrium individual effort, eq. (3) | p.7 | none |
| `N(V, S)` | equilibrium number of entrants, Lemma 2 | p.7 | P5's `k*` |
| `E` | total effort `Σ_{i∈Ω_N(V,S)} e_i` | p.7 | none |
| `(V*, S*)` | the effort-maximising feasible contest | p.8 | none, since P7 has no designer |

## 2. Primitives, as the paper states them

1. **Three stages** (p.4-5). The organiser announces `(V, S)`. Potential
   contestants then decide on entry. Entrants then choose efforts
   simultaneously.
2. **Entry protocol** (p.5). "It is assumed that they enter the contest
   sequentially, and that they are fully aware of the number of current
   participants." Footnote 6 (p.5) reads "Sequential entry and complete
   information ensure that potential contestants play pure strategies (0 or
   1 probability of entry) in the entry stage of the game."
3. **Payoff** (p.6, eq. (2)). `π_i = p_i V − e_i + S − C`. Effort cost equals
   effort.
4. **Effort equilibrium** (Lemma 1, p.6-7). `e = 0` if `N = 1`, otherwise
   `e = H⁻¹((V/N)(1 − 1/N))`. The paper says Lemma 1 "can be established
   through standard techniques" and gives no proof.
5. **Entry count** (Lemma 2, p.7). `N(V,S)` is the largest `N ≤ M` with
   `π(N,V,S) ≥ 0`, "since π(N, V, S) strictly decreases with N(≥ 1)", and is
   zero when `π(1,V,S) < 0`.
6. **Feasibility** (Definition 1, p.7, eq. (5)). `0 ≤ V ≤ Γ₀ − N(V,S)S`.
7. **Objective** (p.8). Maximise `E` over feasible `(V, S)`.
8. **Assumption 1** (p.8). `C ≤ Γ₀/2`.

## 3. Derivation chain, and what each result is derived from

**Step A, effort.** Lemma 1 gives `e` and `π` as functions of `(N, V, S)`.
Derived from eq. (1), eq. (2) and the concavity of `f`. Not proved in the
paper.

**Step B, count.** Lemma 2 follows from the strict decrease of `π` in `N`. The
proof shows `g(x) = Vx − H⁻¹(x(1−x)V)` is increasing on `(0, 1/2]` using
`dH⁻¹/dy ∈ (0,1)`. The count is defined for every `(V, S)`, so the count is a
function of fixed rules.

**Step C, existence of a two-entrant contest** (Lemma 3, p.8-9, eq. (6)). A
feasible contest with at least two entrants exists if and only if
Assumption 1 holds. Derived from the intermediate value of
`Γ₀/2 − H⁻¹((Γ₀ − 2S)/4) − C` in `S`.

**Step D, two necessary conditions at the optimum** (Lemmas 4 and 5, p.9,
proofs in the Appendix p.15-16). At `(V*, S*)` every entrant breaks even,
`π(N(V*,S*),V*,S*) = 0`, and the budget is exhausted,
`V* = Γ₀ − N(V*,S*)S*`. Both proofs perturb `(V*, S*)` and use continuity of
`π` to keep the count fixed while raising the prize.

**Step E, equation (7)** (p.10). Substituting Lemma 5 into the symmetric
payoff and multiplying by `N` gives
`Nπ = (Γ₀ − NS*) − Ne + NS* − NC`, hence `E = Ne = Γ₀ − Nπ − NC`. The paper
then writes "Combining Lemma 4, the following important fact can thus be
established" and states `E = Γ₀ − N(V*,S*)C` as eq. (7). So eq. (7) is
derived from three inputs, namely the payoff identity of p.6, Lemma 5 and
Lemma 4. It is a property of the optimal contest, not an identity of the
model holding at every `(V, S)`.

**Step F, the effort bound** (p.10). The right-hand side of (7) "strictly
decreases with N(V*,S*) [...] for any N(V*,S*) ≥ 2", so total effort is
bounded by `Ē = Γ₀ − 2C`.

**Step G, Theorem 1** (p.10-11). The unique optimal contest has exactly two
entrants and total effort `Ē = Γ₀ − 2C`. The proof reads "Equation (7) shows
that only a contest that attracts two contestants to participate may induce
the total effort of Ē", and existence comes from eq. (8) and Lemma 3. The
argument uses `C > 0` to make the decrease strict.

**Step H, Theorem 2** (p.11). `V* = 4H(Γ₀/2 − C)`,
`S* = Γ₀/2 − 2H(Γ₀/2 − C)`, a fee when `C ≤ Γ₀/2 − H⁻¹(Γ₀/4)` and a subsidy
when `Γ₀/2 − H⁻¹(Γ₀/4) < C < Γ₀/2`.

**Step I, Corollary 1** (p.12). Individual effort is bounded by
`ē = Γ₀/2 − C`.

**Step J, zero entry cost** (Section 4.2 and Theorem 3, p.13). With `C = 0`
the analysis up to (7) still applies, the right-hand side of (7) no longer
depends on `N`, and every `N ∈ {2, ..., M}` is optimal with total effort
`Γ₀`.

## 4. Two consequences of the paper's own primitives that the paper does not display

These are our derivations from the paper's equations. Fu and Lu state
neither.

**4a. The count cap.** Efforts are non-negative because `H(0) = f(0)/f′(0) = 0`
(p.6), so `H⁻¹(0) = 0` and `H⁻¹` is strictly increasing, which makes every
`e(N,V,S) ≥ 0` and so `E ≥ 0`. Eq. (7) with `E ≥ 0` gives
`N(V*,S*)·C ≤ Γ₀`. The paper displays the effort bound `Ē = Γ₀ − 2C` and not
this count bound.

**4b. The fixed-rules inequality.** Take any feasible `(V, S)`, not
necessarily optimal, with `N = N(V,S) ≥ 1`. The p.6 payoff gives
`E = V + NS − NC − Nπ`. Definition 1 gives `V ≤ Γ₀ − NS`, and Lemma 2 gives
`π ≥ 0` at the equilibrium count. Hence `E ≤ Γ₀ − NC` at every feasible
contest, and `N·C ≤ Γ₀` whenever `E ≥ 0`. With no transfer (`S = 0`) the
inequality reads `N·C ≤ V − E`. Equality holds exactly when both slacks
vanish, that is when the Lemma 4 condition `π = 0` and the Lemma 5 condition
`V = Γ₀ − NS` hold together. The equality therefore marks break-even plus
budget exhaustion, which the optimum satisfies, and which some non-optimal
contests also satisfy (the Tullock instance in `verify_fulu.py` FL-6 has one
with `N = 4`).

The falsifier for 4b would be a feasible contest at its Lemma-2 count with
`E > Γ₀ − NC`. None exists, because each of the two inequalities used is a
primitive of the paper. The Tullock grid in FL-6 searches for one and finds
none among the 289 feasible contests with at least one entrant.

## 5. Role of each object, stated so a role error is visible

- **Eq. (7)** is a necessary condition of the designer's optimum, derived
  from Lemmas 4 and 5. Read as a relation that holds for every contest, it
  is false. FL-5 exhibits two feasible Tullock contests at which it fails.
- **`N(V*,S*)`** is the Lemma-2 count evaluated at the designer's choice.
  `N(V,S)` itself is defined for any fixed rules.
- **Sequential entry** has the role the paper gives it in footnote 6, which
  is to make entry strategies pure. Combined with Lemma 2 the count is
  unique. The paper says nothing about which identical contestants enter.
  Our reading is that, with arrival in sequence and the rule "enter if and
  only if the payoff at the resulting count is non-negative", the entrants
  are the first `N(V,S)` arrivals. That reading is ours, and it is what
  `FuLu.lean` formalises as `enters_iff_position`.
- **`C > 0`** is what makes the count matter. At `C = 0` the paper's own
  Theorem 3 gives many optimal counts.

## 6. Boundary observations about the source

- Theorem 2 states `V* = 4H(Γ₀/2 − C) (> 0)`. Because `H(0) = 0`, the
  bracket is zero at `C = Γ₀/2`, which Assumption 1 permits. At that
  boundary `V* = 0` and `Ē = 0`, so a one-entrant contest also gives zero
  effort and the uniqueness in Theorem 1 fails. The subsidy range in
  Theorem 2 is written with strict `C < Γ₀/2`, so the paper appears to
  intend a strict inequality. This does not touch any claim our documents
  make.
- The p.10 display of the payoff has `e(N(V*,S*),V*,S)` with `S` in place of
  `S*`. This is a typographical slip with no effect on (7).
- The Lemma 2 proof opens with "Clearly, π(1,V,S) > π(2,V,S)". At `V = 0`
  the two are equal. Again no effect on (7).

## 7. What could not be reconstructed

1. **Lemma 1.** The uniqueness of the symmetric effort equilibrium and the
   formula `H⁻¹((V/N)(1 − 1/N))` are stated without proof. Reconstructed
   only in the Tullock case `f(x) = x`, where `H⁻¹` is the identity, and only
   numerically (FL-4 to FL-6).
2. **Lemmas 4 and 5.** The Appendix proofs perturb the contest and rely on
   continuity of `π` in all arguments. Not reconstructed. In `FuLu.lean` both
   enter as hypotheses (`hlemma4`, `hlemma5`).
3. **Existence half of Theorem 1 and Lemma 3.** The intermediate-value
   argument over `S` needs continuity of `H⁻¹`. Not reconstructed. Its
   conclusion enters `theorem1_exactly_two` as the hypothesis `hattain`.
4. **Theorem 2 for general `f`.** Checked only in the Tullock instance.
5. **Subgame perfection of the entry stage.** The paper asserts in
   footnote 6 that sequential entry yields pure strategies and gives the
   count in Lemma 2. That the rule "enter if and only if the payoff at the
   resulting count is non-negative" is each arrival's best reply is not
   argued in the paper and is not reconstructed here.
6. **Real arithmetic.** `FuLu.lean` works over `Int`. The money statements
   are linear and homogeneous of degree one in `(Γ₀, V, S, C, e, π, E)` for
   fixed `N`, so a rational counterexample would scale to an integer one.
   That transfer argument is ours and is not machine-checked. Transfer to the
   reals is not checked.
7. **Published version.** Title, page numbers and theorem numbering in
   *Economic Inquiry* 48(1) are unchecked.
