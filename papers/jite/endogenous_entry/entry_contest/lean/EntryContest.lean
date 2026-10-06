/-
  Threshold entry in a fixed-prize contest.
  Lean 4 (core only, no Mathlib) verification of the discrete content.

  Scope note. Mathlib is deliberately not used, so the analytic steps
  (integration by parts; the Stieltjes difference identity) are verified in
  SymPy instead -- see checks/verify_sympy.py. The reason is auditability,
  not availability: staying on core Lean is what lets `#print axioms` certify
  every theorem below. verify.sh measures whether Mathlib is present on each
  run rather than asserting it. What is proved
  HERE is the discrete and order-theoretic content on which the equilibrium
  characterisation rests:

    P3-cor : from the sign of the step difference, Delta is antitone
    P5a    : from Delta antitone, the entry set is downward closed
    P5b    : hence the cutoff k* is unique -- no gaps, no second crossing
    P5c    : strict version
    Alg    : the algebraic factorisation inside the difference identity

  The analytic input enters only as the explicit hypothesis `step_nonpos`,
  so the dependency of the equilibrium result on the analytic identity is
  machine-checked rather than assumed.

  Order is axiomatised by explicit reflexivity/transitivity hypotheses
  rather than a typeclass, so nothing outside core Lean is required.
-/

namespace EntryContest

section Order

variable {α : Type}
variable (le : α → α → Prop)

/-- `f` never increases. -/
def Antitone' (f : Nat → α) : Prop := ∀ i j, i ≤ j → le (f j) (f i)

/-- **P3 corollary.** Adjacent non-increase implies global antitonicity.
    Bridge from the analytic step identity
    `Delta (m+1) - Delta m = -(V/2) * Int phi^2 dK <= 0`
    to monotonicity of the entry incentive in the number of entrants. -/
theorem antitone_of_step
    (lerefl : ∀ a, le a a)
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (f : Nat → α)
    (h : ∀ n, le (f (n + 1)) (f n)) : Antitone' le f := by
  intro i j hij
  induction j with
  | zero =>
    have hi : i = 0 := Nat.le_zero.mp hij
    subst hi
    exact lerefl _
  | succ k ih =>
    cases Nat.lt_or_ge i (k + 1) with
    | inl hlt =>
      have hik : i ≤ k := Nat.lt_succ_iff.mp hlt
      exact letrans _ _ _ (h k) (ih hik)
    | inr hge =>
      have hik : i = k + 1 := Nat.le_antisymm hij hge
      subst hik
      exact lerefl _

/-- Challenger `m` is willing to enter when `m` others already have. -/
def Enters (f : Nat → α) (kappa : α) (m : Nat) : Prop := le kappa (f m)

/-- **P5a. Prefix property.** If the incentive is antitone, the entry set is
    downward closed: if the `j`-th entrant is willing, so is every earlier
    one. This is what makes the equilibrium a threshold rule -- the old
    paper assumed it rather than deriving it. -/
theorem enters_downward_closed
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (f : Nat → α) (kappa : α)
    (hf : Antitone' le f) :
    ∀ i j, i ≤ j → Enters le f kappa j → Enters le f kappa i := by
  intro i j hij hj
  exact letrans _ _ _ hj (hf i j hij)

/-- **P5b. Uniqueness of the cutoff.** If `k` enters and `k+1` does not, the
    entry set is exactly `{0, ..., k}`: no gaps below, no re-entry above. -/
theorem cutoff_unique
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (f : Nat → α) (kappa : α)
    (hf : Antitone' le f) (k : Nat)
    (hk : Enters le f kappa k) (hk1 : ¬ Enters le f kappa (k + 1)) :
    (∀ i, i ≤ k → Enters le f kappa i) ∧
    (∀ j, k + 1 ≤ j → ¬ Enters le f kappa j) := by
  refine ⟨?_, ?_⟩
  · intro i hik
    exact enters_downward_closed le letrans f kappa hf i k hik hk
  · intro j hj hcon
    exact hk1 (enters_downward_closed le letrans f kappa hf (k + 1) j hj hcon)

/-- **Main discrete theorem.** Given that each step of the entry incentive is
    non-increasing -- the sign supplied by the analytic identity -- the
    equilibrium entry set is a prefix. Existence and uniqueness of `k*`
    follow with no auxiliary single-crossing or hazard-rate condition. -/
theorem equilibrium_is_threshold
    (lerefl : ∀ a, le a a)
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (Delta : Nat → α) (kappa : α)
    (step_nonpos : ∀ n, le (Delta (n + 1)) (Delta n)) :
    ∀ i j, i ≤ j → le kappa (Delta j) → le kappa (Delta i) := by
  intro i j hij hj
  exact enters_downward_closed le letrans Delta kappa
    (antitone_of_step le lerefl letrans Delta step_nonpos) i j hij hj

end Order

section Strict

variable {α : Type}
variable (lt : α → α → Prop)

/-- **P5c.** If every step strictly decreases, the incentive is strictly
    antitone, so the cutoff index is attained exactly once. -/
theorem strict_of_step
    (lttrans : ∀ a b c, lt a b → lt b c → lt a c)
    (f : Nat → α)
    (h : ∀ n, lt (f (n + 1)) (f n)) :
    ∀ i j, i < j → lt (f j) (f i) := by
  intro i j hij
  induction j with
  | zero => exact absurd hij (Nat.not_lt_zero i)
  | succ k ih =>
    cases Nat.lt_or_ge i k with
    | inl hlt => exact lttrans _ _ _ (h k) (ih hlt)
    | inr hge =>
      have hik : i = k := Nat.le_antisymm (Nat.lt_succ_iff.mp hij) hge
      subst hik
      exact h i

end Strict

section Saturation

/-!
  **P7. Saturation, uniformly in Q.**

  The analytic input is the uniform bound

      (m+1) * Delta m  <=  V        for every m and every Q,

  which holds because `H_m = F^m G^(Q-1-m) C <= F^m` (all factors are CDFs,
  hence at most 1) and `Int F^m dF = V/(m+1)`. Crucially the bound does not
  mention Q. The conclusion below is therefore uniform in Q: the entry set is
  contained in a fixed finite range determined by `V` and `kappa` alone.
-/

variable {α : Type}
variable (le : α → α → Prop)
variable (smul : Nat → α → α)

/-- If the incentive obeys the uniform bound `(m+1) * Delta m <= V` and
    challenger `m` is willing to enter (`kappa <= Delta m`), then
    `(m+1) * kappa <= V`. Since neither hypothesis involves `Q`, the set of
    entering indices is bounded independently of the number of challengers:
    `k*` saturates. -/
theorem entry_index_bounded
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (smul_mono : ∀ n a b, le a b → le (smul n a) (smul n b))
    (V kappa : α) (Delta : Nat → α)
    (bound : ∀ m, le (smul (m + 1) (Delta m)) V)
    (m : Nat) (hm : le kappa (Delta m)) :
    le (smul (m + 1) kappa) V :=
  letrans _ _ _ (smul_mono (m + 1) kappa (Delta m) hm) (bound m)

end Saturation

section ComparativeStatics

/-!
  **P8 / P9. Monotone comparative statics on the cutoff.**

  Sort challengers by wealth, descending. The `j`-th richest, entering as the
  `j`-th entrant, faces `j-1` others, so her entry condition is

      kappa (w_(j))  <=  Delta (j-1).

  Write `psi j` for the pair `(cost j, gain j)`. If a parameter change lowers
  the cost pointwise, or raises the gain pointwise, then the entry set can
  only grow. Everything below is stated as: a pointwise improvement in the
  entry condition preserves entry.

  This covers, uniformly:
    P8a  raising V     raises `gain`  pointwise  (Delta is proportional to V)
    P8b  raising c     raises `cost`  pointwise  (kappa increasing in c)
    P9   the spread    lowers `cost`  for entrants richer than the mean,
                       raises it for entrants poorer than the mean
  Crucially `gain` does NOT move under a wealth spread: the contest is
  anonymous, so Delta depends on the number of entrants only.
-/

variable {α : Type}
variable (le : α → α → Prop)

/-- Entry of index `j` under a given cost and gain schedule. -/
def EntersAt (cost gain : Nat → α) (j : Nat) : Prop := le (cost j) (gain j)

/-- **Pointwise improvement preserves entry.** If costs weakly fall and gains
    weakly rise at every index, then anyone who entered before still enters.
    Applying this with `cost' = cost` and `gain' >= gain` gives P8a; with
    `gain' = gain` and `cost' <= cost` gives P8b and the P9 rise branch. -/
theorem entry_monotone
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (cost gain cost' gain' : Nat → α)
    (hcost : ∀ j, le (cost' j) (cost j))
    (hgain : ∀ j, le (gain j) (gain' j))
    (j : Nat) (hj : EntersAt le cost gain j) :
    EntersAt le cost' gain' j :=
  letrans _ _ _ (hcost j) (letrans _ _ _ hj (hgain j))

/-- **Contrapositive: pointwise worsening preserves non-entry.** Used for the
    P9 fall branch, where the spread raises the cost of entrants poorer than
    the mean. -/
theorem nonentry_monotone
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (cost gain cost' gain' : Nat → α)
    (hcost : ∀ j, le (cost j) (cost' j))
    (hgain : ∀ j, le (gain' j) (gain j))
    (j : Nat) (hj : ¬ EntersAt le cost gain j) :
    ¬ EntersAt le cost' gain' j := by
  intro hcon
  exact hj (letrans _ _ _ (hcost j) (letrans _ _ _ hcon (hgain j)))

/-- **Single crossing.** The cost schedule is non-decreasing in `j` (poorer
    entrants later) and the gain schedule non-increasing (congestion, P3).
    Hence the entry set is a prefix: `k*` is unique with no fixed-point
    iteration, and the wealth distribution enters only through `cost`. -/
theorem single_crossing
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (cost gain : Nat → α)
    (hcost : ∀ i j, i ≤ j → le (cost i) (cost j))
    (hgain : ∀ i j, i ≤ j → le (gain j) (gain i))
    (i j : Nat) (hij : i ≤ j) (hj : EntersAt le cost gain j) :
    EntersAt le cost gain i :=
  letrans _ _ _ (hcost i j hij) (letrans _ _ _ hj (hgain i j hij))

end ComparativeStatics

section PivotSpread

/-!
  **P9 general. Pivot-spreads.**

  A map `T` is a *pivot-spread about `x0`* if it is monotone and satisfies
      `w <= T w`  whenever `x0 <= w`,      `T w <= w`  whenever `w <= x0`.
  The linear spread `T w = x0 + lam * (w - x0)` with `lam > 1` is the special
  case; a general mean-preserving spread need not pivot at the mean.

  Two facts drive P9. First, monotone `T` preserves the wealth ordering, so
  order statistics map through `T` index by index. Second, since the entry
  cost `kappa` is antitone in wealth, a pivot-spread lowers the cost of
  everyone above `x0` and raises it for everyone below. The gain schedule
  does not move at all (anonymity). Combining with `entry_monotone` and
  `nonentry_monotone` gives the two branches of P9.
-/

variable {α β : Type}
variable (lew : α → α → Prop)
variable (lec : β → β → Prop)

/-- `T` is monotone for the wealth order. -/
def MonotoneT (T : α → α) : Prop := ∀ a b, lew a b → lew (T a) (T b)

/-- `T` spreads about the pivot `x0`. -/
def PivotSpread (T : α → α) (x0 : α) : Prop :=
  (∀ w, lew x0 w → lew w (T w)) ∧ (∀ w, lew w x0 → lew (T w) w)

/-- **Above the pivot the entry cost weakly falls.** With `kappa` antitone in
    wealth, anyone at least as rich as the pivot faces a weakly lower cost
    after the spread. Feeding this to `entry_monotone` gives the rise branch
    of P9. -/
theorem cost_falls_above_pivot
    (T : α → α) (x0 : α) (kap : α → β)
    (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (hT : PivotSpread lew T x0)
    (w : α) (hw : lew x0 w) :
    lec (kap (T w)) (kap w) :=
  hkap w (T w) (hT.1 w hw)

/-- **Below the pivot the entry cost weakly rises.** Feeding this to
    `nonentry_monotone` gives the fall branch of P9. -/
theorem cost_rises_below_pivot
    (T : α → α) (x0 : α) (kap : α → β)
    (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (hT : PivotSpread lew T x0)
    (w : α) (hw : lew w x0) :
    lec (kap w) (kap (T w)) :=
  hkap (T w) w (hT.2 w hw)

/-- **Order preservation.** A monotone `T` maps the wealth ordering into
    itself, so the $j$-th richest before the spread is the $j$-th richest
    after it. This is what lets the argument run index by index on order
    statistics. -/
theorem order_preserved
    (T : α → α) (hT : MonotoneT lew T)
    (a b : α) (hab : lew a b) : lew (T a) (T b) :=
  hT a b hab

end PivotSpread

section Algebra

/-- **Alg.** The factorisation inside the difference identity: with
    `H m = K * G` and `H (m+1) = K * F`, the difference is `K * (F - G)`. -/
theorem step_factor (K F G : Int) : K * F - K * G = K * (F - G) :=
  (Int.mul_sub K F G).symm

/-- Equivalently the step difference is `-(K * phi)` for `phi = G - F`,
    which is the form in which its sign is read off. -/
theorem step_sign (K F G : Int) : K * F - K * G = -(K * (G - F)) := by
  rw [Int.mul_sub]
  omega

end Algebra

section PivotSpreadComposed

/-!
  **P9 general, composed.**

  The three pivot-spread lemmas above are each conditional deductions. This
  section assembles them, together with the entry-monotonicity results of
  `ComparativeStatics`, into the two branches of P9 stated directly on entry
  at an index -- so the chain from the pivot hypothesis to the sign of the
  entry decision is one checked object rather than an assembly left to the
  reader.

  The wealth profile enters as an indexed family `w : Nat -> alpha` (the order
  statistics, `w j` the j-th entrant in entry order). The gain schedule is
  fixed throughout: the contest is anonymous, so a spread of the wealth
  distribution does not move `gain`. That is the content of `hgain_fixed`
  below, and it is the reason a one-sided cost argument suffices.
-/

variable {α β : Type}
variable (lew : α → α → Prop)
variable (lec : β → β → Prop)

/-- Entry at index `j` when costs are read off a wealth profile through
    `kap` and compared against a gain schedule. -/
def EntersW (kap : α → β) (w : Nat → α) (gain : Nat → β) (j : Nat) : Prop :=
  lec (kap (w j)) (gain j)

/-- **P9-gen, rise branch.** If the marginal entrant at index `j` is at least
    as rich as the pivot, then a pivot-spread weakly lowers that entrant's
    cost, and anyone who entered before the spread still enters after it.
    Since this holds at the marginal index, `k*` weakly rises. -/
theorem entry_preserved_above_pivot
    (lectrans : ∀ a b c, lec a b → lec b c → lec a c)
    (T : α → α) (x0 : α) (kap : α → β)
    (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (hT : PivotSpread lew T x0)
    (w : Nat → α) (gain : Nat → β) (j : Nat)
    (hw : lew x0 (w j))
    (hj : EntersW lec kap w gain j) :
    EntersW lec kap (fun i => T (w i)) gain j :=
  lectrans _ _ _ (cost_falls_above_pivot lew lec T x0 kap hkap hT (w j) hw) hj

/-- **P9-gen, fall branch.** If the entrant at index `j` is no richer than the
    pivot, then a pivot-spread weakly raises that entrant's cost, and anyone
    who stayed out before the spread stays out after it. Since this holds at
    the marginal index, `k*` weakly falls. -/
theorem nonentry_preserved_below_pivot
    (lectrans : ∀ a b c, lec a b → lec b c → lec a c)
    (T : α → α) (x0 : α) (kap : α → β)
    (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (hT : PivotSpread lew T x0)
    (w : Nat → α) (gain : Nat → β) (j : Nat)
    (hw : lew (w j) x0)
    (hj : ¬ EntersW lec kap w gain j) :
    ¬ EntersW lec kap (fun i => T (w i)) gain j := by
  intro hcon
  exact hj (lectrans _ _ _
    (cost_rises_below_pivot lew lec T x0 kap hkap hT (w j) hw) hcon)

/-- **P9-gen, prefix form.** Under a pivot-spread with the marginal entrant
    above the pivot, the post-spread entry set still contains every index that
    entered before. Combined with `single_crossing` (which makes both entry
    sets prefixes) this is exactly "`k*` weakly rises": a prefix containing a
    prefix is at least as long. -/
theorem entry_set_grows_above_pivot
    (lectrans : ∀ a b c, lec a b → lec b c → lec a c)
    (T : α → α) (x0 : α) (kap : α → β)
    (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (hT : PivotSpread lew T x0)
    (w : Nat → α) (gain : Nat → β)
    (habove : ∀ j, EntersW lec kap w gain j → lew x0 (w j))
    (j : Nat) (hj : EntersW lec kap w gain j) :
    EntersW lec kap (fun i => T (w i)) gain j :=
  entry_preserved_above_pivot lew lec lectrans T x0 kap hkap hT w gain j
    (habove j hj) hj

end PivotSpreadComposed

section Dominance

/-!
  **P2. Pathwise dominance implies first-order stochastic dominance.**

  Scores are `X w = mu * r w + (1 - mu) * s w` for investors and
  `Y w = mu * r w` for non-investors. Since `s >= 0` and `mu <= 1`, we have
  `Y w <= X w` pathwise. The content of P2 is that this pointwise inequality
  forces the CDF ordering `F <= G`, and the mechanism is event inclusion:
  if `Y` is everywhere below `X`, then whenever `X` lands below a threshold
  `t`, so does `Y`. Hence `{X <= t}` is contained in `{Y <= t}`.

  Monotonicity of probability under inclusion is the one analytic input and
  is supplied as the hypothesis `hmono`; it is not proved here (no Mathlib
  measure theory in this development). What IS proved is the inclusion, which
  is the whole of the probabilistic content.
-/

variable {Ω α β : Type}
variable (le : α → α → Prop)
variable (leb : β → β → Prop)

/-- The event that a score lands weakly below a threshold. -/
def Below (Z : Ω → α) (t : α) : Ω → Prop := fun ω => le (Z ω) t

/-- **P2, event form.** Pathwise dominance `Y <= X` gives the inclusion
    `{X <= t}` implies `{Y <= t}`, for every threshold `t`. -/
theorem below_subset_of_pathwise
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (X Y : Ω → α)
    (hpath : ∀ ω, le (Y ω) (X ω))
    (t : α) (ω : Ω) (hω : Below le X t ω) : Below le Y t ω :=
  letrans _ _ _ (hpath ω) hω

/-- **P2, CDF form.** Given a probability functional `P` that is monotone
    under event inclusion, pathwise dominance yields `F t <= G t` at every
    threshold, where `F t = P {X <= t}` and `G t = P {Y <= t}`. -/
theorem cdf_le_of_pathwise
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (P : (Ω → Prop) → β)
    (hmono : ∀ A B : Ω → Prop, (∀ ω, A ω → B ω) → leb (P A) (P B))
    (X Y : Ω → α)
    (hpath : ∀ ω, le (Y ω) (X ω))
    (t : α) :
    leb (P (Below le X t)) (P (Below le Y t)) :=
  hmono _ _ (below_subset_of_pathwise le letrans X Y hpath t)

/-- **Nonnegativity of the gap.** `phi = G - F >= 0` pointwise is exactly the
    CDF ordering above, restated in the form used by P1 to conclude
    `Delta m >= 0`: investing never hurts. -/
theorem phi_nonneg_of_pathwise
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (P : (Ω → Prop) → β)
    (hmono : ∀ A B : Ω → Prop, (∀ ω, A ω → B ω) → leb (P A) (P B))
    (X Y : Ω → α)
    (hpath : ∀ ω, le (Y ω) (X ω))
    (t : α) :
    leb (P (Below le X t)) (P (Below le Y t)) :=
  cdf_le_of_pathwise le leb letrans P hmono X Y hpath t

end Dominance

section LinearSpread

/-!
  **P9 as a special case of P9-gen.**

  The linear mean-preserving spread `T w = x0 + lam * (w - x0)` with
  `lam >= 1` and `x0` the mean is a pivot-spread about `x0` in the sense of
  the `PivotSpread` section above. Proving that here reduces P9 to the
  already-composed general theorems: the two branches of P9 are then
  `entry_preserved_above_pivot` and `nonentry_preserved_below_pivot`
  instantiated at this `T`.

  Worked over `Int` so that the arithmetic is discharged in core Lean. The
  algebra is scale-free: what matters is the sign of `(lam - 1) * (w - x0)`.
-/

/-- The linear spread about `x0` with factor `lam`. -/
def linSpread (x0 lam w : Int) : Int := x0 + lam * (w - x0)

/-- The displacement identity: `T w - w = (lam - 1) * (w - x0)`. -/
theorem linSpread_sub (x0 lam w : Int) :
    linSpread x0 lam w - w = (lam - 1) * (w - x0) := by
  unfold linSpread
  rw [Int.sub_mul, Int.one_mul]
  omega

/-- Sign helper: above the pivot the displacement is nonnegative. -/
theorem disp_nonneg_above (x0 lam w : Int) (hlam : 1 ≤ lam) (hw : x0 ≤ w) :
    0 ≤ (lam - 1) * (w - x0) :=
  Int.mul_nonneg (by omega) (by omega)

/-- Sign helper: below the pivot the displacement is nonpositive. Derived
    from `Int.mul_nonneg` on the two flipped factors, avoiding any lemma
    outside core. -/
theorem disp_nonpos_below (x0 lam w : Int) (hlam : 1 ≤ lam) (hw : w ≤ x0) :
    (lam - 1) * (w - x0) ≤ 0 := by
  have h : 0 ≤ (lam - 1) * (x0 - w) :=
    Int.mul_nonneg (by omega) (by omega)
  have e : (lam - 1) * (x0 - w) = -((lam - 1) * (w - x0)) := by
    rw [← Int.neg_sub w x0, Int.mul_neg]
  omega

/-- **Above the pivot the linear spread moves wealth up.** -/
theorem linSpread_ge_of_above
    (x0 lam w : Int) (hlam : 1 ≤ lam) (hw : x0 ≤ w) :
    w ≤ linSpread x0 lam w := by
  have h := disp_nonneg_above x0 lam w hlam hw
  have e := linSpread_sub x0 lam w
  omega

/-- **Below the pivot the linear spread moves wealth down.** -/
theorem linSpread_le_of_below
    (x0 lam w : Int) (hlam : 1 ≤ lam) (hw : w ≤ x0) :
    linSpread x0 lam w ≤ w := by
  have h := disp_nonpos_below x0 lam w hlam hw
  have e := linSpread_sub x0 lam w
  omega

/-- **The linear spread is monotone**, so it preserves the wealth ordering
    and hence maps order statistics index by index. -/
theorem linSpread_monotone
    (x0 lam : Int) (hlam : 1 ≤ lam) (a b : Int) (hab : a ≤ b) :
    linSpread x0 lam a ≤ linSpread x0 lam b := by
  unfold linSpread
  have hba : b - x0 - (a - x0) = b - a := by omega
  have h : 0 ≤ lam * (b - a) := Int.mul_nonneg (by omega) (by omega)
  have e : lam * (b - x0) - lam * (a - x0) = lam * (b - a) := by
    rw [← Int.mul_sub, hba]
  omega

/-- **P9 reduces to P9-gen.** The linear mean-preserving spread satisfies the
    `PivotSpread` hypothesis about `x0`, so both branches of P9 follow from
    `entry_preserved_above_pivot` and `nonentry_preserved_below_pivot`. -/
theorem linSpread_isPivotSpread
    (x0 lam : Int) (hlam : 1 ≤ lam) :
    PivotSpread (· ≤ ·) (linSpread x0 lam) x0 :=
  ⟨fun w hw => linSpread_ge_of_above x0 lam w hlam hw,
   fun w hw => linSpread_le_of_below x0 lam w hlam hw⟩

end LinearSpread

section Strictness

/-!
  **P9 strictness.**

  Weak monotonicity of `k*` under a pivot-spread is P9-gen. Strictness needs
  three further steps, two of which are discrete and are proved here:

    (a) below the pivot, `T_lam w` is strictly decreasing in `lam`, and is
        unbounded below as `lam` grows -- so the marginal entrant's wealth
        can be pushed under any given level;
    (b) [analytic, NOT proved here] `kappa` diverges at the support floor,
        so a low enough wealth makes the entry cost exceed the fixed gain.
        This is the boundary condition `kappa w -> infinity as w -> c`,
        checked symbolically in checks/verify_sympy.py (S16);
    (c) once the marginal entrant fails to enter, the post-spread entry set
        is capped at that index -- because it is a prefix -- so the count
        strictly falls.

  Step (b) is the only analytic input and it enters as a hypothesis, in the
  same style as `step_nonpos` elsewhere in this file.
-/

/-- **(a) Strictly decreasing in `lam` below the pivot.** -/
theorem linSpread_strict_anti_in_lam
    (x0 w lam1 lam2 : Int) (hw : w < x0) (hlam : lam1 < lam2) :
    linSpread x0 lam2 w < linSpread x0 lam1 w := by
  unfold linSpread
  have hpos : 0 < (lam2 - lam1) * (x0 - w) :=
    Int.mul_pos (by omega) (by omega)
  have key : lam1 * (w - x0) - lam2 * (w - x0) = (lam2 - lam1) * (x0 - w) := by
    rw [← Int.sub_mul, ← Int.neg_sub lam2 lam1, ← Int.neg_sub x0 w,
        Int.neg_mul_neg]
  omega

/-- **(a) Unbounded below.** For a wealth strictly under the pivot and any
    target level `B` at or below the pivot, some admissible spread factor
    pushes that wealth below `B`. -/
theorem linSpread_unbounded_below
    (x0 w B : Int) (hw : w < x0) (hB : B ≤ x0) :
    ∃ lam, 1 ≤ lam ∧ linSpread x0 lam w ≤ B := by
  refine ⟨1 + (x0 - B), by omega, ?_⟩
  unfold linSpread
  have hnonneg : (0 : Int) ≤ 1 + (x0 - B) := by omega
  have hstep : (1 + (x0 - B)) * (w - x0) ≤ (1 + (x0 - B)) * (-1) :=
    Int.mul_le_mul_of_nonneg_left (by omega) hnonneg
  have hneg : (1 + (x0 - B)) * (-1) = -(1 + (x0 - B)) := Int.mul_neg_one _
  omega

/-- **(c) Marginal exit caps the entry set.** If the entry set after the
    spread is downward closed (which is `single_crossing`) and the index `j`
    does not enter, then no index at or above `j` enters. Combined with `j`
    having entered before the spread, the count strictly falls. -/
theorem entry_ceiling_of_marginal_exit
    {α : Type} (le : α → α → Prop)
    (cost gain : Nat → α)
    (hdc : ∀ i j, i ≤ j → EntersAt le cost gain j → EntersAt le cost gain i)
    (j : Nat) (hj : ¬ EntersAt le cost gain j)
    (i : Nat) (hij : j ≤ i) : ¬ EntersAt le cost gain i :=
  fun hi => hj (hdc j i hij hi)

/-- **P9-strict, discrete core.** Before the spread the marginal index `j`
    enters; after it, `j` does not. Then the post-spread entry set is
    contained in `{0,...,j-1}` while the pre-spread set contains `j`, so the
    count strictly falls. Stated as the two facts that witness it. -/
theorem strict_drop_of_marginal_exit
    {α : Type} (le : α → α → Prop)
    (cost gain cost' : Nat → α)
    (hdc' : ∀ i j, i ≤ j → EntersAt le cost' gain j → EntersAt le cost' gain i)
    (j : Nat)
    (hbefore : EntersAt le cost gain j)
    (hafter : ¬ EntersAt le cost' gain j) :
    EntersAt le cost gain j ∧ (∀ i, j ≤ i → ¬ EntersAt le cost' gain i) :=
  ⟨hbefore, entry_ceiling_of_marginal_exit le cost' gain hdc' j hafter⟩

end Strictness

section CountUniqueness

/-!
  **P5-inv. The equilibrium count is unique across ALL pure-strategy
  equilibria, not merely within the assortative one.**

  This closes a gap found on 2 September 2026 (see `SOUNDNESS_20260902.md`).
  The earlier development proved prefix structure and cutoff uniqueness
  *within* the assortative schedule -- `cost j` is by construction the cost of
  the `j`-th richest -- and the prose then claimed more than that, namely that
  the wealth ordering pins the *identities* of the entrants. It does not: a
  three-agent instance with `Delta = (12,10,1)` and `kappa = (2,5,8)` has three
  equilibria, `{1,2}`, `{1,3}` and `{2,3}`, one of them with the richest agent
  absent.

  What IS true, and is proved here, is that every pure-strategy equilibrium has
  the same size `k*`. Agents are indexed in increasing cost order (equivalently
  decreasing wealth), so `cost` is monotone. An entrant set of size `k` makes
  each member compare her cost against `gain (k-1)` and each outsider against
  `gain k`.

  Two facts about a size-`k` set of distinct indices are used, and only these
  two. They are pure pigeonhole and enter as hypotheses, in the same
  explicit-hypothesis style as `step_nonpos`:

    (high) some member sits at index `>= k-1`;
    (low)  some index `<= k` is NOT a member.

  Both are discharged by exhaustive enumeration in
  `checks/verify_equilibria.py`, which also checks the conclusions below
  directly on random instances.
-/

variable {α : Type}
variable (le : α → α → Prop)

/-- **P5-inv (a). No equilibrium is larger than `k*`.**
    A member at index `>= k-1` costs at least `cost (k-1)`, so the entry
    condition holds at index `k-1` with `k-1` rivals, and maximality of `k*`
    bounds `k`. -/
theorem count_le_kstar
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (cost gain : Nat → α)
    (hcost : ∀ i j, i ≤ j → le (cost i) (cost j))
    (k kstar : Nat)
    (hmax : ∀ m, le (cost (m - 1)) (gain (m - 1)) → m ≤ kstar)
    (i : Nat) (hi : k - 1 ≤ i)
    (hmem : le (cost i) (gain (k - 1))) :
    k ≤ kstar :=
  hmax k (letrans _ _ _ (hcost (k - 1) i hi) hmem)

/-- **P5-inv (b). No equilibrium is smaller than `k*`.**
    Below `k*` the entry condition still holds at index `k` with `k` rivals, so
    the omitted agent at some index `<= k` would profitably deviate in. -/
theorem count_ge_kstar
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (cost gain : Nat → α)
    (hcost : ∀ i j, i ≤ j → le (cost i) (cost j))
    (k kstar : Nat)
    (hprefix : ∀ j, 1 ≤ j → j ≤ kstar → le (cost (j - 1)) (gain (j - 1)))
    (j : Nat) (hj : j ≤ k)
    (hout : ¬ le (cost j) (gain k)) :
    kstar ≤ k := by
  cases Nat.lt_or_ge k kstar with
  | inr hge => exact hge
  | inl hlt =>
    have hk : k + 1 ≤ kstar := by omega
    have h1 : le (cost (k + 1 - 1)) (gain (k + 1 - 1)) :=
      hprefix (k + 1) (by omega) hk
    have hk1 : k + 1 - 1 = k := by omega
    rw [hk1] at h1
    exact absurd (letrans _ _ _ (hcost j k hj) h1) hout

/-- **P5-inv. Every pure-strategy equilibrium has size exactly `k*`.**
    The equilibrium count is an invariant of the game rather than a property of
    the assortative construction. This is what all of P7, P8, P9, P9-gen,
    P9-str and P-MU are statements about, so those results are unaffected by
    the identity multiplicity described above. -/
theorem equilibrium_count_unique
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (cost gain : Nat → α)
    (hcost : ∀ i j, i ≤ j → le (cost i) (cost j))
    (k kstar : Nat)
    (hmax : ∀ m, le (cost (m - 1)) (gain (m - 1)) → m ≤ kstar)
    (hprefix : ∀ j, 1 ≤ j → j ≤ kstar → le (cost (j - 1)) (gain (j - 1)))
    (i : Nat) (hi : k - 1 ≤ i) (hmem : le (cost i) (gain (k - 1)))
    (j : Nat) (hj : j ≤ k) (hout : ¬ le (cost j) (gain k)) :
    k = kstar :=
  Nat.le_antisymm
    (count_le_kstar le letrans cost gain hcost k kstar hmax i hi hmem)
    (count_ge_kstar le letrans cost gain hcost k kstar hprefix j hj hout)

/-- **P5-id. When identities are pinned after all.**
    If the first excluded agent would not enter even at the most favourable
    slot -- `cost k*` fails the entry condition against `gain (k*-1)` -- then
    every member of a size-`k*` equilibrium has index below `k*`. With the
    count fixed at `k*` by `equilibrium_count_unique`, the entrant set is
    exactly `{0,...,k*-1}`: the assortative equilibrium is the unique one.
    Without this condition assortativity is a selection, not a consequence. -/
theorem members_below_kstar
    (letrans : ∀ a b c, le a b → le b c → le a c)
    (cost gain : Nat → α)
    (hcost : ∀ i j, i ≤ j → le (cost i) (cost j))
    (kstar : Nat)
    (hcond : ¬ le (cost kstar) (gain (kstar - 1)))
    (i : Nat) (hmem : le (cost i) (gain (kstar - 1))) :
    i < kstar := by
  cases Nat.lt_or_ge i kstar with
  | inl h => exact h
  | inr h => exact absurd (letrans _ _ _ (hcost kstar i h) hmem) hcond

end CountUniqueness

section EndogenousMargin

/-!
  **N1. The pivot rule with an endogenously determined margin.**

  The pivot theorems above are stated at a fixed index `j`. The comparative
  static of interest is not at a fixed index: `k*` is determined by the entry
  condition and moves when the distribution moves, so the "marginal entrant"
  is a different agent before and after the spread. The question is whether
  the sign rule survives that.

  It does, and the reason is the wealth ordering. Challengers are sorted
  **descending**, `w 0 >= w 1 >= ...`, so a pivot comparison made at the
  marginal index automatically controls one entire side of the profile:

    * if the marginal entrant is above the pivot, every *inframarginal*
      entrant is richer, hence also above the pivot -- so no entrant's cost
      rises, and the entry set cannot shrink;
    * if the marginal entrant is below the pivot, every *extramarginal*
      agent is poorer, hence also below the pivot -- so no outsider's cost
      falls, and the entry set cannot grow.

  Neither direction needs to locate the *post*-spread margin. That is what
  makes the rule well posed with `k*` endogenous, and it is a strictly
  stronger statement than the fixed-index version: the hypothesis is a single
  comparison at the pre-spread margin.
-/

variable {α β : Type}
variable (lew : α → α → Prop)
variable (lec : β → β → Prop)

/-- Challengers sorted by wealth, descending: a later index is no richer. -/
def Descending (w : Nat → α) : Prop := ∀ i j, i ≤ j → lew (w j) (w i)

/-- **Inframarginal coverage.** If the agent at index `k` is above the pivot,
    so is every agent at a lower index. -/
theorem above_pivot_inframarginal
    (lewtrans : ∀ a b c, lew a b → lew b c → lew a c)
    (w : Nat → α) (hdesc : Descending lew w)
    (x0 : α) (k : Nat) (hk : lew x0 (w k))
    (j : Nat) (hj : j ≤ k) : lew x0 (w j) :=
  lewtrans _ _ _ hk (hdesc j k hj)

/-- **Extramarginal coverage.** If the agent at index `k` is below the pivot,
    so is every agent at a higher index. -/
theorem below_pivot_extramarginal
    (lewtrans : ∀ a b c, lew a b → lew b c → lew a c)
    (w : Nat → α) (hdesc : Descending lew w)
    (x0 : α) (k : Nat) (hk : lew (w k) x0)
    (j : Nat) (hj : k ≤ j) : lew (w j) x0 :=
  lewtrans _ _ _ (hdesc k j hj) hk

/-- **N1, rise branch.** Pivot-spread, marginal entrant at index `k` above
    the pivot. Then every index at or below `k` that entered before still
    enters after, so the entry count cannot fall. The hypothesis is a single
    comparison at the pre-spread margin; the ordering supplies the rest. -/
theorem entry_preserved_below_marginal_index
    (lewtrans : ∀ a b c, lew a b → lew b c → lew a c)
    (lectrans : ∀ a b c, lec a b → lec b c → lec a c)
    (T : α → α) (x0 : α) (kap : α → β)
    (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (hT : PivotSpread lew T x0)
    (w : Nat → α) (hdesc : Descending lew w)
    (gain : Nat → β) (k : Nat) (hmarg : lew x0 (w k))
    (j : Nat) (hj : j ≤ k) (hin : EntersW lec kap w gain j) :
    EntersW lec kap (fun i => T (w i)) gain j :=
  entry_preserved_above_pivot lew lec lectrans T x0 kap hkap hT w gain j
    (above_pivot_inframarginal lew lewtrans w hdesc x0 k hmarg j hj) hin

/-- **N1, fall branch.** Pivot-spread, marginal entrant at index `k` below
    the pivot. Then every index at or above `k` that stayed out before stays
    out after, so the entry count cannot rise. -/
theorem nonentry_preserved_above_marginal_index
    (lewtrans : ∀ a b c, lew a b → lew b c → lew a c)
    (lectrans : ∀ a b c, lec a b → lec b c → lec a c)
    (T : α → α) (x0 : α) (kap : α → β)
    (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (hT : PivotSpread lew T x0)
    (w : Nat → α) (hdesc : Descending lew w)
    (gain : Nat → β) (k : Nat) (hmarg : lew (w k) x0)
    (j : Nat) (hj : k ≤ j) (hout : ¬ EntersW lec kap w gain j) :
    ¬ EntersW lec kap (fun i => T (w i)) gain j :=
  nonentry_preserved_below_pivot lew lec lectrans T x0 kap hkap hT w gain j
    (below_pivot_extramarginal lew lewtrans w hdesc x0 k hmarg j hj) hout

/-- **N1, fall branch, on the hypothesis it actually needs.** Applied only at
    ranks strictly beyond the margin, which is where the outsiders are: at `k`
    itself the entry condition holds, so no claim about that index is made or
    required. This is the form Proposition (endogenous margin) (ii) states. -/
theorem nonentry_preserved_beyond_marginal_index
    (lewtrans : ∀ a b c, lew a b → lew b c → lew a c)
    (lectrans : ∀ a b c, lec a b → lec b c → lec a c)
    (T : α → α) (x0 : α) (kap : α → β)
    (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (hT : PivotSpread lew T x0)
    (w : Nat → α) (hdesc : Descending lew w)
    (gain : Nat → β) (k : Nat) (hmarg : lew (w k) x0)
    (j : Nat) (hj : k < j) (hout : ¬ EntersW lec kap w gain j) :
    ¬ EntersW lec kap (fun i => T (w i)) gain j :=
  nonentry_preserved_above_marginal_index lew lec lewtrans lectrans T x0 kap
    hkap hT w hdesc gain k hmarg j (Nat.le_of_lt hj) hout

/-- **A monotone spread preserves the descending order**, so the post-spread
    profile is still sorted and the single-crossing lemma still applies to it.
    Without this the post-spread `k*` need not be well defined. -/
theorem descending_preserved
    (T : α → α) (hmono : MonotoneT lew T)
    (w : Nat → α) (hdesc : Descending lew w) :
    Descending lew (fun i => T (w i)) :=
  fun i j hij => hmono (w j) (w i) (hdesc i j hij)

end EndogenousMargin

section AssortativeSelection

/-!
  **N4. Assortative entry as a selection among equilibria.**

  `equilibrium_count_unique` says every pure-strategy equilibrium has the same
  size `k*`, but not that they have the same members: where
  `kappa (k*+1) <= Delta (k*-1)`, several distinct entrant sets are equilibria
  (see `SOUNDNESS_20260902.md`). The assortative set -- the `k*` agents with
  the lowest entry cost -- is therefore a *selection*, and needs an argument.

  The argument is that it is the cheapest equilibrium. Because the contest is
  anonymous, the gain schedule depends on the entrant *count* alone, and the
  count is the same in every equilibrium; so aggregate surplus differs across
  equilibria only through the total entry cost actually paid. Minimising that
  total is maximising aggregate surplus.

  The core of the minimisation is a one-step exchange: replacing a member by a
  cheaper non-member never raises the total. That is what is proved here. The
  full statement -- that the `k*` cheapest agents minimise the total over all
  size-`k*` sets -- follows by iterating exchanges and is the standard sorting
  fact; it is checked numerically in `checks/verify_equilibria.py` rather than
  proved here.
-/

/-- Total entry cost actually paid by a set of entrants, listed by index. -/
def totalCost (kap : Nat → Int) (S : List Nat) : Int := (S.map kap).sum

/-- **Exchange step.** If costs are non-decreasing in the index, swapping a
    member for one with a lower index never raises the total. -/
theorem cost_exchange_le
    (kap : Nat → Int) (hmono : ∀ a b, a ≤ b → kap a ≤ kap b)
    (S : List Nat) (i j : Nat) (hij : i ≤ j) :
    totalCost kap (i :: S) ≤ totalCost kap (j :: S) := by
  unfold totalCost
  simp only [List.map_cons, List.sum_cons]
  exact Int.add_le_add_right (hmono i j hij) _

/-- **Cost is additive over the entrant list**, so the exchange step composes. -/
theorem totalCost_cons (kap : Nat → Int) (i : Nat) (S : List Nat) :
    totalCost kap (i :: S) = kap i + totalCost kap S := by
  unfold totalCost
  simp only [List.map_cons, List.sum_cons]

/-- **Anonymity consequence.** With the gain schedule a function of the
    entrant count alone, and the count equal across equilibria, the only term
    distinguishing two equilibria is the total entry cost. Stated as: if two
    entrant lists have the same length, their aggregate payoffs differ exactly
    by the difference of their total costs. -/
theorem surplus_gap_is_cost_gap
    (kap : Nat → Int) (gain : Nat → Int) (S T : List Nat)
    (hlen : S.length = T.length) :
    (gain S.length - totalCost kap S) - (gain T.length - totalCost kap T)
      = totalCost kap T - totalCost kap S := by
  rw [hlen]
  omega

end AssortativeSelection

section GeneralSpreads

/-!
  **R1. Beyond pivot-spreads: a one-sided tail condition.**

  Theorem `entry_preserved_above_pivot` assumes `T` is a pivot-spread, i.e.
  that the displacement `T w - w` changes sign exactly once. That is the
  dispersive-order case. A general mean-preserving spread need not be
  dispersive: its quantile difference may cross zero arbitrarily many times.

  The pivot hypothesis is stronger than the argument needs. Compare two
  *sorted* profiles `w` and `w'` rank by rank -- which is always available,
  since the rank-wise comparison of two distributions is their quantile
  coupling -- and the entry conclusion needs only the sign of the
  displacement on **one side of the margin**:

    * `w j <= w' j` for every `j <= k` (ranks at or above the margin) gives
      that no pre-spread entrant exits, hence the count cannot fall;
    * `w' j <= w j` for every `j >= k` gives that no pre-spread outsider
      enters, hence the count cannot rise.

  Nothing is assumed about the other side, so the displacement may cross zero
  any number of times there. This is why the count is *easier* than the
  aggregates signed by the assignment literature: `k*` is a threshold
  statistic reading the quantile function down to the margin, not an integral
  over all ranks, so no integration-by-parts and no log-concavity condition
  is required.
-/

variable {α β : Type}
variable (lew : α → α → Prop)
variable (lec : β → β → Prop)

/-- Displacement is non-negative on the ranks at or above the margin `k`. -/
def UpToMargin (w w' : Nat → α) (k : Nat) : Prop :=
  ∀ j, j ≤ k → lew (w j) (w' j)

/-- Displacement is non-positive on the ranks at or below the margin `k`. -/
def DownFromMargin (w w' : Nat → α) (k : Nat) : Prop :=
  ∀ j, k ≤ j → lew (w' j) (w j)

/-- Displacement is non-positive on the ranks **strictly** below the margin
    `k`, i.e. on the outsiders only. This is what the fall branch actually
    needs: the agent at the margin is an entrant, so the entry condition holds
    there and nothing has to be assumed about how her wealth moves. -/
def BeyondMargin (w w' : Nat → α) (k : Nat) : Prop :=
  ∀ j, k < j → lew (w' j) (w j)

/-- **R1, rise branch.** No pivot hypothesis: only the sign of the
    displacement at ranks at or above the margin. Every pre-spread entrant at
    such a rank still enters, so the count cannot fall. -/
theorem entry_preserved_of_up_to_margin
    (lectrans : ∀ a b c, lec a b → lec b c → lec a c)
    (kap : α → β) (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (w w' : Nat → α) (gain : Nat → β) (k : Nat)
    (hup : UpToMargin lew w w' k)
    (j : Nat) (hj : j ≤ k) (hin : EntersW lec kap w gain j) :
    EntersW lec kap w' gain j :=
  lectrans _ _ _ (hkap (w j) (w' j) (hup j hj)) hin

/-- **R1, fall branch, on the hypothesis it actually needs.** Only the
    outsiders' ranks are constrained. The marginal entrant may move either
    way: she satisfies the entry condition before the spread, so she is never
    a `j` at which this theorem is applied. -/
theorem nonentry_preserved_of_beyond_margin
    (lectrans : ∀ a b c, lec a b → lec b c → lec a c)
    (kap : α → β) (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (w w' : Nat → α) (gain : Nat → β) (k : Nat)
    (hbey : BeyondMargin lew w w' k)
    (j : Nat) (hj : k < j) (hout : ¬ EntersW lec kap w gain j) :
    ¬ EntersW lec kap w' gain j :=
  fun hcon => hout (lectrans _ _ _ (hkap (w' j) (w j) (hbey j hj)) hcon)

/-- **The old fall-branch hypothesis is strictly stronger than needed.** It
    constrains the margin itself; `BeyondMargin` does not. -/
theorem downFromMargin_imp_beyondMargin
    (w w' : Nat → α) (k : Nat) (hdown : DownFromMargin lew w w' k) :
    BeyondMargin lew w w' k :=
  fun j hj => hdown j (Nat.le_of_lt hj)

/-- **R1, fall branch.** Symmetric: only the sign of the displacement at ranks
    at or below the margin. Superseded by
    `nonentry_preserved_of_beyond_margin`, which assumes less; kept because
    the pivot bridge below lands on this form. -/
theorem nonentry_preserved_of_down_from_margin
    (lectrans : ∀ a b c, lec a b → lec b c → lec a c)
    (kap : α → β) (hkap : ∀ a b, lew a b → lec (kap b) (kap a))
    (w w' : Nat → α) (gain : Nat → β) (k : Nat)
    (hdown : DownFromMargin lew w w' k)
    (j : Nat) (hj : k ≤ j) (hout : ¬ EntersW lec kap w gain j) :
    ¬ EntersW lec kap w' gain j :=
  fun hcon => hout (lectrans _ _ _ (hkap (w' j) (w j) (hdown j hj)) hcon)

/-- **The pivot hypothesis implies the tail condition**, so
    `entry_preserved_below_marginal_index` is a special case of
    `entry_preserved_of_up_to_margin`. The converse fails: a displacement may
    cross zero repeatedly below the margin and still satisfy `UpToMargin`. -/
theorem pivot_imp_upToMargin
    (lewtrans : ∀ a b c, lew a b → lew b c → lew a c)
    (T : α → α) (x0 : α) (hT : PivotSpread lew T x0)
    (w : Nat → α) (hdesc : Descending lew w)
    (k : Nat) (hmarg : lew x0 (w k)) :
    UpToMargin lew w (fun i => T (w i)) k :=
  fun j hj =>
    hT.1 (w j) (above_pivot_inframarginal lew lewtrans w hdesc x0 k hmarg j hj)

/-- Likewise for the fall branch. -/
theorem pivot_imp_downFromMargin
    (lewtrans : ∀ a b c, lew a b → lew b c → lew a c)
    (T : α → α) (x0 : α) (hT : PivotSpread lew T x0)
    (w : Nat → α) (hdesc : Descending lew w)
    (k : Nat) (hmarg : lew (w k) x0) :
    DownFromMargin lew w (fun i => T (w i)) k :=
  fun j hj =>
    hT.2 (w j) (below_pivot_extramarginal lew lewtrans w hdesc x0 k hmarg j hj)

end GeneralSpreads

section TailBand

/-!
  **Open item 3: exactly which margins the tail condition covers.**

  Write the rank-by-rank displacement of two descending profiles as
  `d j = w' j - w j`, and let `L` be the richest rank whose wealth falls and
  `M` the poorest rank whose wealth rises. The two hypotheses of the tail
  condition are then exactly `k ≤ L` (rise branch) and `M < k` (fall branch),
  so the proposition is silent precisely on margins with `L < k` and `k ≤ M`
  -- the *band*.

  `band_empty_iff` settles what dispersiveness buys, as an iff rather than a
  slogan: the rule covers **every** margin exactly when `M ≤ L`, which is
  exactly the condition that no rank which rises is poorer than a rank which
  falls -- single crossing of the displacement. So a pivot-spread is not a
  convenient special case; it is *the* class on which the rule is margin-free.

  The complementary fact is negative and is deliberately not formalised here,
  being a statement about particular economies rather than about the order
  structure: inside the band both directions occur, so the hypothesis cannot be
  weakened. Two explicit witnesses are exhibited in
  `checks/verify_tailband.py` (T4).
-/

/-- A margin `k` is **covered** when one of the two branches applies. -/
def Covered (L M k : Nat) : Prop := k ≤ L ∨ M < k

/-- **The rule is margin-free exactly on dispersive spreads.** -/
theorem band_empty_iff (L M : Nat) : (∀ k, Covered L M k) ↔ M ≤ L := by
  constructor
  · intro h
    rcases h (L + 1) with hle | hlt
    · exact absurd hle (Nat.not_succ_le_self L)
    · exact Nat.lt_succ_iff.mp hlt
  · intro hML k
    rcases Nat.lt_or_ge k (L + 1) with hk | hk
    · exact Or.inl (Nat.lt_succ_iff.mp hk)
    · exact Or.inr (Nat.lt_of_le_of_lt hML hk)

/-- Contrapositive form: if some rank that rises is strictly poorer than some
    rank that falls, a margin exists at which the rule says nothing. -/
theorem band_nonempty_of_lt (L M : Nat) (h : L < M) :
    ¬ (∀ k, Covered L M k) :=
  fun hall => absurd ((band_empty_iff L M).mp hall) (Nat.not_le.mpr h)

end TailBand

end EntryContest
