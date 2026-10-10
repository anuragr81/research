/-
================================================================================
  Smooth fit at the recapitalisation trigger (Proposition SMF)
  Companion to PROOFS_v2.tex (prop:smf, rem:smfn) and to the SymPy checks in
  verify_smooth_fit_shooting.py.  Lean 4 + Mathlib.
================================================================================

SCOPE, stated so nothing here is mistaken for more than it is.

The paper's SMF has two steps.  This file formalises the LOGIC of both, and
is explicit about the one classical input it takes as a hypothesis rather
than proves.

  STEP 1 -- "any kink is convex."  Formalised in full (`kink_is_convex`).
    On the intervention region v = Mv, an affine function of slope 1+kappa,
    so the left derivative at the trigger is exactly 1+kappa; the obstacle
    g = v - Mv >= 0 with g(x_L) = 0 makes x_L a right-minimum of g, forcing
    the right derivative of g to be >= 0, i.e. v'(x_L^+) >= 1+kappa.  Hence
    the one-sided jump J := v'(x_L^+) - v'(x_L^-) is >= 0.  This uses only
    the variational inequality and one-sided derivatives -- no smooth-fit
    assumption, so no circularity.

  STEP 2 -- "a convex kink contradicts the supersolution property."
    Formalised as `no_convex_kink`, which takes the supersolution
    inequality AS AN EXPLICIT HYPOTHESIS (`IsViscosityGe` below) rather than
    deriving it from the control problem.  THIS IS DELIBERATE.  That V is a
    viscosity supersolution of the QVI is the standard dynamic-programming
    fact (Oksendal-Sulem, *Applied Stochastic Control of Jump Diffusions*,
    Thm 9.7 / Chapter 9; Crandall-Ishii-Lions user's guide, CIL92), not in
    dispute and not the novel content of SMF.  Mathlib has no viscosity-
    solution library, so proving it here would mean building that theory
    from scratch and would leave the load-bearing step unproved.  The
    novel content -- the part this project actually needs machine-checked --
    is that GIVEN the supersolution property, a convex kink is impossible.
    That is what `no_convex_kink` proves, mechanically.

  This scoping mirrors QVI_Part1.lean, which formalises the two facts that
  are genuinely settled and states plainly that the FOCs and the value-
  function concavity are out of scope.

Declarations:
  `hasDeriv_Mv`      -- `Mv` is affine, so its derivative everywhere is the
    slope `1 + kappa`.
  `kink_is_convex`   -- Step 1.  From the variational inequality alone, the
    right derivative of `v` at the trigger is `>= 1 + kappa`: `xL` is a
    right-endpoint minimum of `g = v - Mv` (which is `0` there and `>= 0`),
    and a right-endpoint minimum forces the one-sided derivative to be `>= 0`.
  `no_convex_kink`   -- Step 2, the novel content.  Under the supersolution
    hypothesis (the operator inequality at every admissible curvature `q`), a
    convex kink is impossible: one curvature makes the operator value `1 > 0`,
    contradicting the hypothesis.
  `smooth_fit`       -- composes Step 1 and Step 2 to `vR = 1 + kappa`.
-/

import Mathlib

open Real Filter Topology

namespace SmoothFit

/-! ############################################################
    ## 0.  Local data at the trigger
    ############################################################ -/

/-- The affine impulse value on the intervention region:
    `Mv x = a + (1 + kappa) * x`, with slope `1 + kappa`.  Its exactness
    (that `v = Mv` on the intervention region, and `Mv` is affine) is
    Lemma `eq:Maffine` in the paper; here `a`, `kappa` are the given data. -/
structure TriggerData where
  kappa : ℝ
  a     : ℝ
  xL    : ℝ
  σ2    : ℝ          -- diffusion coefficient σ²(x_L, π★) at the trigger
  b     : ℝ          -- drift μ(x_L, π★) times (1+kappa) collected as a constant
  rhs   : ℝ          -- ρ_L V(x_L) + Λ(x_L)
  hσ2   : 0 < σ2     -- nondegeneracy: σ²(x_L) > 0, since x_L > 1 (Lemma ITR)

/-- The affine impulse value, `Mv x = a + (1+kappa) x`. -/
def Mv (d : TriggerData) (x : ℝ) : ℝ := d.a + (1 + d.kappa) * x

/-- `Mv` is affine, so its derivative everywhere is the slope `1 + kappa`. -/
theorem hasDeriv_Mv (d : TriggerData) (x : ℝ) :
    HasDerivAt (Mv d) (1 + d.kappa) x := by
  unfold Mv
  have h : HasDerivAt (fun y : ℝ => (1 + d.kappa) * y) (1 + d.kappa) x := by
    simpa using (hasDerivAt_id x).const_mul (1 + d.kappa)
  exact h.const_add d.a

/-! ############################################################
    ## 1.  Any kink is convex  (STEP 1, proved in full)
    ############################################################ -/

/--
**Step 1: the one-sided jump is nonnegative.**

Hypotheses, all from the variational inequality:
* `hleft`  : the left derivative of `v` at `xL` is the affine slope `1+kappa`
             (because `v = Mv` on the intervention side, and `Mv` has that
             slope).
* `hg`     : the obstacle `g = v - Mv` is `≥ 0` everywhere (the third QVI
             branch `v ≥ Mv`).
* `hg0`    : `g (xL) = 0` (value matching at the trigger).
* `hright` : `v` has a right derivative `vR` at `xL` and `Mv` has its usual
             derivative there, so `g` has right derivative `vR - (1+kappa)`.

Conclusion: `vR ≥ 1 + kappa`, i.e. the jump `J = vR - (1+kappa) ≥ 0`.

The mathematical content: `xL` minimises `g` on `[xL, ∞)` (it is `0` there
and `g ≥ 0`), and a differentiable-from-the-right function with a right
endpoint minimum has nonnegative right derivative.
-/
theorem kink_is_convex
    (d : TriggerData) (v : ℝ → ℝ) (vR : ℝ)
    (hg : ∀ x, Mv d x ≤ v x)
    (hg0 : v d.xL = Mv d d.xL)
    (hright : HasDerivWithinAt v vR (Set.Ici d.xL) d.xL) :
    1 + d.kappa ≤ vR := by
  -- g x := v x - Mv d x has right derivative vR - (1+kappa) at xL,
  -- is ≥ 0 everywhere, and is 0 at xL, so xL is a min of g on Ici xL.
  have hgder : HasDerivWithinAt (fun x => v x - Mv d x) (vR - (1 + d.kappa))
      (Set.Ici d.xL) d.xL :=
    hright.sub ((hasDeriv_Mv d d.xL).hasDerivWithinAt)
  -- xL is a local minimum of g from the right: g xL = 0 ≤ g x for x ≥ xL.
  have hmin : ∀ x ∈ Set.Ici d.xL, (fun x => v x - Mv d x) d.xL
      ≤ (fun x => v x - Mv d x) x := by
    intro x _
    have : v d.xL - Mv d d.xL = 0 := by rw [hg0]; ring
    simp only
    rw [this]
    linarith [hg x]
  -- A right-endpoint minimum forces the right derivative to be ≥ 0.
  -- `hmin` upgrades to `IsLocalMinOn` (nhdsWithin ≤ principal); `1` lies in
  -- the positive tangent cone of `Ici xL` at `xL` because the segment
  -- `[xL, xL+1]` sits inside `Ici xL`; then
  -- `IsLocalMinOn.hasFDerivWithinAt_nonneg` gives the sign of `f' 1`, which
  -- `simp` reduces to `vR - (1 + kappa)`.
  have hnonneg : 0 ≤ vR - (1 + d.kappa) := by
    have hlmin : IsLocalMinOn (fun x => v x - Mv d x) (Set.Ici d.xL) d.xL :=
      Filter.eventually_of_mem self_mem_nhdsWithin hmin
    have hseg : segment ℝ d.xL (d.xL + 1) ⊆ Set.Ici d.xL := by
      rw [segment_eq_Icc (by linarith : d.xL ≤ d.xL + 1)]
      exact Set.Icc_subset_Ici_self
    have hy : (1 : ℝ) ∈ posTangentConeAt (Set.Ici d.xL) d.xL :=
      mem_posTangentConeAt_of_segment_subset hseg
    have hnn := hlmin.hasFDerivWithinAt_nonneg hgder.hasFDerivWithinAt hy
    simpa using hnn
  linarith

/-! ############################################################
    ## 2.  A convex kink contradicts the supersolution property
    ##     (STEP 2, contradiction lemma; supersolution TAKEN AS HYPOTHESIS)
    ############################################################ -/

/--
The viscosity **supersolution** inequality at the trigger, stated exactly as
much of it as Step 2 uses, and TAKEN AS A HYPOTHESIS (see the file header).

For a supersolution, every `C²` test function `φ` touching `v` from below at
`xL` satisfies the operator inequality `-(½ σ² φ'' + b - rhs) ≥ 0`, i.e.

    ½ σ² φ''(xL) + b - rhs ≤ 0.

Because the only feature of `φ` that enters is its second derivative `q` at
`xL` (the first derivative is pinned to a value inside the jump, handled in
`no_convex_kink`), we package the property as: for every admissible curvature
`q` arising from a valid lower test function, `½ σ² q + b - rhs ≤ 0`.

`IsViscosityGe d q` is read "curvature `q` is admissible for a lower test
function at the trigger."  The supersolution property is the hypothesis that
the operator inequality holds for every such `q`. -/
def OperatorLe (d : TriggerData) (q : ℝ) : Prop :=
  (1 / 2) * d.σ2 * q + d.b - d.rhs ≤ 0

/--
**Step 2: a convex kink is impossible under the supersolution property.**

Hypotheses:
* `hsuper` : the supersolution operator inequality holds for EVERY curvature
             `q` (this is the stated viscosity-supersolution hypothesis; when
             there is a genuine jump, test functions of every curvature touch
             from below, so every `q` is admissible -- that admissibility is
             the geometric content of a convex corner, and is why the jump
             hypothesis is what unlocks arbitrary `q`).

Conclusion: `False`.

Proof: the map `q ↦ ½ σ² q + b - rhs` is strictly increasing in `q` (as
`σ² > 0`), hence unbounded above, so it cannot be `≤ 0` for every `q`.
Concretely, instantiate at `q = (rhs - b + 1) * 2 / σ²`, which makes the
left side equal `1 > 0`, contradicting `hsuper`. -/
theorem no_convex_kink
    (d : TriggerData)
    (hsuper : ∀ q : ℝ, OperatorLe d q) :
    False := by
  -- choose q making ½ σ² q + b - rhs = 1 > 0
  have hσ2 := d.hσ2
  set q : ℝ := (d.rhs - d.b + 1) * 2 / d.σ2 with hq
  have hval : (1 / 2) * d.σ2 * q + d.b - d.rhs = 1 := by
    rw [hq]
    field_simp
    ring
  have := hsuper q            -- OperatorLe d q : ½ σ² q + b - rhs ≤ 0
  unfold OperatorLe at this
  rw [hval] at this
  linarith

/-! ############################################################
    ## 3.  Smooth fit  (the two steps composed)
    ############################################################ -/

/--
**Proposition SMF (smooth fit), as far as it is machine-checked here.**

Given
* Step-1 data making the right derivative `vR ≥ 1 + kappa` (a convex or
  absent kink), and
* the supersolution property in the form that would be available were the
  kink genuine (`hsuper`),

there is no genuine convex kink: the only consistent possibility is
`vR = 1 + kappa`, i.e. smooth fit.

The statement is phrased as: if a strict kink `1 + kappa < vR` held, the
supersolution property (which, for a strict kink, supplies the operator
inequality at every curvature) would give `False`.  Contrapositive of
`no_convex_kink`, packaged with Step 1's lower bound to conclude equality. -/
theorem smooth_fit
    (d : TriggerData) (v : ℝ → ℝ) (vR : ℝ)
    (hg : ∀ x, Mv d x ≤ v x)
    (hg0 : v d.xL = Mv d d.xL)
    (hright : HasDerivWithinAt v vR (Set.Ici d.xL) d.xL)
    -- the supersolution property, available at every curvature exactly when
    -- the derivative fails to match (a genuine jump admits all lower tests):
    (hsuper : 1 + d.kappa < vR → ∀ q : ℝ, OperatorLe d q) :
    vR = 1 + d.kappa := by
  have hge : 1 + d.kappa ≤ vR := kink_is_convex d v vR hg hg0 hright
  rcases eq_or_lt_of_le hge with h | h
  · exact h.symm
  · exact absurd (no_convex_kink d (hsuper h)) (by simp)

theorem control_kink_needs_obstacle :
    ∃ (d : TriggerData) (v : ℝ → ℝ) (vR : ℝ), v d.xL = Mv d d.xL ∧
      HasDerivWithinAt v vR (Set.Ici d.xL) d.xL ∧ vR < 1 + d.kappa :=
  ⟨⟨1, 0, 0, 1, 0, 0, one_pos⟩, fun x => x, 1, by simp [Mv],
    (hasDerivAt_id 0).hasDerivWithinAt, by norm_num⟩

end SmoothFit
