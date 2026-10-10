/-
================================================================================
  Asymmetric Capital Buffer Control -- Phase 2, Part 1
  Companion to qvi_formulation.tex and verify_qvi_part1.py.  Lean 4 + Mathlib.
================================================================================

SCOPE.  This covers ONLY the two facts that are actually proven at this
stage of the project: the exact nesting of the risk-neutral benchmark at
lambda = 1, and concavity of the running-payoff term -Lambda.  It does
NOT attempt the first-order conditions (heuristic, pending a verification
theorem), the concavity of the value function V itself (stated as an open
Conjecture in qvi_formulation.tex, not a theorem), or the comparative-
statics prediction (downstream of that open conjecture).  Formalising any
of those now would misrepresent their status -- see the ledger's own
docstring for the same disclosure.

STATUS.  Written without access to a Lean toolchain in this session, same
as every prior Lean file in this project.  NOT YET COMPILE-CHECKED.  Both
theorems verified symbolically first (verify_qvi_part1.py, tags Q1, Q2).

Confidence assessment, stated honestly rather than uniformly:
  `lambda_asymmetry_vanishes_at_one` is pure algebra (substitute lambda=1
    into a product, get 0) -- high confidence, essentially mechanical.
  `neg_Lambda_concave` originally used `h1.min h2`, guessing that
    Mathlib exposes a `ConcaveOn.min` combinator. It does not. Checked
    against Mathlib4 master (Analysis/Convex/Function.lean): the real
    name is `ConcaveOn.inf` (pointwise-min-of-concave-is-concave,
    proved by transporting through `ConvexOn.sup` and `.dual`), and it
    concludes concavity of the lattice-inf `f ⊓ g`, not literally
    `fun x => min x R`. The proof now uses `ConcaveOn.inf` and bridges
    `⊓` to `min` via an isolated `funext x; rfl` step (`hpointwise`),
    on the expectation that Pi's `⊓` and ℝ's `Min` instance reduce to
    `min` definitionally -- this bridging step has NOT been compiled,
    only checked by reading Mathlib's instance definitions
    (Order/Lattice.lean's `Pi.inf_apply`, `Data/Real/Basic.lean`'s
    `Min ℝ` instance), so it remains the one soft spot in this
    theorem. A second guessed name, `ConcaveOn.sub_const`, also turned
    out not to exist -- Mathlib has only `add_const` (and `ConcaveOn.sub`,
    which wants a ConvexOn second argument), so the constant shift now
    goes through `add_const (-R)` plus a second Pi-to-lambda bridge.
    The names now actually checked against Mathlib master are
    `ConcaveOn.inf`, `ConcaveOn.add_const`, `ConcaveOn.smul`,
    `concaveOn_id`, `concaveOn_const`, `convex_univ` and `Pi.add_apply`.
    Note `concaveOn_id` is stated for `_root_.id`, not `fun x => x`;
    these are defeq so the ascription in `h1` should be accepted, but
    that too is a defeq assumption rather than a compiled fact. None of
    this file has been seen by an actual compiler.
-/

import Mathlib

open Real

/-! ############################################################
    ## 1.  Exact nesting at lambda = 1
    ############################################################ -/

/-- The asymmetry penalty. Lambda(x) = (lambda - 1) * (R - x)⁺,
    matching Assumption 1 (asymmetric response) in qvi_formulation.tex. -/
noncomputable def Lambda (lam R x : ℝ) : ℝ := (lam - 1) * max (R - x) 0

/-- **Exact nesting of the risk-neutral benchmark.** At lambda = 1 the
    asymmetry penalty vanishes identically, for every buffer level x and
    every reference R -- not merely in the limit. Corresponds to SymPy
    tag Q1 and to Proposition 1 (exact nesting) in qvi_formulation.tex. -/
theorem lambda_asymmetry_vanishes_at_one (R x : ℝ) :
    Lambda 1 R x = 0 := by
  unfold Lambda
  ring

/-! ############################################################
    ## 2.  Concavity of the running payoff -Lambda
    ############################################################ -/

/-- **-Lambda is concave**, for any fixed lambda ≥ 1 and reference R.
    Corresponds to SymPy tag Q2 and to Proposition 2 in
    qvi_formulation.tex. This is the structural fact that lets the
    standard concave-running-payoff route to concavity of the value
    function remain available -- in contrast to an S-shaped utility,
    which is convex below the reference and forces a concavification
    argument instead (as in Li, Yu & Zhang, arXiv:2108.02648). -/
theorem neg_Lambda_concave {lam R : ℝ} (hlam : 1 ≤ lam) :
    ConcaveOn ℝ Set.univ (fun x => -Lambda lam R x) := by
  -- -Lambda(x) = -(lam-1) * max(R-x, 0) = (lam-1) * min(x-R, 0)
  -- i.e. -Lambda(x) = (lam-1) * (min(x, R) - R), a nonneg-scaled
  -- concave function (min of an affine function and a constant is
  -- concave; scaling by a nonnegative constant preserves concavity).
  -- NB: no `unfold Lambda` here -- it would unfold Lambda in the main
  -- goal too, leaving nothing for `rw [heq]` below to match against.
  have hnonneg : (0:ℝ) ≤ lam - 1 := by linarith
  have heq : (fun x : ℝ => -Lambda lam R x)
      = (fun x : ℝ => (lam - 1) * (min x R - R)) := by
    funext x
    unfold Lambda
    rw [show max (R - x) 0 = -(min x R - R) by
          rcases le_total x R with h | h
          · simp [min_eq_left h, max_eq_left (by linarith : (0:ℝ) ≤ R - x)]
          · simp [min_eq_right h, max_eq_right (by linarith : R - x ≤ 0)]]
    ring
  rw [heq]
  apply ConcaveOn.smul hnonneg
  -- min x R - R is concave: min of an affine function (id) and a
  -- constant function is concave (constants are both concave and
  -- convex; the pointwise min of two concave functions is concave;
  -- affine functions are concave).
  --
  -- Mathlib's combinator for this is `ConcaveOn.inf`, not `.min`
  -- (verified against Mathlib4 master, Analysis/Convex/Function.lean):
  --   theorem ConcaveOn.inf (hf : ConcaveOn 𝕜 s f) (hg : ConcaveOn 𝕜 s g) :
  --       ConcaveOn 𝕜 s (f ⊓ g)
  -- It's proved by transporting through ConvexOn.sup and .dual, so it
  -- produces concavity of the lattice-inf `f ⊓ g`, not literally
  -- `fun x => min x R`. Those are equal (Pi's `⊓` and ℝ's `Min`
  -- instance both reduce to `min` by rfl), but that reduction hasn't
  -- been compile-checked here, so it's isolated into its own line
  -- (`hpointwise`) rather than left implicit -- if `rfl` ever doesn't
  -- go through, only that one line needs fixing (try
  -- `by ext x; simp [Pi.inf_apply]` as a fallback).
  have h1 : ConcaveOn ℝ Set.univ (fun x : ℝ => x) := concaveOn_id (convex_univ)
  have h2 : ConcaveOn ℝ Set.univ (fun _ : ℝ => R) := concaveOn_const R convex_univ
  have hinf : ConcaveOn ℝ Set.univ ((fun x : ℝ => x) ⊓ (fun _ : ℝ => R)) := h1.inf h2
  have hpointwise :
      ((fun x : ℝ => x) ⊓ (fun _ : ℝ => R)) = fun x : ℝ => min x R := by
    funext x; rfl
  rw [hpointwise] at hinf
  -- `ConcaveOn.sub_const` does NOT exist in Mathlib (verified against
  -- master: Analysis/Convex/Function.lean has only `ConcaveOn.add_const`,
  -- and `ConcaveOn.sub`, which needs a ConvexOn second argument). Also
  -- note `add_const` concludes with the *Pi-addition* `f + fun _ => b`,
  -- not `fun x => f x - b`. So: shift by `-R`, then bridge the
  -- Pi-addition back into lambda form, same pattern as `hpointwise`.
  have hshift : ConcaveOn ℝ Set.univ ((fun x : ℝ => min x R) + fun _ : ℝ => -R) :=
    hinf.add_const (-R)
  have hbridge : ((fun x : ℝ => min x R) + fun _ : ℝ => -R)
      = fun x : ℝ => min x R - R := by
    funext x
    simp [Pi.add_apply, sub_eq_add_neg]
  rwa [hbridge] at hshift
