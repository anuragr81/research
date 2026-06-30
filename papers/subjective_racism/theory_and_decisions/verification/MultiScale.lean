/-
  MultiScale.lean -- the two EXACT coincidences underpinning the multi-scale theorem.
  Lean 4 + Mathlib.  COMPILES (no errors, no `sorry`).

  Companion to SoftMarkers.lean (which formalises the order-dependence direction). Here:
    Theorem 1 (single cue):  Jeffrey = Bayes, exactly -- any correlation, any impression.
    Theorem 2 (independence, c=0):  the two Jeffrey orders coincide and equal q (x) r = Bayes.
  These are parts (1) and (2) of the multi-scale theorem. Parts (3) [O(c) belief divergence]
  and (4) [O(c^2) welfare loss] are leading-order / numerical and stay in SymPy by design
  (multiscale_theorem_verification.py) -- forcing them into Lean would be contrived.

  Model. Joint prior on two binary attributes A,B: p_ij = P(A=i, B=j). A cue's IMPRESSION is
  a target marginal; Jeffrey on A to q:  q_i * p_ij / (p_i0+p_i1), conditionals on A rigid.
  The matched single-cue Bayes likelihood is l_i = q_i / (p_i0+p_i1).
-/
import Mathlib

namespace MultiScale

/-! ## Theorem 1 — single cue: Jeffrey = Bayes, exactly (any correlation, any impression) -/

/-- The matched-likelihood Bayes normaliser is 1 for a normalised impression `q0+q1=1`. -/
theorem matched_normaliser_eq_one
    (p00 p01 p10 p11 q0 q1 : ℝ)
    (hq : q0 + q1 = 1) (hA0 : p00 + p01 ≠ 0) (hA1 : p10 + p11 ≠ 0) :
    p00 * (q0/(p00+p01)) + p01 * (q0/(p00+p01))
      + p10 * (q1/(p10+p11)) + p11 * (q1/(p10+p11)) = 1 := by
  have e : p00 * (q0/(p00+p01)) + p01 * (q0/(p00+p01))
            + p10 * (q1/(p10+p11)) + p11 * (q1/(p10+p11)) = q0 + q1 := by
    field_simp; ring
  rw [e, hq]

/-- Single cue: the matched-likelihood Bayes posterior equals the Jeffrey update.
Shown for cell (0,0); the other three cells are identical by the same computation
(the normaliser being 1 is what makes Bayes collapse onto Jeffrey). -/
theorem single_cue_coincidence
    (p00 p01 p10 p11 q0 q1 : ℝ)
    (hq : q0 + q1 = 1) (hA0 : p00 + p01 ≠ 0) (hA1 : p10 + p11 ≠ 0) :
    p00 * (q0/(p00+p01))
      / (p00 * (q0/(p00+p01)) + p01 * (q0/(p00+p01))
          + p10 * (q1/(p10+p11)) + p11 * (q1/(p10+p11)))
    = q0 * p00 / (p00+p01) := by
  rw [matched_normaliser_eq_one p00 p01 p10 p11 q0 q1 hq hA0 hA1, div_one]
  ring

/-! ## Theorem 2 — independence (c=0): both Jeffrey orders give q ⊗ r = Bayes.
Independent prior `p_ij = a_i b_j` (zero covariance). -/

/-- Jeffrey order A-then-B, cell (0,0), under an independent prior, equals `q0*r0`. -/
theorem independence_AthenB_cell00
    (a0 a1 b0 b1 q0 q1 r0 : ℝ)
    (hb : b0 + b1 = 1) (hq : q0 + q1 = 1)
    (ha0 : a0 ≠ 0) (ha1 : a1 ≠ 0) (hb0 : b0 ≠ 0) :
    r0 * (q0*(a0*b0)/(a0*b0 + a0*b1))
      / (q0*(a0*b0)/(a0*b0 + a0*b1) + q1*(a1*b0)/(a1*b0 + a1*b1))
    = q0 * r0 := by
  -- A-conditioned normalisers collapse: a0*b0+a0*b1 = a0, a1*b0+a1*b1 = a1
  have hA0 : a0*b0 + a0*b1 = a0 := by rw [← mul_add, hb, mul_one]
  have hA1 : a1*b0 + a1*b1 = a1 := by rw [← mul_add, hb, mul_one]
  rw [hA0, hA1]
  -- the once-updated cells: q0*(a0*b0)/a0 = q0*b0,  q1*(a1*b0)/a1 = q1*b0
  have j00 : q0*(a0*b0)/a0 = q0*b0 := by field_simp
  have j10 : q1*(a1*b0)/a1 = q1*b0 := by field_simp
  rw [j00, j10]
  -- the B-update normaliser: q0*b0 + q1*b0 = b0
  have hden : q0*b0 + q1*b0 = b0 := by rw [← add_mul, hq, one_mul]
  rw [hden]
  field_simp

/-- Jeffrey order B-then-A, cell (0,0), under an independent prior, equals `q0*r0`. -/
theorem independence_BthenA_cell00
    (a0 a1 b0 b1 q0 r0 r1 : ℝ)
    (ha : a0 + a1 = 1) (hr : r0 + r1 = 1)
    (hb0 : b0 ≠ 0) (hb1 : b1 ≠ 0) (ha0 : a0 ≠ 0) :
    q0 * (r0*(a0*b0)/(a0*b0 + a1*b0))
      / (r0*(a0*b0)/(a0*b0 + a1*b0) + r1*(a0*b1)/(a0*b1 + a1*b1))
    = q0 * r0 := by
  have hB0 : a0*b0 + a1*b0 = b0 := by rw [← add_mul, ha, one_mul]
  have hB1 : a0*b1 + a1*b1 = b1 := by rw [← add_mul, ha, one_mul]
  rw [hB0, hB1]
  have j00 : r0*(a0*b0)/b0 = r0*a0 := by field_simp
  have j01 : r1*(a0*b1)/b1 = r1*a0 := by field_simp
  rw [j00, j01]
  have hden : r0*a0 + r1*a0 = a0 := by rw [← add_mul, hr, one_mul]
  rw [hden]
  field_simp

/-- **c = 0 ⇒ the two Jeffrey orders coincide** (both equal `q0*r0`), i.e. order-independence
and Bayes-coincidence under independent cues. Cell (0,0); the others are identical. -/
theorem independence_orders_coincide
    (a0 a1 b0 b1 q0 q1 r0 r1 : ℝ)
    (ha : a0 + a1 = 1) (hb : b0 + b1 = 1) (hq : q0 + q1 = 1) (hr : r0 + r1 = 1)
    (ha0 : a0 ≠ 0) (ha1 : a1 ≠ 0) (hb0 : b0 ≠ 0) (hb1 : b1 ≠ 0) :
    r0 * (q0*(a0*b0)/(a0*b0 + a0*b1))
      / (q0*(a0*b0)/(a0*b0 + a0*b1) + q1*(a1*b0)/(a1*b0 + a1*b1))
    =
    q0 * (r0*(a0*b0)/(a0*b0 + a1*b0))
      / (r0*(a0*b0)/(a0*b0 + a1*b0) + r1*(a0*b1)/(a0*b1 + a1*b1)) := by
  rw [independence_AthenB_cell00 a0 a1 b0 b1 q0 q1 r0 hb hq ha0 ha1 hb0,
      independence_BthenA_cell00 a0 a1 b0 b1 q0 r0 r1 ha hr hb0 hb1 ha0]

end MultiScale
