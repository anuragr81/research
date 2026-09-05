import Mathlib

-- The polynomial defs below have ~84 terms each; elaborating them over a generic
-- CommRing exceeds the default 200k-heartbeat cap. Lift it (the `ring` proof
-- itself is fast once the terms are built).
set_option maxHeartbeats 2000000
set_option maxRecDepth 4000
/-
  Paper B — locality is the switch for association-immunity.

  Setting: two binary attributes A, B. A prior joint law on the four cells is
  parametrised by marginals a = P(A=1), b = P(B=1) and an association parameter
  c, with cells
      P11 = a*b + c,  P10 = a*(1-b) - c,  P01 = (1-a)*b - c,  P00 = (1-a)*(1-b) + c.
  The cross-attribute association is  assoc = P11*P00 - P10*P01  (= c on the prior).

  Two ATTRIBUTE-LOCAL Jeffrey cues are applied: an A-cue resetting the A-marginal
  to tA (rescaling the A-rows), and a B-cue resetting the B-marginal to tB
  (rescaling the B-columns). Route AB applies A then B; route BA applies B then A.

  The order-gap in the resulting association, cleared of its (nonzero) Jeffrey
  normalisers, is the polynomial `gapNum` below. The theorem states that this
  numerator is divisible by c^2: i.e. the association order-effect between the two
  reading orders vanishes to first order in c, for a GENERAL prior (a, b) and
  ARBITRARY target marginals (tA, tB). This is the local half of the
  locality-is-the-switch result; the non-local half fails this (first-order
  coefficient generically nonzero) and is witnessed numerically, not here.

  Proof is a pure polynomial identity: gapNum = c^2 * gapQuot, closed by `ring`.
-/

variable {F : Type*} [CommRing F]

/-- Numerator of the association order-gap for two attribute-local Jeffrey cues,
    after clearing the nonzero Jeffrey normalisers. -/
def gapNum (a b c tA tB : F) : F :=
  -2*a^3*b^2*c^2*tA^2*tB^3 + 3*a^3*b^2*c^2*tA^2*tB^2 - a^3*b^2*c^2*tA^2*tB + 2*a^3*b^2*c^2*tA*tB^3 - 3*a^3*b^2*c^2*tA*tB^2 + a^3*b^2*c^2*tA*tB + 2*a^3*b*c^2*tA^2*tB^3 - 3*a^3*b*c^2*tA^2*tB^2 + a^3*b*c^2*tA^2*tB - 2*a^3*b*c^2*tA*tB^3 + 3*a^3*b*c^2*tA*tB^2 - a^3*b*c^2*tA*tB + 2*a^2*b^3*c^2*tA^3*tB^2 - 2*a^2*b^3*c^2*tA^3*tB - 3*a^2*b^3*c^2*tA^2*tB^2 + 3*a^2*b^3*c^2*tA^2*tB + a^2*b^3*c^2*tA*tB^2 - a^2*b^3*c^2*tA*tB - 3*a^2*b^2*c^2*tA^3*tB^2 + 3*a^2*b^2*c^2*tA^3*tB + 3*a^2*b^2*c^2*tA^2*tB^3 - 3*a^2*b^2*c^2*tA^2*tB - 3*a^2*b^2*c^2*tA*tB^3 + 3*a^2*b^2*c^2*tA*tB^2 - 2*a^2*b*c^3*tA^2*tB^3 + 3*a^2*b*c^3*tA^2*tB^2 - a^2*b*c^3*tA^2*tB + 2*a^2*b*c^3*tA*tB^3 - 3*a^2*b*c^3*tA*tB^2 + a^2*b*c^3*tA*tB + a^2*b*c^2*tA^3*tB^2 - a^2*b*c^2*tA^3*tB - 3*a^2*b*c^2*tA^2*tB^3 + 3*a^2*b*c^2*tA^2*tB^2 + 3*a^2*b*c^2*tA*tB^3 - 4*a^2*b*c^2*tA*tB^2 + a^2*b*c^2*tA*tB + a^2*c^3*tA^2*tB^4 - a^2*c^3*tA^2*tB^3 - a^2*c^3*tA*tB^4 + a^2*c^3*tA*tB^3 - 2*a*b^3*c^2*tA^3*tB^2 + 2*a*b^3*c^2*tA^3*tB + 3*a*b^3*c^2*tA^2*tB^2 - 3*a*b^3*c^2*tA^2*tB - a*b^3*c^2*tA*tB^2 + a*b^3*c^2*tA*tB + 2*a*b^2*c^3*tA^3*tB^2 - 2*a*b^2*c^3*tA^3*tB - 3*a*b^2*c^3*tA^2*tB^2 + 3*a*b^2*c^3*tA^2*tB + a*b^2*c^3*tA*tB^2 - a*b^2*c^3*tA*tB + 3*a*b^2*c^2*tA^3*tB^2 - 3*a*b^2*c^2*tA^3*tB - a*b^2*c^2*tA^2*tB^3 - 3*a*b^2*c^2*tA^2*tB^2 + 4*a*b^2*c^2*tA^2*tB + a*b^2*c^2*tA*tB^3 - a*b^2*c^2*tA*tB - 2*a*b*c^3*tA^3*tB^2 + 2*a*b*c^3*tA^3*tB + 2*a*b*c^3*tA^2*tB^3 - 2*a*b*c^3*tA^2*tB - 2*a*b*c^3*tA*tB^3 + 2*a*b*c^3*tA*tB^2 - a*b*c^2*tA^3*tB^2 + a*b*c^2*tA^3*tB + a*b*c^2*tA^2*tB^3 - a*b*c^2*tA^2*tB - a*b*c^2*tA*tB^3 + a*b*c^2*tA*tB^2 - a*c^3*tA^2*tB^4 + a*c^3*tA^2*tB^3 + a*c^3*tA*tB^4 - a*c^3*tA*tB^3 - b^2*c^3*tA^4*tB^2 + b^2*c^3*tA^4*tB + b^2*c^3*tA^3*tB^2 - b^2*c^3*tA^3*tB + b*c^3*tA^4*tB^2 - b*c^3*tA^4*tB - b*c^3*tA^3*tB^2 + b*c^3*tA^3*tB

/-- Explicit quotient: gapNum = c^2 * gapQuot. -/
def gapQuot (a b c tA tB : F) : F :=
  -2*a^3*b^2*tA^2*tB^3 + 3*a^3*b^2*tA^2*tB^2 - a^3*b^2*tA^2*tB + 2*a^3*b^2*tA*tB^3 - 3*a^3*b^2*tA*tB^2 + a^3*b^2*tA*tB + 2*a^3*b*tA^2*tB^3 - 3*a^3*b*tA^2*tB^2 + a^3*b*tA^2*tB - 2*a^3*b*tA*tB^3 + 3*a^3*b*tA*tB^2 - a^3*b*tA*tB + 2*a^2*b^3*tA^3*tB^2 - 2*a^2*b^3*tA^3*tB - 3*a^2*b^3*tA^2*tB^2 + 3*a^2*b^3*tA^2*tB + a^2*b^3*tA*tB^2 - a^2*b^3*tA*tB - 3*a^2*b^2*tA^3*tB^2 + 3*a^2*b^2*tA^3*tB + 3*a^2*b^2*tA^2*tB^3 - 3*a^2*b^2*tA^2*tB - 3*a^2*b^2*tA*tB^3 + 3*a^2*b^2*tA*tB^2 - 2*a^2*b*c*tA^2*tB^3 + 3*a^2*b*c*tA^2*tB^2 - a^2*b*c*tA^2*tB + 2*a^2*b*c*tA*tB^3 - 3*a^2*b*c*tA*tB^2 + a^2*b*c*tA*tB + a^2*b*tA^3*tB^2 - a^2*b*tA^3*tB - 3*a^2*b*tA^2*tB^3 + 3*a^2*b*tA^2*tB^2 + 3*a^2*b*tA*tB^3 - 4*a^2*b*tA*tB^2 + a^2*b*tA*tB + a^2*c*tA^2*tB^4 - a^2*c*tA^2*tB^3 - a^2*c*tA*tB^4 + a^2*c*tA*tB^3 - 2*a*b^3*tA^3*tB^2 + 2*a*b^3*tA^3*tB + 3*a*b^3*tA^2*tB^2 - 3*a*b^3*tA^2*tB - a*b^3*tA*tB^2 + a*b^3*tA*tB + 2*a*b^2*c*tA^3*tB^2 - 2*a*b^2*c*tA^3*tB - 3*a*b^2*c*tA^2*tB^2 + 3*a*b^2*c*tA^2*tB + a*b^2*c*tA*tB^2 - a*b^2*c*tA*tB + 3*a*b^2*tA^3*tB^2 - 3*a*b^2*tA^3*tB - a*b^2*tA^2*tB^3 - 3*a*b^2*tA^2*tB^2 + 4*a*b^2*tA^2*tB + a*b^2*tA*tB^3 - a*b^2*tA*tB - 2*a*b*c*tA^3*tB^2 + 2*a*b*c*tA^3*tB + 2*a*b*c*tA^2*tB^3 - 2*a*b*c*tA^2*tB - 2*a*b*c*tA*tB^3 + 2*a*b*c*tA*tB^2 - a*b*tA^3*tB^2 + a*b*tA^3*tB + a*b*tA^2*tB^3 - a*b*tA^2*tB - a*b*tA*tB^3 + a*b*tA*tB^2 - a*c*tA^2*tB^4 + a*c*tA^2*tB^3 + a*c*tA*tB^4 - a*c*tA*tB^3 - b^2*c*tA^4*tB^2 + b^2*c*tA^4*tB + b^2*c*tA^3*tB^2 - b^2*c*tA^3*tB + b*c*tA^4*tB^2 - b*c*tA^4*tB - b*c*tA^3*tB^2 + b*c*tA^3*tB

/-- The association order-gap numerator is divisible by `c^2`:
    the sequence effect on the cross-attribute association is second order in `c`,
    for any prior `(a,b)` and any target marginals `(tA,tB)`. -/
theorem assoc_gap_second_order (a b c tA tB : F) :
    gapNum a b c tA tB = c ^ 2 * gapQuot a b c tA tB := by
  unfold gapNum gapQuot
  ring

/-- Consequently, `c = 0` is at least a double root: the gap and its formal
    first-order part both vanish. Stated as: gapNum factors through `c^2`. -/
theorem assoc_gap_has_c_sq_factor (a b c tA tB : F) :
    ∃ R : F, gapNum a b c tA tB = c ^ 2 * R :=
  ⟨gapQuot a b c tA tB, assoc_gap_second_order a b c tA tB⟩
