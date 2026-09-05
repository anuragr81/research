import Mathlib

set_option maxHeartbeats 2000000
set_option maxRecDepth 8000

/-
  Paper B — structural version: the Jeffrey updates are DEFINED here and the
  association order-gap is DERIVED by Lean, closing the seam left by the
  literal-polynomial version (AssocLocality.lean), where `gapNum` was supplied
  by computer algebra and only its divisibility by c^2 was machine-checked.

  Prior cells, parametrised by marginals a = P(A=1), b = P(B=1) and association c.
  Two attribute-local Jeffrey cues: the A-cue rescales the A-rows to marginal tA,
  the B-cue rescales the B-columns to marginal tB. Route AB applies A then B;
  route BA applies B then A. `assoc Q = Q11*Q00 - Q10*Q01`.

  Theorem: over a field, with the four Jeffrey normalisers nonzero, the order-gap
  in the association equals c^2 * gapQuot / (D1*D2*D3*D4). The order-gap is
  therefore second order in c for a general prior and arbitrary targets.
-/

variable {K : Type*} [Field K]

namespace PaperB

def p11 (a b c : K) : K := a*b + c
def p10 (a b c : K) : K := a*(1-b) - c
def p01 (a b c : K) : K := (1-a)*b - c
def p00 (a b c : K) : K := (1-a)*(1-b) + c

def assoc (q11 q10 q01 q00 : K) : K := q11*q00 - q10*q01

def D1 (a b c tB : K) : K := a*b^2 - a*b - b^2 + b*c + b - c*tB
def D2 (a b c tB : K) : K := a*b^2 - a*b + b*c - c*tB
def D3 (a b c tA : K) : K := a^2*b - a*b + a*c - c*tA
def D4 (a b c tA : K) : K := a^2*b - a^2 - a*b + a*c + a - c*tA

def gapQuot (a b c tA tB : K) : K :=
  -2*a^3*b^2*tA^2*tB^3 + 3*a^3*b^2*tA^2*tB^2 - a^3*b^2*tA^2*tB + 2*a^3*b^2*tA*tB^3 - 3*a^3*b^2*tA*tB^2 + a^3*b^2*tA*tB + 2*a^3*b*tA^2*tB^3 - 3*a^3*b*tA^2*tB^2 + a^3*b*tA^2*tB - 2*a^3*b*tA*tB^3 + 3*a^3*b*tA*tB^2 - a^3*b*tA*tB + 2*a^2*b^3*tA^3*tB^2 - 2*a^2*b^3*tA^3*tB - 3*a^2*b^3*tA^2*tB^2 + 3*a^2*b^3*tA^2*tB + a^2*b^3*tA*tB^2 - a^2*b^3*tA*tB - 3*a^2*b^2*tA^3*tB^2 + 3*a^2*b^2*tA^3*tB + 3*a^2*b^2*tA^2*tB^3 - 3*a^2*b^2*tA^2*tB - 3*a^2*b^2*tA*tB^3 + 3*a^2*b^2*tA*tB^2 - 2*a^2*b*c*tA^2*tB^3 + 3*a^2*b*c*tA^2*tB^2 - a^2*b*c*tA^2*tB + 2*a^2*b*c*tA*tB^3 - 3*a^2*b*c*tA*tB^2 + a^2*b*c*tA*tB + a^2*b*tA^3*tB^2 - a^2*b*tA^3*tB - 3*a^2*b*tA^2*tB^3 + 3*a^2*b*tA^2*tB^2 + 3*a^2*b*tA*tB^3 - 4*a^2*b*tA*tB^2 + a^2*b*tA*tB + a^2*c*tA^2*tB^4 - a^2*c*tA^2*tB^3 - a^2*c*tA*tB^4 + a^2*c*tA*tB^3 - 2*a*b^3*tA^3*tB^2 + 2*a*b^3*tA^3*tB + 3*a*b^3*tA^2*tB^2 - 3*a*b^3*tA^2*tB - a*b^3*tA*tB^2 + a*b^3*tA*tB + 2*a*b^2*c*tA^3*tB^2 - 2*a*b^2*c*tA^3*tB - 3*a*b^2*c*tA^2*tB^2 + 3*a*b^2*c*tA^2*tB + a*b^2*c*tA*tB^2 - a*b^2*c*tA*tB + 3*a*b^2*tA^3*tB^2 - 3*a*b^2*tA^3*tB - a*b^2*tA^2*tB^3 - 3*a*b^2*tA^2*tB^2 + 4*a*b^2*tA^2*tB + a*b^2*tA*tB^3 - a*b^2*tA*tB - 2*a*b*c*tA^3*tB^2 + 2*a*b*c*tA^3*tB + 2*a*b*c*tA^2*tB^3 - 2*a*b*c*tA^2*tB - 2*a*b*c*tA*tB^3 + 2*a*b*c*tA*tB^2 - a*b*tA^3*tB^2 + a*b*tA^3*tB + a*b*tA^2*tB^3 - a*b*tA^2*tB - a*b*tA*tB^3 + a*b*tA*tB^2 - a*c*tA^2*tB^4 + a*c*tA^2*tB^3 + a*c*tA*tB^4 - a*c*tA*tB^3 - b^2*c*tA^4*tB^2 + b^2*c*tA^4*tB + b^2*c*tA^3*tB^2 - b^2*c*tA^3*tB + b*c*tA^4*tB^2 - b*c*tA^4*tB - b*c*tA^3*tB^2 + b*c*tA^3*tB

/-- Route AB: A-cue (rows to `tA`) then B-cue (columns to `tB`). -/
noncomputable def routeAB (a b c tA tB : K) : K × K × K × K :=
  let r11 := tA * p11 a b c / a
  let r10 := tA * p10 a b c / a
  let r01 := (1-tA) * p01 a b c / (1-a)
  let r00 := (1-tA) * p00 a b c / (1-a)
  let cB  := r11 + r01
  let cBc := r10 + r00
  (tB * r11 / cB, (1-tB) * r10 / cBc, tB * r01 / cB, (1-tB) * r00 / cBc)

/-- Route BA: B-cue (columns to `tB`) then A-cue (rows to `tA`). -/
noncomputable def routeBA (a b c tA tB : K) : K × K × K × K :=
  let s11 := tB * p11 a b c / b
  let s01 := tB * p01 a b c / b
  let s10 := (1-tB) * p10 a b c / (1-b)
  let s00 := (1-tB) * p00 a b c / (1-b)
  let rA  := s11 + s10
  let rAc := s01 + s00
  (tA * s11 / rA, tA * s10 / rA, (1-tA) * s01 / rAc, (1-tA) * s00 / rAc)

/-- The association order-gap, derived from the update definitions, is
    `c^2 * gapQuot / (D1*D2*D3*D4)` — hence second order in `c`. -/
theorem assoc_order_gap_structural
    (a b c tA tB : K)
    (ha : a ≠ 0) (ha' : (1:K) - a ≠ 0)
    (hb : b ≠ 0) (hb' : (1:K) - b ≠ 0)
    (h1 : D1 a b c tB ≠ 0) (h2 : D2 a b c tB ≠ 0)
    (h3 : D3 a b c tA ≠ 0) (h4 : D4 a b c tA ≠ 0) :
    (match routeAB a b c tA tB with | (q11,q10,q01,q00) => assoc q11 q10 q01 q00)
      - (match routeBA a b c tA tB with | (q11,q10,q01,q00) => assoc q11 q10 q01 q00)
    = c^2 * gapQuot a b c tA tB / (D1 a b c tB * D2 a b c tB * D3 a b c tA * D4 a b c tA) := by
  -- Reduce the tuple-matches and the `let`-bound normalisers to explicit
  -- fractions (the four compound column/row masses cB, cBc, rA, rAc).
  simp only [routeAB, routeBA, assoc]
  -- Each compound mass is `±Dᵢ / (marginal·(1-marginal))`, so it is nonzero
  -- exactly under the stated hypotheses.  Rewriting to the `Dᵢ` closed form
  -- lets `field_simp` clear the outer divisions using `h1..h4` and `ha..hb'`.
  rw [show tA * p11 a b c / a + (1 - tA) * p01 a b c / (1 - a)
        = -D3 a b c tA / (a * (1 - a)) from by unfold p11 p01 D3; field_simp; ring,
      show tA * p10 a b c / a + (1 - tA) * p00 a b c / (1 - a)
        = D4 a b c tA / (a * (1 - a)) from by unfold p10 p00 D4; field_simp; ring,
      show tB * p11 a b c / b + (1 - tB) * p10 a b c / (1 - b)
        = -D2 a b c tB / (b * (1 - b)) from by unfold p11 p10 D2; field_simp; ring,
      show tB * p01 a b c / b + (1 - tB) * p00 a b c / (1 - b)
        = D1 a b c tB / (b * (1 - b)) from by unfold p01 p00 D1; field_simp; ring]
  -- Clear the outer divisions by the compound masses (now in `Dᵢ` form) and by
  -- the marginal factors, using the nonzero hypotheses; then expand the
  -- definitions and finish with a polynomial identity.
  field_simp [ha, ha', hb, hb', h1, h2, h3, h4]
  unfold p11 p10 p01 p00 gapQuot D1 D2 D3 D4
  ring

end PaperB
