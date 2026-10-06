/-
  Hopkins & Kornienko: the dispersive order and the pivot-spread hypothesis.

  LITERATURE.tex (section "Hopkins and Kornienko", Point 4) argues that the
  pivot-spread class of P9-gen is not an ad hoc restriction invented to make
  the proof work, but is -- up to a crossing condition -- the standard
  dispersion order of this literature. It states outright that this "should be
  checked formally and then stated". This file is that check.

  Their Definition 1 (Hopkins-Kornienko 2010, AEJ:Micro 2(3), p.128, quoting
  Shaked & Shanthikumar 2007, p.148): F is smaller in the dispersive order
  than G, written F <=d G, whenever G^{-1}(r) - F^{-1}(r) is weakly increasing
  for r in (0,1).

  Writing T = G^{-1} o F for the map carrying each rank's F-quantile to the
  same rank's G-quantile, that condition says exactly:

      w |-> T w - w   is weakly increasing.

  which is `Dispersive` below. Everything here is order theory over `Int`;
  no measure theory and no Mathlib. The quantile functions themselves, and
  the fact that T is the quantile transform, are NOT formalised -- they are
  the analytic input, assumed, in the same style as `step_nonpos` in
  EntryContest.lean.

  `PivotSpread` is restated here in the shape it has in EntryContest.lean,
  instantiated at `Int` with `<=`, so the two agree by inspection. This file
  is standalone and does not import that development.
-/

namespace HopkinsKornienko

/-- The displacement of the quantile map. -/
def disp (T : Int → Int) (w : Int) : Int := T w - w

/-- **Definition 1, transported.** The dispersive order says the displacement
    is weakly increasing. -/
def Dispersive (T : Int → Int) : Prop := ∀ a b, a ≤ b → disp T a ≤ disp T b

/-- `PivotSpread`, in the shape used in EntryContest.lean, at `Int` with `<=`. -/
def PivotSpread (T : Int → Int) (x0 : Int) : Prop :=
  (∀ w, x0 ≤ w → w ≤ T w) ∧ (∀ w, w ≤ x0 → T w ≤ w)

/-- `MonotoneT`, likewise. -/
def MonotoneT (T : Int → Int) : Prop := ∀ a b, a ≤ b → T a ≤ T b

/-- **HK-L1. The dispersive order already forces `T` to be monotone.**
    P9-gen carries `MonotoneT` as a separate hypothesis; under the dispersive
    order it is not an extra assumption but a consequence, since
    `T b - b >= T a - a` and `b >= a` together give `T b >= T a`. -/
theorem dispersive_imp_monotone (T : Int → Int) (h : Dispersive T) :
    MonotoneT T := by
  intro a b hab
  have := h a b hab
  unfold disp at this
  omega

/-- **HK-L2. A dispersive map with a zero of the displacement is a
    pivot-spread about that zero.** This is the bridge claimed in
    LITERATURE.tex: dispersive order plus a crossing gives exactly the
    hypothesis P9-gen requires. -/
theorem dispersive_crossing_imp_pivot
    (T : Int → Int) (x0 : Int)
    (h : Dispersive T) (hx0 : T x0 = x0) :
    PivotSpread T x0 := by
  constructor
  · intro w hw
    have := h x0 w hw
    unfold disp at this
    omega
  · intro w hw
    have := h w x0 hw
    unfold disp at this
    omega

/-- **HK-L3. The sign change happens at most once.** Once the displacement is
    non-negative it stays non-negative, so a dispersive map cannot cross back.
    This is the content of "if in addition `T w - w` changes sign, it does so
    once". -/
theorem dispersive_no_return
    (T : Int → Int) (h : Dispersive T)
    (a b : Int) (hab : a ≤ b) (ha : a ≤ T a) : b ≤ T b := by
  have := h a b hab
  unfold disp at this
  omega

/-- Mirror of HK-L3 below the crossing: once the displacement is non-positive
    going down, it stays non-positive. -/
theorem dispersive_no_return_below
    (T : Int → Int) (h : Dispersive T)
    (a b : Int) (hab : a ≤ b) (hb : T b ≤ b) : T a ≤ a := by
  have := h a b hab
  unfold disp at this
  omega

/-- The pure translation `T w = w + 1`. -/
def shiftUp (w : Int) : Int := w + 1

/-- `shiftUp` is dispersive: its displacement is the constant `1`. -/
theorem shiftUp_dispersive : Dispersive shiftUp := by
  intro a b _
  unfold disp shiftUp
  omega

/-- **HK-L4. The crossing hypothesis in HK-L2 is necessary, not decorative.**
    A translation is dispersive and monotone, yet is a pivot-spread about no
    point whatsoever, because its displacement never changes sign. So the
    dispersive order alone does NOT deliver the P9-gen hypothesis, and
    LITERATURE.tex's "if in addition `T w - w` changes sign" is load-bearing.
    Stated as: for every candidate pivot, `PivotSpread` fails. -/
theorem shiftUp_not_pivotSpread (x0 : Int) : ¬ PivotSpread shiftUp x0 := by
  intro hp
  have := hp.2 x0 (by omega)
  unfold shiftUp at this
  omega

/-- **HK-L5. Corollary, the statement LITERATURE.tex wanted.** A dispersive
    map whose displacement changes sign -- non-positive somewhere, and zero at
    `x0` -- satisfies both hypotheses P9-gen needs, `MonotoneT` and
    `PivotSpread`, with no further restriction. -/
theorem dispersive_crossing_gives_P9gen_hypotheses
    (T : Int → Int) (x0 : Int)
    (h : Dispersive T) (hx0 : T x0 = x0) :
    MonotoneT T ∧ PivotSpread T x0 :=
  ⟨dispersive_imp_monotone T h, dispersive_crossing_imp_pivot T x0 h hx0⟩

end HopkinsKornienko
