import Mathlib
import LewisThompson

/-!
# Shaked (1982), "Dispersive ordering of distributions"

Moshe Shaked, *Journal of Applied Probability* 19 (1982), 310-320. Locators are printed pages.

`F ≤disp G` means `F⁻¹(β) − F⁻¹(α) ≤ G⁻¹(β) − G⁻¹(α)` for `0 < α < β < 1` (1.1). With quantile
functions `y = F⁻¹` and `z = G⁻¹` on `(0, 1)` this is `LewisThompson.OrdSpacing y z`, which we
reuse together with `LewisThompson.OrdDiff`, `spacing_iff_diff`, `transport_disp` and
`scale_pair`.

Shaked's standing assumption (p.312) is that `F` and `G` are strictly increasing and continuous on
interval supports. We state it as hypotheses on the distribution function and its quantile
function rather than as one package:

* `Monotone F` — a distribution function;
* `StrictMonoOn F (F ⁻¹' Ioo 0 1)` — strictly increasing on its support interval (for a monotone
  `F` the set where `0 < F < 1` is an interval);
* `∀ u ∈ Ioo 0 1, F (y u) = u` — `y` inverts `F` on `(0, 1)`; such a `y` exists exactly when the
  monotone `F` takes every value in `(0, 1)`, which is where continuity enters;
* `∀ x, G x ∈ Icc 0 1` where the values of a distribution function are needed.

Each theorem below takes only the hypotheses its proof uses.
-/

open Set Filter Topology

namespace Shaked1982

open LewisThompson

/-! ## Sign changes, (1.4)

`S⁻(h)` counts the sign changes of `h`, zeros ignored. Theorem 2.1's condition, "`S⁻ ≤ 1`, with
sign sequence `−, +` in case of equality", says that `h` is never negative after it has been
positive. -/

/-- `h` changes sign at most once on `S`, and if once then from `−` to `+`: once positive, never
    negative later. -/
def SignChangeUpOnce (h : ℝ → ℝ) (S : Set ℝ) : Prop :=
  ∀ u1 ∈ S, ∀ u2 ∈ S, u1 < u2 → 0 < h u1 → 0 ≤ h u2

/-- `S⁻(h) ≤ 1` on `S`: no three points at which the signs of `h` strictly alternate. -/
def AtMostOneSignChange (h : ℝ → ℝ) (S : Set ℝ) : Prop :=
  ∀ u1 ∈ S, ∀ u2 ∈ S, ∀ u3 ∈ S, u1 < u2 → u2 < u3 → ¬ (h u1 * h u2 < 0 ∧ h u2 * h u3 < 0)

/-- Every change from `+` to `−` is preceded by a negative value, so the first sign change, if
    there is one, is from `−` to `+`. -/
def FirstChangeUp (h : ℝ → ℝ) (S : Set ℝ) : Prop :=
  ∀ u1 ∈ S, ∀ u2 ∈ S, u1 < u2 → 0 < h u1 → h u2 < 0 → ∃ u0 ∈ S, u0 < u1 ∧ h u0 < 0

/-- **Shaked's wording, checked.** "At most one sign change, and from `−` to `+` if one" is the
    conjunction of `AtMostOneSignChange` and `FirstChangeUp`, and is `SignChangeUpOnce`. -/
theorem signChangeUpOnce_iff (h : ℝ → ℝ) (S : Set ℝ) :
    SignChangeUpOnce h S ↔ AtMostOneSignChange h S ∧ FirstChangeUp h S := by
  constructor
  · intro H
    refine ⟨fun u1 h1 u2 h2 u3 h3 h12 h23 hp => ?_, fun u1 h1 u2 h2 h12 hp hn => ?_⟩
    · obtain ⟨p, q⟩ := hp
      rcases lt_trichotomy (h u2) 0 with hn | hz | hpos
      · have h1pos : 0 < h u1 := by nlinarith
        have := H u1 h1 u2 h2 h12 h1pos
        linarith
      · rw [hz, mul_zero] at p
        exact lt_irrefl _ p
      · have h3neg : h u3 < 0 := by nlinarith
        have := H u2 h2 u3 h3 h23 hpos
        linarith
    · exact absurd (H u1 h1 u2 h2 h12 hp) (not_le.mpr hn)
  · rintro ⟨A, B⟩ u1 h1 u2 h2 h12 hp
    by_contra hn
    push_neg at hn
    obtain ⟨u0, h0, h01, hneg⟩ := B u1 h1 u2 h2 h12 hp hn
    exact A u0 h0 u1 h1 u2 h2 h01 h12 ⟨by nlinarith, by nlinarith⟩

theorem signChangeUpOnce_mono {h : ℝ → ℝ} {S T : Set ℝ} (H : SignChangeUpOnce h S)
    (hTS : T ⊆ S) : SignChangeUpOnce h T :=
  fun u1 h1 u2 h2 => H u1 (hTS h1) u2 (hTS h2)

/-! ## Theorem 2.1, p.312, in quantile form -/

/-- **Theorem 2.1, quantile form.** `F ≤disp G` iff, for every `c`, `G⁻¹ − F⁻¹ − c` changes sign at
    most once on `(0, 1)`, and from `−` to `+` if once. -/
theorem thm21_quantile (y z : ℝ → ℝ) :
    OrdSpacing y z ↔ ∀ c : ℝ, SignChangeUpOnce (fun u => z u - y u - c) (Ioo 0 1) := by
  constructor
  · intro h c u1 h1 u2 h2 h12 hp
    have := h u1 h1 u2 h2 h12.le
    simp only at hp ⊢
    linarith
  · intro H α hα β hβ hab
    by_contra hlt
    push_neg at hlt
    have hne : α < β := lt_of_le_of_ne hab (by
      rintro rfl
      simp at hlt)
    have := H (((z α - y α) + (z β - y β)) / 2) α hα β hβ hne (by simp only; linarith)
    simp only at this
    linarith

/-- The same in Hopkins and Kornienko's form, through `LewisThompson.spacing_iff_diff`. -/
theorem thm21_diff (y z : ℝ → ℝ) :
    OrdDiff y z ↔ ∀ c : ℝ, SignChangeUpOnce (fun u => z u - y u - c) (Ioo 0 1) :=
  (spacing_iff_diff y z).symm.trans (thm21_quantile y z)

/-! ## The bridge from quantiles to distribution functions -/

/-- Above the quantile: `u < F t` iff `F⁻¹(u) < t`. -/
theorem lt_cdf_iff (F y : ℝ → ℝ) (hF : Monotone F) (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1))
    (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u) (u : ℝ) (hu : u ∈ Ioo (0 : ℝ) 1) (t : ℝ) :
    u < F t ↔ y u < t := by
  constructor
  · intro h
    by_contra hle
    push_neg at hle
    have := hF hle
    rw [hy u hu] at this
    linarith
  · intro h
    have hle : u ≤ F t := (hy u hu).symm.le.trans (hF h.le)
    rcases hle.lt_or_eq with hlt | heq
    · exact hlt
    · exfalso
      have hmem : y u ∈ F ⁻¹' Ioo 0 1 := by
        simp only [mem_preimage, hy u hu]
        exact hu
      have hmem' : t ∈ F ⁻¹' Ioo 0 1 := by
        simp only [mem_preimage, ← heq]
        exact hu
      have := hFs hmem hmem' h
      rw [hy u hu, ← heq] at this
      exact lt_irrefl _ this

/-- Below the quantile: `F t < u` iff `t < F⁻¹(u)`. -/
theorem cdf_lt_iff (F y : ℝ → ℝ) (hF : Monotone F) (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1))
    (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u) (u : ℝ) (hu : u ∈ Ioo (0 : ℝ) 1) (t : ℝ) :
    F t < u ↔ t < y u := by
  constructor
  · intro h
    by_contra hle
    push_neg at hle
    have := hF hle
    rw [hy u hu] at this
    linarith
  · intro h
    have hle : F t ≤ u := (hF h.le).trans (hy u hu).le
    rcases hle.lt_or_eq with hlt | heq
    · exact hlt
    · exfalso
      have hmem : y u ∈ F ⁻¹' Ioo 0 1 := by
        simp only [mem_preimage, hy u hu]
        exact hu
      have hmem' : t ∈ F ⁻¹' Ioo 0 1 := by
        simp only [mem_preimage, heq]
        exact hu
      have := hFs hmem' hmem h
      rw [hy u hu, heq] at this
      exact lt_irrefl _ this

/-- **The sign bridge.** At `x = G⁻¹(u)`, `F_c(x) − G(x) = F(x − c) − G(x)` has the sign of
    `G⁻¹(u) − F⁻¹(u) − c`. -/
theorem cdf_sign_eq_quantile_sign (F G y z : ℝ → ℝ) (hF : Monotone F)
    (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1)) (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u)
    (hz : ∀ u ∈ Ioo (0 : ℝ) 1, G (z u) = u) (u : ℝ) (hu : u ∈ Ioo (0 : ℝ) 1) (c : ℝ) :
    SignType.sign (F (z u - c) - G (z u)) = SignType.sign (z u - y u - c) := by
  rw [hz u hu]
  rcases lt_trichotomy (z u - y u - c) 0 with h | h | h
  · rw [sign_neg h, sign_neg]
    have := (cdf_lt_iff F y hF hFs hy u hu (z u - c)).mpr (by linarith)
    linarith
  · rw [h, sign_zero, sign_eq_zero_iff, sub_eq_zero, show z u - c = y u by linarith, hy u hu]
  · rw [sign_pos h, sign_pos]
    have := (lt_cdf_iff F y hF hFs hy u hu (z u - c)).mpr (by linarith)
    linarith

/-! ## Theorem 2.1, p.312, in Shaked's form `S⁻(F_c − G) ≤ 1`, `−` to `+` -/

/-- **Theorem 2.1, `⇒`, on the whole line.** If `F ≤disp G` then for every `c` the function
    `F_c − G` is never negative after it has been positive. Strictness of `F` is not needed. -/
theorem thm21_cdf_forward (F G y z : ℝ → ℝ) (hF : Monotone F) (hG : Monotone G)
    (hGr : ∀ x, G x ∈ Icc (0 : ℝ) 1)
    (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u) (hz : ∀ u ∈ Ioo (0 : ℝ) 1, G (z u) = u)
    (h : OrdSpacing y z) (c : ℝ) : SignChangeUpOnce (fun x => F (x - c) - G x) univ := by
  intro x1 _ x2 _ hx hpos
  simp only at hpos ⊢
  have hq : F (x1 - c) ≤ F (x2 - c) := hF (by linarith)
  have hw0 := (hGr x1).1
  have hv1 := (hGr x2).2
  rcases le_or_gt (G x2) (G x1) with hv | hv
  · linarith
  · -- a level `u0` strictly between `G x1` and `min (F (x1 - c)) (G x2)`
    have hm1 : G x1 < min (F (x1 - c)) (G x2) := lt_min (by linarith) hv
    have hm2 : min (F (x1 - c)) (G x2) ≤ F (x1 - c) := min_le_left _ _
    have hm3 : min (F (x1 - c)) (G x2) ≤ G x2 := min_le_right _ _
    have hu0m : (G x1 + min (F (x1 - c)) (G x2)) / 2 ∈ Ioo (0 : ℝ) 1 :=
      ⟨by linarith, by linarith⟩
    have hz0 : x1 < z ((G x1 + min (F (x1 - c)) (G x2)) / 2) := by
      by_contra hle
      push_neg at hle
      have := hG hle
      rw [hz _ hu0m] at this
      linarith
    have hy0 : y ((G x1 + min (F (x1 - c)) (G x2)) / 2) < x1 - c := by
      by_contra hle
      push_neg at hle
      have := hF hle
      rw [hy _ hu0m] at this
      linarith
    -- at that level the quantile gap exceeds `c`; it does at every higher level, so `F_c ≥ G` at `x2`
    by_contra hneg
    push_neg at hneg
    have hum : (F (x2 - c) + G x2) / 2 ∈ Ioo (0 : ℝ) 1 := ⟨by linarith, by linarith⟩
    have hsp := h _ hu0m _ hum (by linarith)
    have hzu : z ((F (x2 - c) + G x2) / 2) < x2 := by
      by_contra hle
      push_neg at hle
      have := hG hle
      rw [hz _ hum] at this
      linarith
    have := hF (show y ((F (x2 - c) + G x2) / 2) ≤ x2 - c by linarith)
    rw [hy _ hum] at this
    linarith

/-- **Theorem 2.1, `⇐`.** If for every `c` the function `F_c − G` is never negative after it has
    been positive, on the support `G⁻¹((0, 1))` of `G` alone, then `F ≤disp G`. Proved through
    `cdf_sign_eq_quantile_sign` and `thm21_quantile`. -/
theorem thm21_cdf_converse (F G y z : ℝ → ℝ) (hF : Monotone F)
    (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1)) (hG : Monotone G)
    (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u) (hz : ∀ u ∈ Ioo (0 : ℝ) 1, G (z u) = u)
    (H : ∀ c : ℝ, SignChangeUpOnce (fun x => F (x - c) - G x) (z '' Ioo 0 1)) :
    OrdSpacing y z := by
  rw [thm21_quantile]
  intro c u1 h1 u2 h2 h12 hpos
  simp only at hpos ⊢
  have hz12 : z u1 < z u2 := by
    by_contra hle
    push_neg at hle
    have := hG hle
    rw [hz u1 h1, hz u2 h2] at this
    linarith
  have s1 := cdf_sign_eq_quantile_sign F G y z hF hFs hy hz u1 h1 c
  have s2 := cdf_sign_eq_quantile_sign F G y z hF hFs hy hz u2 h2 c
  have p1 : 0 < F (z u1 - c) - G (z u1) := by
    rw [← sign_eq_one_iff, s1, sign_eq_one_iff]
    exact hpos
  have p2 := H c (z u1) (mem_image_of_mem z h1) (z u2) (mem_image_of_mem z h2) hz12 p1
  simp only at p2
  rw [← sign_nonneg_iff, ← s2, sign_nonneg_iff]
  exact p2

/-- **Theorem 2.1** (p.312): `F ≤disp G` iff `S⁻(F_c − G) ≤ 1` on `ℝ` for every `c`, with sign
    sequence `−, +` in case of equality. -/
theorem thm21_cdf (F G y z : ℝ → ℝ) (hF : Monotone F) (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1))
    (hG : Monotone G) (hGr : ∀ x, G x ∈ Icc (0 : ℝ) 1)
    (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u) (hz : ∀ u ∈ Ioo (0 : ℝ) 1, G (z u) = u) :
    OrdSpacing y z ↔ ∀ c : ℝ, SignChangeUpOnce (fun x => F (x - c) - G x) univ :=
  ⟨fun h c => thm21_cdf_forward F G y z hF hG hGr hy hz h c,
    fun H => thm21_cdf_converse F G y z hF hFs hG hy hz
      (fun c => signChangeUpOnce_mono (H c) (subset_univ _))⟩

/-- **Theorem 2.1 on the support of `G`.** The same equivalence with the sign changes counted on
    `G⁻¹((0, 1))` only. -/
theorem thm21_cdf_support (F G y z : ℝ → ℝ) (hF : Monotone F)
    (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1)) (hG : Monotone G) (hGr : ∀ x, G x ∈ Icc (0 : ℝ) 1)
    (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u) (hz : ∀ u ∈ Ioo (0 : ℝ) 1, G (z u) = u) :
    OrdSpacing y z ↔ ∀ c : ℝ, SignChangeUpOnce (fun x => F (x - c) - G x) (z '' Ioo 0 1) :=
  ⟨fun h c => signChangeUpOnce_mono (thm21_cdf_forward F G y z hF hG hGr hy hz h c)
      (subset_univ _),
    thm21_cdf_converse F G y z hF hFs hG hy hz⟩

/-! ## Theorem 2.3, p.314: the transport `φ = G⁻¹ ∘ F` -/

/-- On the support of `F`, the quantile function inverts `F`. -/
theorem quantile_cdf (F y : ℝ → ℝ) (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1))
    (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u) (x : ℝ) (hx : x ∈ F ⁻¹' Ioo 0 1) :
    y (F x) = x := by
  have hmem : y (F x) ∈ F ⁻¹' Ioo 0 1 := by
    simp only [mem_preimage]
    rw [hy _ hx]
    exact hx
  exact hFs.injOn hmem hx (hy _ hx)

/-- **Theorem 2.3** (p.314), on an interval support: `F ≤disp G` iff `φ = G⁻¹ ∘ F` satisfies
    `φ(x2) − φ(x1) ≥ x2 − x1` for `x1 < x2` in the support of `F`. -/
theorem thm23 (F y z : ℝ → ℝ) (hF : Monotone F) (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1))
    (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u) :
    OrdSpacing y z ↔ ∀ x1 ∈ F ⁻¹' Ioo 0 1, ∀ x2 ∈ F ⁻¹' Ioo 0 1, x1 < x2 →
      x2 - x1 ≤ z (F x2) - z (F x1) := by
  constructor
  · intro h x1 h1 x2 h2 h12
    have := h (F x1) h1 (F x2) h2 (hF h12.le)
    rw [quantile_cdf F y hFs hy x1 h1, quantile_cdf F y hFs hy x2 h2] at this
    exact this
  · intro H α hα β hβ hab
    rcases hab.lt_or_eq with hlt | rfl
    · have hyab : y α < y β := by
        by_contra hle
        push_neg at hle
        have := hF hle
        rw [hy α hα, hy β hβ] at this
        linarith
      have hmα : y α ∈ F ⁻¹' Ioo 0 1 := by
        simp only [mem_preimage, hy α hα]
        exact hα
      have hmβ : y β ∈ F ⁻¹' Ioo 0 1 := by
        simp only [mem_preimage, hy β hβ]
        exact hβ
      have := H (y α) hmα (y β) hmβ hyab
      rw [hy α hα, hy β hβ] at this
      exact this
    · simp

/-- **Theorem 2.3 with full support, from Lewis and Thompson.** When `F` is strictly increasing on
    `ℝ` with values in `(0, 1)`, Shaked's statement is `LewisThompson.transport_disp` composed with
    `LewisThompson.spacing_iff_diff`, read as increments of `φ`. -/
theorem thm23_full_support (F y z : ℝ → ℝ) (hF : StrictMono F)
    (hFr : ∀ x, F x ∈ Ioo (0 : ℝ) 1) (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u)
    (hyF : ∀ x, y (F x) = x) :
    OrdSpacing y z ↔ ∀ x1 x2 : ℝ, x1 < x2 → x2 - x1 ≤ z (F x2) - z (F x1) := by
  rw [spacing_iff_diff, transport_disp F y z hF hFr hy hyF]
  constructor
  · intro h x1 x2 h12
    have := h h12.le
    simp only at this
    linarith
  · intro h x1 x2 h12
    rcases h12.lt_or_eq with hlt | rfl
    · have := h x1 x2 hlt
      simp only
      linarith
    · exact le_rfl

/-- **Theorem 2.3, derivative form, sufficiency:** `φ' ≥ 1` on the support of `F` gives
    `F ≤disp G`. -/
theorem thm23_deriv (F y z φ' : ℝ → ℝ) (hF : Monotone F) (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1))
    (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u)
    (hd : ∀ x ∈ F ⁻¹' Ioo 0 1, HasDerivAt (fun x => z (F x)) (φ' x) x)
    (h1 : ∀ x ∈ F ⁻¹' Ioo 0 1, 1 ≤ φ' x) : OrdSpacing y z := by
  rw [thm23 F y z hF hFs hy]
  have hconv : Convex ℝ (F ⁻¹' Ioo 0 1) := (ordConnected_Ioo.preimage_mono hF).convex
  intro x1 hx1 x2 hx2 h12
  have := hconv.mul_sub_le_image_sub_of_le_deriv (f := fun x => z (F x)) (C := 1)
    (fun x hx => (hd x hx).continuousAt.continuousWithinAt)
    (fun x hx => (hd x (interior_subset hx)).differentiableAt.differentiableWithinAt)
    (fun x hx => by
      rw [(hd x (interior_subset hx)).deriv]
      exact h1 x (interior_subset hx))
    x1 hx1 x2 hx2 h12.le
  simpa using this

/-- **Theorem 2.3, derivative form, necessity:** if `F ≤disp G`, then at an interior point of the
    support of `F` where `φ` is differentiable, `φ' ≥ 1`. -/
theorem thm23_deriv_necessary (F y z : ℝ → ℝ) (hF : Monotone F)
    (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1)) (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u)
    (h : OrdSpacing y z) (x d : ℝ) (hx : F ⁻¹' Ioo 0 1 ∈ 𝓝 x)
    (hd : HasDerivAt (fun x => z (F x)) d x) : 1 ≤ d := by
  have hmono : MonotoneOn (fun x => z (F x) - x) (F ⁻¹' Ioo 0 1) := by
    intro a ha b hb hab
    rcases hab.lt_or_eq with hlt | rfl
    · have := (thm23 F y z hF hFs hy).mp h a ha b hb hlt
      simp only
      linarith
    · exact le_rfl
  have hacc : AccPt x (𝓟 (F ⁻¹' Ioo 0 1)) := by
    have := (PerfectSpace.univ_preperfect x (mem_univ x)).nhds_inter hx
    rwa [inter_univ] at this
  have := (hd.sub (hasDerivAt_id' x)).hasDerivWithinAt.nonneg_of_monotoneOn hacc hmono
  linarith

/-- **Theorem 2.3, derivative form.** On an open support where `φ` is differentiable,
    `F ≤disp G` iff `φ' ≥ 1`. -/
theorem thm23_deriv_iff (F y z φ' : ℝ → ℝ) (hF : Monotone F)
    (hFs : StrictMonoOn F (F ⁻¹' Ioo 0 1)) (hy : ∀ u ∈ Ioo (0 : ℝ) 1, F (y u) = u)
    (hopen : IsOpen (F ⁻¹' Ioo 0 1))
    (hd : ∀ x ∈ F ⁻¹' Ioo 0 1, HasDerivAt (fun x => z (F x)) (φ' x) x) :
    OrdSpacing y z ↔ ∀ x ∈ F ⁻¹' Ioo 0 1, 1 ≤ φ' x :=
  ⟨fun h x hx => thm23_deriv_necessary F y z hF hFs hy h x (φ' x) (hopen.mem_nhds hx) (hd x hx),
    thm23_deriv F y z φ' hF hFs hy hd⟩

/-! ## Illustration, p.315: `Y = aX` with `a > 1` -/

/-- **`X ≤disp aX` for `a ≥ 1`.** This is `LewisThompson.scale_pair` verbatim. -/
theorem scale_disp (y : ℝ → ℝ) (hy : MonotoneOn y (Ioo 0 1)) (a : ℝ) (ha : 1 ≤ a) :
    OrdSpacing y (fun u => a * y u) :=
  scale_pair y hy a ha

/-! ## Example 3.3, p.319: the exponential against `x/(1+x)` -/

/-- The exponential distribution function: `1 − e^{−x}` for `x > 0`, `0` otherwise. -/
noncomputable def Fexp (x : ℝ) : ℝ := max 0 (1 - Real.exp (-x))

/-- `G(x) = x/(1+x)` for `x > 0`, `0` otherwise. -/
noncomputable def Gx (x : ℝ) : ℝ := max x 0 / (1 + max x 0)

/-- The exponential quantile function, `−log(1 − u)`. -/
noncomputable def yExp (u : ℝ) : ℝ := -Real.log (1 - u)

/-- The quantile function of `Gx`, `u/(1 − u)`. -/
noncomputable def zx (u : ℝ) : ℝ := u / (1 - u)

theorem Fexp_of_pos (x : ℝ) (hx : 0 < x) : Fexp x = 1 - Real.exp (-x) := by
  unfold Fexp
  have : Real.exp (-x) < 1 := Real.exp_lt_one_iff.mpr (by linarith)
  exact max_eq_right (by linarith)

theorem Fexp_mem_iff (x : ℝ) : Fexp x ∈ Ioo (0 : ℝ) 1 ↔ 0 < x := by
  constructor
  · intro h
    by_contra hle
    push_neg at hle
    have he : 1 ≤ Real.exp (-x) := Real.one_le_exp (by linarith)
    have : Fexp x = 0 := by
      unfold Fexp
      exact max_eq_left (by linarith)
    rw [this] at h
    exact lt_irrefl _ h.1
  · intro hx
    rw [Fexp_of_pos x hx]
    have h1 : Real.exp (-x) < 1 := Real.exp_lt_one_iff.mpr (by linarith)
    have h2 := Real.exp_pos (-x)
    exact ⟨by linarith, by linarith⟩

theorem Fexp_monotone : Monotone Fexp := by
  intro a b hab
  unfold Fexp
  have : Real.exp (-b) ≤ Real.exp (-a) := Real.exp_le_exp.mpr (by linarith)
  exact max_le_max le_rfl (by linarith)

theorem Fexp_strictMonoOn : StrictMonoOn Fexp (Fexp ⁻¹' Ioo 0 1) := by
  intro a ha b hb hab
  have ha0 : 0 < a := (Fexp_mem_iff a).mp ha
  have hb0 : 0 < b := (Fexp_mem_iff b).mp hb
  rw [Fexp_of_pos a ha0, Fexp_of_pos b hb0]
  have : Real.exp (-b) < Real.exp (-a) := Real.exp_lt_exp.mpr (by linarith)
  linarith

/-- The CDF fact for `F`: `F(F⁻¹(u)) = u`. -/
theorem Fexp_yExp (u : ℝ) (hu : u ∈ Ioo (0 : ℝ) 1) : Fexp (yExp u) = u := by
  unfold Fexp yExp
  rw [neg_neg, Real.exp_log (show (0 : ℝ) < 1 - u by linarith [hu.2])]
  rw [sub_sub_cancel]
  exact max_eq_right hu.1.le

theorem Gx_monotone : Monotone Gx := by
  intro a b hab
  unfold Gx
  have hp : 0 ≤ max a 0 := le_max_right _ _
  have hq : 0 ≤ max b 0 := le_max_right _ _
  have hpq : max a 0 ≤ max b 0 := max_le_max hab le_rfl
  rw [div_le_div_iff₀ (by linarith) (by linarith)]
  nlinarith

theorem Gx_mem (x : ℝ) : Gx x ∈ Icc (0 : ℝ) 1 := by
  unfold Gx
  have hp : 0 ≤ max x 0 := le_max_right _ _
  refine ⟨by positivity, ?_⟩
  rw [div_le_one (by linarith)]
  linarith

/-- The CDF fact for `G`: `G(G⁻¹(u)) = u`. -/
theorem Gx_zx (u : ℝ) (hu : u ∈ Ioo (0 : ℝ) 1) : Gx (zx u) = u := by
  have h1 : 0 < 1 - u := by linarith [hu.2]
  have hz : 0 < zx u := div_pos hu.1 h1
  unfold Gx
  rw [max_eq_left hz.le]
  unfold zx
  field_simp
  ring

/-- Shaked's `φ(x) = G⁻¹(F(x)) = e^x − 1` on the support `x > 0`. -/
theorem phi33 (x : ℝ) (hx : 0 < x) : zx (Fexp x) = Real.exp x - 1 := by
  rw [Fexp_of_pos x hx]
  unfold zx
  rw [sub_sub_cancel, Real.exp_neg]
  have := Real.exp_pos x
  field_simp

theorem phi33_hasDerivAt (x : ℝ) (hx : 0 < x) :
    HasDerivAt (fun x => zx (Fexp x)) (Real.exp x) x := by
  have h : HasDerivAt (fun x => Real.exp x - 1) (Real.exp x) x :=
    (Real.hasDerivAt_exp x).sub_const 1
  refine h.congr_of_eventuallyEq ?_
  filter_upwards [Ioi_mem_nhds hx] with t ht
  exact phi33 t ht

/-- **Example 3.3** (p.319): with `φ(x) = e^x − 1` and `φ' = e^x ≥ 1` on `x > 0`, Theorem 2.3
    gives `F ≤disp G` for `F(x) = 1 − e^{−x}` and `G(x) = x/(1+x)`. -/
theorem example33 : OrdSpacing yExp zx :=
  thm23_deriv Fexp yExp zx Real.exp Fexp_monotone Fexp_strictMonoOn Fexp_yExp
    (fun x hx => phi33_hasDerivAt x ((Fexp_mem_iff x).mp hx))
    (fun x hx => Real.one_le_exp ((Fexp_mem_iff x).mp hx).le)

/-- Example 3.3 read through Theorem 2.1: every shift of `F` crosses `G` at most once, from
    below. The hypotheses of `thm21_cdf_forward` hold for this pair. -/
theorem example33_cdf (c : ℝ) : SignChangeUpOnce (fun x => Fexp (x - c) - Gx x) univ :=
  thm21_cdf_forward Fexp Gx yExp zx Fexp_monotone Gx_monotone Gx_mem Fexp_yExp Gx_zx example33 c

/-! ## Controls: each half of "at most once, from `−` to `+`" is load-bearing -/

/-- The uniform quantile function on `(0, 1)`. -/
def yU (u : ℝ) : ℝ := u

/-- A quantile function whose gap to `yU` is the hump `u (1 − u)`. -/
def zHump (u : ℝ) : ℝ := u + u * (1 - u)

/-- The quantile function of a uniform law on `(0, 2)`. -/
def yTwo (u : ℝ) : ℝ := 2 * u

/-- `zHump` is a genuine quantile function: strictly increasing on `(0, 1)`. -/
theorem zHump_strictMonoOn : StrictMonoOn zHump (Ioo 0 1) := by
  intro a ha b hb hab
  unfold zHump
  nlinarith [ha.1, hb.2]

/-- **Control (a): one sign change is load-bearing.** For `y = u` and `z = 2u − u²`, the gap
    `z − y = u (1 − u)` is not monotone. For every `c` the first sign change of `z − y − c`, if any,
    is from `−` to `+`; at `c = 1/5` the signs run `−, +, −` (at `1/10`, `1/2`, `9/10`), so there
    are two changes; and the order fails. -/
theorem control_not_monotone :
    (∀ c : ℝ, FirstChangeUp (fun u => zHump u - yU u - c) (Ioo 0 1))
      ∧ (zHump (1 / 10) - yU (1 / 10) - 1 / 5 < 0 ∧ 0 < zHump (1 / 2) - yU (1 / 2) - 1 / 5
          ∧ zHump (9 / 10) - yU (9 / 10) - 1 / 5 < 0)
      ∧ ¬ AtMostOneSignChange (fun u => zHump u - yU u - 1 / 5) (Ioo 0 1)
      ∧ ¬ SignChangeUpOnce (fun u => zHump u - yU u - 1 / 5) (Ioo 0 1)
      ∧ ¬ OrdSpacing yU zHump := by
  have pattern : zHump (1 / 10) - yU (1 / 10) - 1 / 5 < 0 ∧ 0 < zHump (1 / 2) - yU (1 / 2) - 1 / 5
      ∧ zHump (9 / 10) - yU (9 / 10) - 1 / 5 < 0 := by
    unfold zHump yU
    norm_num
  have m1 : (1 / 10 : ℝ) ∈ Ioo (0 : ℝ) 1 := ⟨by norm_num, by norm_num⟩
  have m5 : (1 / 2 : ℝ) ∈ Ioo (0 : ℝ) 1 := ⟨by norm_num, by norm_num⟩
  have m9 : (9 / 10 : ℝ) ∈ Ioo (0 : ℝ) 1 := ⟨by norm_num, by norm_num⟩
  have hnot : ¬ OrdSpacing yU zHump := by
    intro h
    have := h (1 / 2) m5 (9 / 10) m9 (by norm_num)
    unfold zHump yU at this
    norm_num at this
  refine ⟨fun c => ?_, pattern, fun A => ?_, fun S => ?_, hnot⟩
  · intro u1 h1 u2 h2 h12 hp hn
    simp only [zHump, yU] at hp hn
    have hc : 0 < c := by nlinarith [h2.1, h2.2]
    have hm0 : 0 < min c u1 / 2 := by linarith [lt_min hc h1.1]
    have hmc : min c u1 / 2 < c := by linarith [min_le_left c u1]
    refine ⟨min c u1 / 2, ⟨hm0, by linarith [min_le_right c u1, h1.2]⟩,
      by linarith [min_le_right c u1, h1.1], ?_⟩
    simp only [zHump, yU]
    nlinarith
  · obtain ⟨p1, p2, p3⟩ := pattern
    exact A (1 / 10) m1 (1 / 2) m5 (9 / 10) m9 (by norm_num) (by norm_num)
      ⟨by nlinarith, by nlinarith⟩
  · obtain ⟨_, p2, p3⟩ := pattern
    have := S (1 / 2) m5 (9 / 10) m9 (by norm_num) p2
    linarith

/-- **Control (b): the direction `−` to `+` is load-bearing.** For `y = 2u` and `z = u` the gap
    `z − y = −u` falls. For every `c` the function `z − y − c` changes sign at most once; at
    `c = −1/2` it is `1/2 − u`, which changes once, from `+` to `−` (positive at `1/4` and at every
    earlier point, negative at `3/4`), so the first change is downward; and the order fails. -/
theorem control_down_crossing :
    (∀ c : ℝ, AtMostOneSignChange (fun u => yU u - yTwo u - c) (Ioo 0 1))
      ∧ (0 < yU (1 / 4) - yTwo (1 / 4) - (-1 / 2) ∧ yU (3 / 4) - yTwo (3 / 4) - (-1 / 2) < 0)
      ∧ ¬ FirstChangeUp (fun u => yU u - yTwo u - (-1 / 2)) (Ioo 0 1)
      ∧ ¬ SignChangeUpOnce (fun u => yU u - yTwo u - (-1 / 2)) (Ioo 0 1)
      ∧ ¬ OrdSpacing yTwo yU := by
  have pattern : 0 < yU (1 / 4) - yTwo (1 / 4) - (-1 / 2)
      ∧ yU (3 / 4) - yTwo (3 / 4) - (-1 / 2) < 0 := by
    unfold yU yTwo
    norm_num
  have m1 : (1 / 4 : ℝ) ∈ Ioo (0 : ℝ) 1 := ⟨by norm_num, by norm_num⟩
  have m3 : (3 / 4 : ℝ) ∈ Ioo (0 : ℝ) 1 := ⟨by norm_num, by norm_num⟩
  refine ⟨fun c => ?_, pattern, fun B => ?_, fun S => ?_, fun h => ?_⟩
  · intro u1 _ u2 _ u3 _ h12 h23 hp
    obtain ⟨p, q⟩ := hp
    simp only [yU, yTwo] at p q
    rcases lt_trichotomy (u2 - 2 * u2 - c) 0 with hn | hz | hpos
    · have : u3 - 2 * u3 - c < 0 := by linarith
      nlinarith
    · rw [hz, zero_mul] at q
      exact lt_irrefl _ q
    · have : 0 < u1 - 2 * u1 - c := by linarith
      nlinarith
  · obtain ⟨u0, h0, h01, hneg⟩ := B (1 / 4) m1 (3 / 4) m3 (by norm_num) pattern.1 pattern.2
    simp only [yU, yTwo] at hneg
    linarith [h0.1]
  · have := S (1 / 4) m1 (3 / 4) m3 (by norm_num) pattern.1
    linarith [pattern.2]
  · have := h (1 / 4) m1 (3 / 4) m3 (by norm_num)
    unfold yU yTwo at this
    norm_num at this

end Shaked1982
