namespace CostrellLoury

def wLo (D th b0 b1 mh mu : Int) : Int := D * b0 * mu + (D - th) * (b1 - b0) * mh

def wHi (D th b0 b1 mh mu : Int) : Int := D * b1 * mu - th * (b1 - b0) * mh

def wage (D th b0 b1 mh mu : Int) : Int :=
  if mu ≤ mh then wLo D th b0 b1 mh mu else wHi D th b0 b1 mh mu

def spanW (D th b0 b1 top mh : Int) : Int :=
  wage D th b0 b1 mh top - wage D th b0 b1 mh 0

theorem branches_agree_at_margin (D th b0 b1 mh : Int) :
    wLo D th b0 b1 mh mh = wHi D th b0 b1 mh mh := by
  unfold wLo wHi
  simp only [Int.sub_mul, Int.mul_sub]
  omega

theorem wage_eq_lo (D th b0 b1 mh mu : Int) (h : mu ≤ mh) :
    wage D th b0 b1 mh mu = wLo D th b0 b1 mh mu := by
  unfold wage
  split
  · rfl
  · omega

theorem wage_eq_hi (D th b0 b1 mh mu : Int) (h : mh ≤ mu) :
    wage D th b0 b1 mh mu = wHi D th b0 b1 mh mu := by
  unfold wage
  split
  · have he : mh = mu := by omega
    subst he
    exact branches_agree_at_margin D th b0 b1 mh
  · rfl

theorem wage_rises_below_margin (D th b0 b1 mh mh' mu : Int)
    (hb : b0 ≤ b1) (hth : th ≤ D) (hm : mh ≤ mh') (hmu : mu ≤ mh) :
    wage D th b0 b1 mh mu ≤ wage D th b0 b1 mh' mu := by
  rw [wage_eq_lo D th b0 b1 mh mu hmu, wage_eq_lo D th b0 b1 mh' mu (by omega)]
  unfold wLo
  have key : 0 ≤ (D - th) * (b1 - b0) * (mh' - mh) :=
    Int.mul_nonneg (Int.mul_nonneg (by omega) (by omega)) (by omega)
  rw [Int.mul_sub] at key
  omega

theorem wage_falls_above_margin (D th b0 b1 mh mh' mu : Int)
    (hb : b0 ≤ b1) (hth : 0 ≤ th) (hm : mh ≤ mh') (hmu : mh' ≤ mu) :
    wage D th b0 b1 mh' mu ≤ wage D th b0 b1 mh mu := by
  rw [wage_eq_hi D th b0 b1 mh' mu hmu, wage_eq_hi D th b0 b1 mh mu (by omega)]
  unfold wHi
  have key : 0 ≤ th * (b1 - b0) * (mh' - mh) :=
    Int.mul_nonneg (Int.mul_nonneg hth (by omega)) (by omega)
  rw [Int.mul_sub] at key
  omega

theorem span_closed_form (D th b0 b1 top mh : Int) (h0 : 0 ≤ mh) (htop : mh ≤ top) :
    spanW D th b0 b1 top mh = D * b1 * top - D * (b1 - b0) * mh := by
  unfold spanW
  rw [wage_eq_hi D th b0 b1 mh top htop, wage_eq_lo D th b0 b1 mh 0 h0]
  unfold wLo wHi
  simp only [Int.sub_mul, Int.mul_sub, Int.mul_zero]
  omega

theorem span_falls_iff_margin_rises (D th b0 b1 top mh mh' : Int)
    (hD : 0 < D) (hb : b0 < b1)
    (h0 : 0 ≤ mh) (h1 : mh ≤ top) (h0' : 0 ≤ mh') (h1' : mh' ≤ top) :
    spanW D th b0 b1 top mh' ≤ spanW D th b0 b1 top mh ↔ mh ≤ mh' := by
  rw [span_closed_form D th b0 b1 top mh h0 h1, span_closed_form D th b0 b1 top mh' h0' h1']
  have hpos : 0 < D * (b1 - b0) := Int.mul_pos hD (by omega)
  constructor
  · intro h
    have h2 : D * (b1 - b0) * mh ≤ D * (b1 - b0) * mh' := by omega
    exact Int.le_of_mul_le_mul_left h2 hpos
  · intro h
    have h2 : 0 ≤ D * (b1 - b0) * (mh' - mh) := Int.mul_nonneg (Int.le_of_lt hpos) (by omega)
    rw [Int.mul_sub] at h2
    omega

def LeftTailDown (Fq Gq : Nat → Int) (L : Nat) : Prop := ∀ p, p ≤ L → Gq p ≤ Fq p

def RightTailUp (Fq Gq : Nat → Int) (U n : Nat) : Prop := ∀ p, p < n → U ≤ p → Fq p ≤ Gq p

def TailSpread (Fq Gq : Nat → Int) (L U n : Nat) : Prop :=
  LeftTailDown Fq Gq L ∧ RightTailUp Fq Gq U n

def OnSupport (Q : Nat → Int) (top : Int) (n : Nat) : Prop := ∀ p, p < n → 0 ≤ Q p ∧ Q p ≤ top

theorem tail_rule_high_theta (Fq Gq : Nat → Int) (L U n th : Nat) (b0 b1 top : Int)
    (hts : TailSpread Fq Gq L U n) (hU : U ≤ th) (hn : th < n) (hb : b0 < b1)
    (hF : OnSupport Fq top n) (hG : OnSupport Gq top n) :
    Fq th ≤ Gq th
    ∧ spanW (n : Int) (th : Int) b0 b1 top (Gq th) ≤ spanW (n : Int) (th : Int) b0 b1 top (Fq th)
    ∧ (∀ mu, mu ≤ Fq th →
        wage (n : Int) (th : Int) b0 b1 (Fq th) mu ≤ wage (n : Int) (th : Int) b0 b1 (Gq th) mu)
    ∧ (∀ mu, Gq th ≤ mu →
        wage (n : Int) (th : Int) b0 b1 (Gq th) mu ≤ wage (n : Int) (th : Int) b0 b1 (Fq th) mu) := by
  have hup : Fq th ≤ Gq th := hts.2 th hn hU
  have hF' := hF th hn
  have hG' := hG th hn
  refine ⟨hup, ?_, ?_, ?_⟩
  · exact (span_falls_iff_margin_rises n th b0 b1 top (Fq th) (Gq th) (by omega) hb
      hF'.1 hF'.2 hG'.1 hG'.2).2 hup
  · intro mu hmu
    exact wage_rises_below_margin n th b0 b1 (Fq th) (Gq th) mu (by omega) (by omega) hup hmu
  · intro mu hmu
    exact wage_falls_above_margin n th b0 b1 (Fq th) (Gq th) mu (by omega) (by omega) hup hmu

theorem tail_rule_low_theta (Fq Gq : Nat → Int) (L U n th : Nat) (b0 b1 top : Int)
    (hts : TailSpread Fq Gq L U n) (hL : th ≤ L) (hn : th < n) (hb : b0 < b1)
    (hF : OnSupport Fq top n) (hG : OnSupport Gq top n) :
    Gq th ≤ Fq th
    ∧ spanW (n : Int) (th : Int) b0 b1 top (Fq th) ≤ spanW (n : Int) (th : Int) b0 b1 top (Gq th)
    ∧ (∀ mu, mu ≤ Gq th →
        wage (n : Int) (th : Int) b0 b1 (Gq th) mu ≤ wage (n : Int) (th : Int) b0 b1 (Fq th) mu)
    ∧ (∀ mu, Fq th ≤ mu →
        wage (n : Int) (th : Int) b0 b1 (Fq th) mu ≤ wage (n : Int) (th : Int) b0 b1 (Gq th) mu) := by
  have hdown : Gq th ≤ Fq th := hts.1 th hL
  have hF' := hF th hn
  have hG' := hG th hn
  refine ⟨hdown, ?_, ?_, ?_⟩
  · exact (span_falls_iff_margin_rises n th b0 b1 top (Gq th) (Fq th) (by omega) hb
      hG'.1 hG'.2 hF'.1 hF'.2).2 hdown
  · intro mu hmu
    exact wage_rises_below_margin n th b0 b1 (Gq th) (Fq th) mu (by omega) (by omega) hdown hmu
  · intro mu hmu
    exact wage_falls_above_margin n th b0 b1 (Gq th) (Fq th) mu (by omega) (by omega) hdown hmu

def PivotSpread (T : Int → Int) (x0 : Int) : Prop :=
  (∀ w, x0 ≤ w → w ≤ T w) ∧ (∀ w, w ≤ x0 → T w ≤ w)

theorem pivot_spread_at_fixed_rank (T : Int → Int) (x0 : Int) (Fq Gq : Nat → Int)
    (n th : Nat) (b0 b1 top : Int)
    (hT : PivotSpread T x0) (hGT : ∀ p, Gq p = T (Fq p)) (hn : th < n) (hb : b0 < b1)
    (hF : OnSupport Fq top n) (hG : OnSupport Gq top n) :
    (x0 ≤ Fq th →
      spanW (n : Int) (th : Int) b0 b1 top (Gq th) ≤ spanW (n : Int) (th : Int) b0 b1 top (Fq th))
    ∧ (Fq th ≤ x0 →
      spanW (n : Int) (th : Int) b0 b1 top (Fq th) ≤ spanW (n : Int) (th : Int) b0 b1 top (Gq th)) := by
  have hF' := hF th hn
  have hG' := hG th hn
  constructor
  · intro hx
    have hup : Fq th ≤ Gq th := by rw [hGT th]; exact hT.1 (Fq th) hx
    exact (span_falls_iff_margin_rises n th b0 b1 top (Fq th) (Gq th) (by omega) hb
      hF'.1 hF'.2 hG'.1 hG'.2).2 hup
  · intro hx
    have hdown : Gq th ≤ Fq th := by rw [hGT th]; exact hT.2 (Fq th) hx
    exact (span_falls_iff_margin_rises n th b0 b1 top (Gq th) (Fq th) (by omega) hb
      hG'.1 hG'.2 hF'.1 hF'.2).2 hdown

def idQ (p : Nat) : Int := p

def upQ (p : Nat) : Int := p + 1

theorem control_margin_moves :
    (∀ p, idQ p < upQ p)
    ∧ upQ 3 < idQ 5
    ∧ spanW 10 5 1 2 20 (idQ 5) < spanW 10 3 1 2 20 (upQ 3)
    ∧ spanW 10 5 1 2 20 (upQ 5) < spanW 10 5 1 2 20 (idQ 5) := by
  refine ⟨?_, by decide, by decide, by decide⟩
  intro p
  unfold idQ upQ
  omega

def sumTo (f : Nat → Int) : Nat → Int
  | 0 => 0
  | n + 1 => sumTo f n + f n

theorem sumTo_zero (f : Nat → Int) : sumTo f 0 = 0 := rfl

theorem sumTo_succ (f : Nat → Int) (n : Nat) : sumTo f (n + 1) = sumTo f n + f n := rfl

theorem sumTo_congr (f g : Nat → Int) (n : Nat) (h : ∀ i, i < n → f i = g i) :
    sumTo f n = sumTo g n := by
  induction n with
  | zero => rfl
  | succ k ih =>
    rw [sumTo_succ, sumTo_succ, ih (fun i hi => h i (by omega)), h k (by omega)]

theorem sumTo_sub (f g : Nat → Int) (n : Nat) :
    sumTo (fun i => f i - g i) n = sumTo f n - sumTo g n := by
  induction n with
  | zero => rfl
  | succ k ih =>
    rw [sumTo_succ, sumTo_succ, sumTo_succ, ih]
    omega

theorem sum_nonneg (f : Nat → Int) (n : Nat) (h : ∀ i, i < n → 0 ≤ f i) : 0 ≤ sumTo f n := by
  induction n with
  | zero => exact Int.le_refl 0
  | succ k ih =>
    rw [sumTo_succ]
    have h1 := ih (fun i hi => h i (by omega))
    have h2 := h k (by omega)
    omega

theorem sum_nonpos (f : Nat → Int) (n : Nat) (h : ∀ i, i < n → f i ≤ 0) : sumTo f n ≤ 0 := by
  induction n with
  | zero => exact Int.le_refl 0
  | succ k ih =>
    rw [sumTo_succ]
    have h1 := ih (fun i hi => h i (by omega))
    have h2 := h k (by omega)
    omega

theorem tail_sum_nonneg (d : Nat → Int) (i n : Nat) (hin : i ≤ n)
    (h : ∀ j, i ≤ j → j < n → 0 ≤ d j) : 0 ≤ sumTo d n - sumTo d i := by
  induction n with
  | zero =>
    have hi : i = 0 := by omega
    subst hi
    exact Int.le_refl _
  | succ k ih =>
    by_cases hik : i ≤ k
    · rw [sumTo_succ]
      have h1 := ih hik (fun j hj hjk => h j hj (by omega))
      have h2 := h k hik (by omega)
      omega
    · have he : i = k + 1 := by omega
      subst he
      omega

theorem abel (w x : Nat → Int) (n : Nat) :
    sumTo (fun i => w i * x i) n
      + sumTo (fun i => (w (i + 1) - w i) * sumTo x (i + 1)) n
      = w n * sumTo x n := by
  induction n with
  | zero =>
    rw [sumTo_zero, sumTo_zero, sumTo_zero, Int.mul_zero]
    omega
  | succ k ih =>
    rw [sumTo_succ (fun i => w i * x i), sumTo_succ (fun i => (w (i + 1) - w i) * sumTo x (i + 1)),
      sumTo_succ x]
    generalize sumTo (fun i => w i * x i) k = A at ih ⊢
    generalize sumTo (fun i => (w (i + 1) - w i) * sumTo x (i + 1)) k = B at ih ⊢
    simp only [Int.sub_mul, Int.mul_add]
    omega

def qdiff (Fq Gq : Nat → Int) (i : Nat) : Int := Gq i - Fq i

def Gam (d : Nat → Int) (n i : Nat) : Int := sumTo d n - sumTo d i

def RiskierQ (Fq Gq : Nat → Int) (n : Nat) : Prop :=
  Gam (qdiff Fq Gq) n 0 = 0 ∧ ∀ i, i ≤ n → 0 ≤ Gam (qdiff Fq Gq) n i

def output (beta mu : Nat → Int) (n : Nat) : Int := sumTo (fun i => beta i * mu i) n

theorem gam_top_zero (d : Nat → Int) (n : Nat) : Gam d n n = 0 := by
  unfold Gam
  omega

theorem output_diff (beta Fq Gq : Nat → Int) (n : Nat) :
    output beta Gq n - output beta Fq n = sumTo (fun i => beta i * qdiff Fq Gq i) n := by
  unfold output
  rw [← sumTo_sub]
  apply sumTo_congr
  intro i _
  unfold qdiff
  rw [Int.mul_sub]

theorem prop5_integral_form (beta Fq Gq : Nat → Int) (n : Nat)
    (hmean : Gam (qdiff Fq Gq) n 0 = 0) :
    output beta Gq n - output beta Fq n
      = sumTo (fun i => (beta (i + 1) - beta i) * Gam (qdiff Fq Gq) n (i + 1)) n := by
  have hS : sumTo (qdiff Fq Gq) n = 0 := by
    unfold Gam at hmean
    rw [sumTo_zero] at hmean
    omega
  have hab := abel beta (qdiff Fq Gq) n
  rw [hS, Int.mul_zero] at hab
  have hneg : sumTo (fun i => (beta (i + 1) - beta i) * Gam (qdiff Fq Gq) n (i + 1)) n
      = sumTo (fun i => 0 - (beta (i + 1) - beta i) * sumTo (qdiff Fq Gq) (i + 1)) n := by
    apply sumTo_congr
    intro i _
    unfold Gam
    rw [hS, Int.mul_sub, Int.mul_zero]
  rw [output_diff, hneg, sumTo_sub]
  have hz : sumTo (fun _ => (0 : Int)) n = 0 := by
    have := sum_nonneg (fun _ => (0 : Int)) n (fun _ _ => Int.le_refl 0)
    have := sum_nonpos (fun _ => (0 : Int)) n (fun _ _ => Int.le_refl 0)
    omega
  rw [hz]
  omega

theorem prop5_monotone_weight (beta Fq Gq : Nat → Int) (n : Nat)
    (hrisk : RiskierQ Fq Gq n) (hmono : ∀ i, i + 1 < n → beta i ≤ beta (i + 1)) :
    output beta Fq n ≤ output beta Gq n := by
  have hform := prop5_integral_form beta Fq Gq n hrisk.1
  have hnn : 0 ≤ sumTo (fun i => (beta (i + 1) - beta i) * Gam (qdiff Fq Gq) n (i + 1)) n := by
    apply sum_nonneg
    intro i hi
    by_cases hlast : i + 1 < n
    · exact Int.mul_nonneg (by have := hmono i hlast; omega) (hrisk.2 (i + 1) (by omega))
    · have he : i + 1 = n := by omega
      rw [he, gam_top_zero]
      exact Int.le_of_eq (Int.mul_zero _).symm
  omega

theorem single_crossing_riskier (Fq Gq : Nat → Int) (n k : Nat)
    (hlow : ∀ i, i < k → qdiff Fq Gq i ≤ 0)
    (hhigh : ∀ i, k ≤ i → i < n → 0 ≤ qdiff Fq Gq i)
    (hmean : sumTo (qdiff Fq Gq) n = 0) : RiskierQ Fq Gq n := by
  refine ⟨?_, ?_⟩
  · unfold Gam
    rw [sumTo_zero, hmean]
    rfl
  · intro i hi
    unfold Gam
    by_cases hik : i ≤ k
    · have := sum_nonpos (qdiff Fq Gq) i (fun j hj => hlow j (by omega))
      omega
    · exact tail_sum_nonneg (qdiff Fq Gq) i n hi (fun j hj hjn => hhigh j (by omega) hjn)

theorem prop5_single_crossing (beta Fq Gq : Nat → Int) (n k : Nat)
    (hlow : ∀ i, i < k → qdiff Fq Gq i ≤ 0)
    (hhigh : ∀ i, k ≤ i → i < n → 0 ≤ qdiff Fq Gq i)
    (hmean : sumTo (qdiff Fq Gq) n = 0)
    (hmono : ∀ i, i + 1 < n → beta i ≤ beta (i + 1)) :
    output beta Fq n ≤ output beta Gq n :=
  prop5_monotone_weight beta Fq Gq n (single_crossing_riskier Fq Gq n k hlow hhigh hmean) hmono

def qFlat : Nat → Int := fun i => ([1, 1] : List Int).getD i 0

def qWide : Nat → Int := fun i => ([0, 2] : List Int).getD i 0

def wDown : Nat → Int := fun i => ([1, 0] : List Int).getD i 0

def wUp : Nat → Int := fun i => ([0, 1] : List Int).getD i 0

theorem control_prop5_decreasing_weight :
    RiskierQ qFlat qWide 2 ∧ wDown 1 < wDown 0 ∧ output wDown qWide 2 < output wDown qFlat 2 := by
  unfold RiskierQ
  decide

theorem control_prop5_contraction :
    Gam (qdiff qWide qFlat) 2 0 = 0 ∧ wUp 0 ≤ wUp 1 ∧ ¬ RiskierQ qWide qFlat 2
    ∧ output wUp qFlat 2 < output wUp qWide 2 := by
  unfold RiskierQ
  decide

def expect (psi H : Nat → Int) (n : Nat) : Int := sumTo (fun i => psi i * (H (i + 1) - H i)) n

def wageSpan (beta mu : Nat → Int) (n : Nat) : Int := expect beta mu n

theorem sumTo_telescope (h : Nat → Int) (k : Nat) :
    sumTo (fun i => h (i + 1) - h i) k = h k - h 0 := by
  induction k with
  | zero => show (0 : Int) = h 0 - h 0; omega
  | succ j ih =>
    rw [sumTo_succ, ih]
    omega

theorem sumTo_shift (h : Nat → Int) (k : Nat) :
    sumTo (fun i => h (i + 1)) k = sumTo h (k + 1) - h 0 := by
  induction k with
  | zero => show (0 : Int) = (0 + h 0) - h 0; omega
  | succ j ih =>
    rw [sumTo_succ, ih, sumTo_succ h (j + 1)]
    omega

theorem lemma1_identity (psi f g : Nat → Int) (n : Nat)
    (h0 : g 0 = f 0) (hn : g n = f n) (hmean : sumTo (qdiff f g) n = 0) :
    expect psi g n - expect psi f n
      = sumTo (fun i => ((psi (i + 2) - psi (i + 1)) - (psi (i + 1) - psi i))
          * sumTo (qdiff f g) (i + 2)) n := by
  have he0 : qdiff f g 0 = 0 := by unfold qdiff; omega
  have hen : qdiff f g n = 0 := by unfold qdiff; omega
  have hA : expect psi g n - expect psi f n
      = sumTo (fun i => psi i * (qdiff f g (i + 1) - qdiff f g i)) n := by
    unfold expect
    rw [← sumTo_sub]
    apply sumTo_congr
    intro i _
    unfold qdiff
    simp only [Int.mul_sub]
    omega
  have hB : sumTo (fun i => psi i * (qdiff f g (i + 1) - qdiff f g i)) n
      + sumTo (fun i => (psi (i + 1) - psi i)
          * sumTo (fun j => qdiff f g (j + 1) - qdiff f g j) (i + 1)) n
      = psi n * sumTo (fun j => qdiff f g (j + 1) - qdiff f g j) n :=
    abel psi (fun j => qdiff f g (j + 1) - qdiff f g j) n
  have hB2 : sumTo (fun i => (psi (i + 1) - psi i)
          * sumTo (fun j => qdiff f g (j + 1) - qdiff f g j) (i + 1)) n
      = sumTo (fun i => (psi (i + 1) - psi i) * qdiff f g (i + 1)) n := by
    apply sumTo_congr
    intro i _
    rw [sumTo_telescope, he0, Int.sub_zero]
  have hB3 : sumTo (fun j => qdiff f g (j + 1) - qdiff f g j) n = 0 := by
    rw [sumTo_telescope, he0, hen]
    rfl
  rw [hB2, hB3, Int.mul_zero] at hB
  have hC : sumTo (fun i => (psi (i + 1) - psi i) * qdiff f g (i + 1)) n
      + sumTo (fun i => ((psi (i + 1 + 1) - psi (i + 1)) - (psi (i + 1) - psi i))
          * sumTo (fun j => qdiff f g (j + 1)) (i + 1)) n
      = (psi (n + 1) - psi n) * sumTo (fun j => qdiff f g (j + 1)) n :=
    abel (fun i => psi (i + 1) - psi i) (fun j => qdiff f g (j + 1)) n
  have hC2 : sumTo (fun i => ((psi (i + 1 + 1) - psi (i + 1)) - (psi (i + 1) - psi i))
          * sumTo (fun j => qdiff f g (j + 1)) (i + 1)) n
      = sumTo (fun i => ((psi (i + 2) - psi (i + 1)) - (psi (i + 1) - psi i))
          * sumTo (qdiff f g) (i + 2)) n := by
    apply sumTo_congr
    intro i _
    rw [sumTo_shift, he0, Int.sub_zero]
  have hC3 : sumTo (fun j => qdiff f g (j + 1)) n = 0 := by
    rw [sumTo_shift, he0, Int.sub_zero, sumTo_succ, hmean, hen]
    rfl
  rw [hC2, hC3, Int.mul_zero] at hC
  omega

theorem lemma1_concave (psi f g : Nat → Int) (n : Nat)
    (hrisk : RiskierQ f g n) (h0 : g 0 = f 0) (hn : g n = f n)
    (hconc : ∀ i, i + 2 ≤ n → psi (i + 2) - psi (i + 1) ≤ psi (i + 1) - psi i) :
    expect psi f n ≤ expect psi g n := by
  have hS : sumTo (qdiff f g) n = 0 := by
    have := hrisk.1
    unfold Gam at this
    rw [sumTo_zero] at this
    omega
  have hid := lemma1_identity psi f g n h0 hn hS
  have hnn : 0 ≤ sumTo (fun i => ((psi (i + 2) - psi (i + 1)) - (psi (i + 1) - psi i))
      * sumTo (qdiff f g) (i + 2)) n := by
    apply sum_nonneg
    intro i _
    by_cases hlast : i + 2 ≤ n
    · apply Int.mul_nonneg_of_nonpos_of_nonpos
      · have := hconc i hlast
        omega
      · have := hrisk.2 (i + 2) hlast
        unfold Gam at this
        omega
    · have he : i + 2 = n + 1 := by omega
      have hz : sumTo (qdiff f g) (i + 2) = 0 := by
        rw [he, sumTo_succ, hS]
        unfold qdiff
        omega
      rw [hz]
      exact Int.le_of_eq (Int.mul_zero _).symm
  omega

theorem lemma1_convex (psi f g : Nat → Int) (n : Nat)
    (hrisk : RiskierQ f g n) (h0 : g 0 = f 0) (hn : g n = f n)
    (hconv : ∀ i, i + 2 ≤ n → psi (i + 1) - psi i ≤ psi (i + 2) - psi (i + 1)) :
    expect psi g n ≤ expect psi f n := by
  have hS : sumTo (qdiff f g) n = 0 := by
    have := hrisk.1
    unfold Gam at this
    rw [sumTo_zero] at this
    omega
  have hid := lemma1_identity psi f g n h0 hn hS
  have hnp : sumTo (fun i => ((psi (i + 2) - psi (i + 1)) - (psi (i + 1) - psi i))
      * sumTo (qdiff f g) (i + 2)) n ≤ 0 := by
    apply sum_nonpos
    intro i _
    by_cases hlast : i + 2 ≤ n
    · apply Int.mul_nonpos_of_nonneg_of_nonpos
      · have := hconv i hlast
        omega
      · have := hrisk.2 (i + 2) hlast
        unfold Gam at this
        omega
    · have he : i + 2 = n + 1 := by omega
      have hz : sumTo (qdiff f g) (i + 2) = 0 := by
        rw [he, sumTo_succ, hS]
        unfold qdiff
        omega
      rw [hz]
      exact Int.le_of_eq (Int.mul_zero _)
  omega

theorem prop6_span_widens_concave (beta f g : Nat → Int) (n : Nat)
    (hrisk : RiskierQ f g n) (h0 : g 0 = f 0) (hn : g n = f n)
    (hconc : ∀ i, i + 2 ≤ n → beta (i + 2) - beta (i + 1) ≤ beta (i + 1) - beta i) :
    wageSpan beta f n ≤ wageSpan beta g n :=
  lemma1_concave beta f g n hrisk h0 hn hconc

theorem prop6_span_narrows_convex (beta f g : Nat → Int) (n : Nat)
    (hrisk : RiskierQ f g n) (h0 : g 0 = f 0) (hn : g n = f n)
    (hconv : ∀ i, i + 2 ≤ n → beta (i + 1) - beta i ≤ beta (i + 2) - beta (i + 1)) :
    wageSpan beta g n ≤ wageSpan beta f n :=
  lemma1_convex beta f g n hrisk h0 hn hconv

def fLin : Nat → Int := fun i => ([0, 1, 2, 3] : List Int).getD i 0

def gSpr : Nat → Int := fun i => ([0, 0, 3, 3] : List Int).getD i 0

def betaConcave : Nat → Int := fun i => ([0, 2, 3, 3] : List Int).getD i 0

def betaConvex : Nat → Int := fun i => ([0, 0, 1, 3] : List Int).getD i 0

theorem control_curvature_decides_span :
    RiskierQ fLin gSpr 3 ∧ gSpr 0 = fLin 0 ∧ gSpr 3 = fLin 3
    ∧ (∀ i, i < 3 → betaConcave i ≤ betaConcave (i + 1))
    ∧ (∀ i, i < 3 → betaConvex i ≤ betaConvex (i + 1))
    ∧ (∀ i, i < 2 → betaConcave (i + 2) - betaConcave (i + 1) ≤ betaConcave (i + 1) - betaConcave i)
    ∧ (∀ i, i < 2 → betaConvex (i + 1) - betaConvex i ≤ betaConvex (i + 2) - betaConvex (i + 1))
    ∧ wageSpan betaConcave fLin 3 < wageSpan betaConcave gSpr 3
    ∧ wageSpan betaConvex gSpr 3 < wageSpan betaConvex fLin 3
    ∧ output betaConcave fLin 3 < output betaConcave gSpr 3
    ∧ output betaConvex fLin 3 < output betaConvex gSpr 3 := by
  unfold RiskierQ
  decide

theorem prop10_output_decides (B c0 mF mG : Int) (r s u phi : Int → Int)
    (hB : 0 < B) (hc0 : 0 < c0)
    (hr : ∀ a b, a < b → r a < r b)
    (hs : ∀ a b, a ≤ b → s b ≤ s a)
    (hu : ∀ a b, a ≤ b → u b ≤ u a)
    (hphi : ∀ a b, a ≤ b → phi a ≤ phi b)
    (hQ : B * r mF ≤ B * r mG) :
    c0 * (B * r mF) ≤ c0 * (B * r mG)
    ∧ B * u mG ≤ B * u mF
    ∧ ∀ mu mu', mu' ≤ mu → B * s mG * (phi mu - phi mu') ≤ B * s mF * (phi mu - phi mu') := by
  have hrm : r mF ≤ r mG := Int.le_of_mul_le_mul_left hQ hB
  have hm : mF ≤ mG := by
    cases Int.lt_or_le mG mF with
    | inl h =>
      have := hr mG mF h
      omega
    | inr h => exact h
  refine ⟨Int.mul_le_mul_of_nonneg_left hQ (Int.le_of_lt hc0),
    Int.mul_le_mul_of_nonneg_left (hu mF mG hm) (Int.le_of_lt hB), ?_⟩
  intro mu mu' hmu
  have hd : 0 ≤ phi mu - phi mu' := by
    have := hphi mu' mu hmu
    omega
  exact Int.mul_le_mul_of_nonneg_right
    (Int.mul_le_mul_of_nonneg_left (hs mF mG hm) (Int.le_of_lt hB)) hd

theorem reversal_concave (beta f g : Nat → Int) (n : Nat)
    (hrisk : RiskierQ f g n) (h0 : g 0 = f 0) (hn : g n = f n)
    (hconc : ∀ i, i + 2 ≤ n → beta (i + 2) - beta (i + 1) ≤ beta (i + 1) - beta i)
    (B c0 mF mG : Int) (r s u phi : Int → Int)
    (hB : 0 < B) (hc0 : 0 < c0)
    (hr : ∀ a b, a < b → r a < r b)
    (hs : ∀ a b, a ≤ b → s b ≤ s a)
    (hu : ∀ a b, a ≤ b → u b ≤ u a)
    (hphi : ∀ a b, a ≤ b → phi a ≤ phi b)
    (hQ : B * r mF ≤ B * r mG) :
    wageSpan beta f n ≤ wageSpan beta g n
    ∧ ∀ mu mu', mu' ≤ mu → B * s mG * (phi mu - phi mu') ≤ B * s mF * (phi mu - phi mu') :=
  ⟨prop6_span_widens_concave beta f g n hrisk h0 hn hconc,
    (prop10_output_decides B c0 mF mG r s u phi hB hc0 hr hs hu hphi hQ).2.2⟩

theorem no_reversal_convex (beta f g : Nat → Int) (n : Nat)
    (hrisk : RiskierQ f g n) (h0 : g 0 = f 0) (hn : g n = f n)
    (hconv : ∀ i, i + 2 ≤ n → beta (i + 1) - beta i ≤ beta (i + 2) - beta (i + 1))
    (B c0 mF mG : Int) (r s u phi : Int → Int)
    (hB : 0 < B) (hc0 : 0 < c0)
    (hr : ∀ a b, a < b → r a < r b)
    (hs : ∀ a b, a ≤ b → s b ≤ s a)
    (hu : ∀ a b, a ≤ b → u b ≤ u a)
    (hphi : ∀ a b, a ≤ b → phi a ≤ phi b)
    (hQ : B * r mF ≤ B * r mG) :
    wageSpan beta g n ≤ wageSpan beta f n
    ∧ ∀ mu mu', mu' ≤ mu → B * s mG * (phi mu - phi mu') ≤ B * s mF * (phi mu - phi mu') :=
  ⟨prop6_span_narrows_convex beta f g n hrisk h0 hn hconv,
    (prop10_output_decides B c0 mF mG r s u phi hB hc0 hr hs hu hphi hQ).2.2⟩

def fTail : Nat → Int := fun i => ([0, 10, 20, 30, 40, 50, 60] : List Int).getD i 0

def gUp : Nat → Int := fun i => ([0, 9, 20, 31, 39, 51, 60] : List Int).getD i 0

def gDown : Nat → Int := fun i => ([0, 9, 21, 29, 40, 51, 60] : List Int).getD i 0

theorem control_tail_rule_silent_between :
    TailSpread fTail gUp 1 5 7 ∧ TailSpread fTail gDown 1 5 7
    ∧ RiskierQ fTail gUp 7 ∧ RiskierQ fTail gDown 7
    ∧ (∀ i, i < 6 → gUp i ≤ gUp (i + 1)) ∧ (∀ i, i < 6 → gDown i ≤ gDown (i + 1))
    ∧ 1 < 3 ∧ 3 < 5
    ∧ fTail 3 < gUp 3 ∧ gDown 3 < fTail 3
    ∧ spanW 7 3 1 2 60 (gUp 3) < spanW 7 3 1 2 60 (fTail 3)
    ∧ spanW 7 3 1 2 60 (fTail 3) < spanW 7 3 1 2 60 (gDown 3) := by
  unfold TailSpread LeftTailDown RightTailUp RiskierQ
  decide

end CostrellLoury
