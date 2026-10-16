import Mathlib

/-! Fryer and Jackson (2008), the structure Paper A1 relies on or contrasts with,
in the paper's notation.

Objects carry binary attributes `θ_k(o) ∈ {0, 1}`. The variation of a group of
objects is eq. (4), `Var(O) = ∑_{o ∈ O} d(θ(o), θ̄(O))`, with `d` the city-block
metric, so it is the sum over attributes of the variation in each attribute.

Three levels. (i) Objects: a finite set `S` of objects of any type, attributes
`θ : ι → Fin m → ℝ`, binary on `S`; eq. (4) is `Var`, and Lemmas 3 and 4 hold in
this generality. (ii) Groups summarised by counts (`Grp`), which Lemma 4 allows:
Theorem 1, Lemma 2 and Proposition 2 at any number of attributes. (iii) One
attribute at a time (`varAttr`) and the four types of Example 1 (`catVar`, counts
`a, b, c, d` of `(0,0), (0,1), (1,0), (1,1)`, first coordinate race with 1 white,
second human capital with 1 high), for the examples. The coverage of each
numbered result is listed in `lit/fryer_jackson_2008/CLAIMS.md`. -/

namespace FryerJackson2008

/-- Eq. (4) in one binary attribute. `np` objects have the attribute, `nm` do
not, and each is at city-block distance `|θ_k(o) − θ̄_k|` from the mean. -/
noncomputable def varAttr (np nm : ℝ) : ℝ :=
  np * |1 - np / (np + nm)| + nm * |0 - np / (np + nm)|

/-- Lemma 4 for one attribute. `Var(O) = 2 n⁺ n⁻ / n`. -/
theorem lemma4_one {np nm : ℝ} (hp : 0 ≤ np) (hm : 0 ≤ nm) (hn : 0 < np + nm) :
    varAttr np nm = 2 * np * nm / (np + nm) := by
  unfold varAttr
  have h1 : 1 - np / (np + nm) = nm / (np + nm) := by field_simp; ring
  rw [h1, zero_sub, abs_neg, abs_of_nonneg (div_nonneg hm hn.le),
    abs_of_nonneg (div_nonneg hp hn.le)]
  field_simp
  ring

/-- The variation of a category holding the four types of Example 1 in counts
`a, b, c, d` of `(0,0), (0,1), (1,0), (1,1)`. The city-block metric adds the
race attribute (`c + d` ones) and the human capital attribute (`b + d` ones). -/
noncomputable def catVar (a b c d : ℝ) : ℝ :=
  varAttr (c + d) (a + b) + varAttr (b + d) (a + c)

/-- The variation of the union of two homogeneous groups of sizes `ni, nj` that
differ on `h` attributes. Each differing attribute contributes `varAttr ni nj`
and each common attribute contributes nothing. -/
noncomputable def pairVar (h ni nj : ℝ) : ℝ := h * varAttr ni nj

/-- Proposition 1. For homogeneous groups, `Var(O₃ ∪ O₄) > Var(O₁ ∪ O₂)` if and
only if `h₃₄ (1/n₁ + 1/n₂) > h₁₂ (1/n₃ + 1/n₄)`. -/
theorem prop1 {h12 h34 n1 n2 n3 n4 : ℝ} (h1 : 0 < n1) (h2 : 0 < n2) (h3 : 0 < n3)
    (h4 : 0 < n4) :
    pairVar h12 n1 n2 < pairVar h34 n3 n4 ↔
      h12 * (1 / n3 + 1 / n4) < h34 * (1 / n1 + 1 / n2) := by
  unfold pairVar
  rw [lemma4_one h1.le h2.le (by linarith), lemma4_one h3.le h4.le (by linarith)]
  have e12 : 2 * n1 * n2 / (n1 + n2) = 2 / (1 / n1 + 1 / n2) := by
    field_simp; ring
  have e34 : 2 * n3 * n4 / (n3 + n4) = 2 / (1 / n3 + 1 / n4) := by
    field_simp; ring
  have p12 : 0 < 1 / n1 + 1 / n2 := by positivity
  have p34 : 0 < 1 / n3 + 1 / n4 := by positivity
  rw [e12, e34, mul_div_assoc', mul_div_assoc', div_lt_div_iff₀ p12 p34]
  constructor <;> intro h <;> nlinarith

/-- Example 1 and Table 1, three categories, with 5, 5, 45, 45 objects of types
`(0,0), (0,1), (1,0), (1,1)`. The race-based categorization, which pools the two
black types, has total variation 5. Pooling the two low types gives 9, and
pooling a black type with the white type of the other human capital level gives
18. -/
theorem example1_table :
    catVar 5 5 0 0 = 5 ∧ catVar 5 0 45 0 = 9 ∧ catVar 0 5 0 45 = 9 ∧
      catVar 5 0 0 45 = 18 ∧ catVar 0 5 45 0 = 18 ∧ catVar 0 0 45 45 = 45 := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩ <;>
    norm_num [catVar, varAttr, abs_of_nonneg, abs_of_nonpos]

/-- Example 1. With three categories the race-based categorization, in which the
black types share a category and the white types are perfectly sorted, has a
strictly smaller variation than each of the five other ways to merge two of the
four types. -/
theorem example1_race_optimal :
    catVar 5 5 0 0 < catVar 0 0 45 45 ∧ catVar 5 5 0 0 < catVar 5 0 45 0 ∧
      catVar 5 5 0 0 < catVar 0 5 0 45 ∧ catVar 5 5 0 0 < catVar 5 0 0 45 ∧
      catVar 5 5 0 0 < catVar 0 5 45 0 := by
  obtain ⟨a, b, c, d, e, f⟩ := example1_table
  rw [a, b, c, d, e, f]
  norm_num

/-- The prototype of the category holding both black types is `(0, 1/2)`. A
black worker of high human capital is predicted at `1/2` in human capital, an
error of `1/2`, while a white worker of high human capital, alone in a category,
is predicted without error. -/
theorem example1_prototype_error :
    |(1 : ℝ) - 5 / (5 + 5)| = 1 / 2 ∧ |(1 : ℝ) - 45 / (45 + 0)| = 0 := by
  constructor <;> norm_num

/-- The expected prediction error per object of a category, the payoff loss in
the expected-utility problem of Section 3, is the variation divided by the size.
In one binary attribute it is `2 p (1 − p)` with `p` the share of the category
having the attribute, twice the variance of that share. -/
theorem meanDev_eq {np nm : ℝ} (hp : 0 ≤ np) (hm : 0 ≤ nm) (hn : 0 < np + nm) :
    varAttr np nm / (np + nm) = 2 * (np / (np + nm)) * (1 - np / (np + nm)) := by
  rw [lemma4_one hp hm hn]
  field_simp
  ring

/-- The proof of Proposition 2. In the equidistant world (distance 1 between
different types), adding `nn` identical new objects to a category of `n` objects
with variation `V` raises its variation by `nn (2 n − V)/(n + nn)`, which is `nn`
times `2 − (V + 2 nn)/(n + nn)`. So the variation rises least where
`(V + 2 nn)/(n + nn)` is largest, as the proposition states. The printed proof
omits the factor `nn` (FJ-D2). -/
theorem prop2_change {n nn V : ℝ} (hn : 0 < n) (hnn : 0 < nn) :
    (n * V + 2 * n * nn) / (n + nn) - V = nn * (2 - (V + 2 * nn) / (n + nn)) := by
  have : n + nn ≠ 0 := by linarith
  field_simp
  ring

/-- Example 4 at the stated counts (70, 160, 80, 28 objects of types
`(0,0,0,0), (1,0,0,0), (0,1,0,0), (0,1,1,1)`), computed from Lemma 4 attribute by
attribute. Pooling types 1, 3 and 4 and leaving type 2 alone (`V₁₃₄,₂`) has a
strictly smaller variation than pooling 1 with 2 and 3 with 4 (`V₁₂,₃₄`), which
the paper reports as optimal. Footnote 22's numbers are not reproduced (FJ-D3).
The left side is `V₁₃₄,₂` (attribute 2 has 108 ones and 70 zeros, attributes 3
and 4 have 28 ones and 150 zeros), the right side `V₁₂,₃₄`. -/
theorem example4_not_reproduced :
    varAttr 108 70 + 2 * varAttr 28 150 < varAttr 160 70 + 2 * varAttr 28 80 := by
  norm_num [varAttr, abs_of_nonneg, abs_of_nonpos]

/-- Control: the race-based categorization is strictly optimal in Example 1
only because the black group is a minority. With 25 objects of each of the four
types, pooling the two black types and pooling the two low types give the same
variation, 25, so the strict inequality of `example1_race_optimal` fails. -/
theorem control_no_minority :
    catVar 25 25 0 0 = 25 ∧ catVar 25 0 25 0 = 25 ∧
      ¬ catVar 25 25 0 0 < catVar 25 0 25 0 := by
  have h1 : catVar 25 25 0 0 = 25 := by
    norm_num [catVar, varAttr, abs_of_nonneg, abs_of_nonpos]
  have h2 : catVar 25 0 25 0 = 25 := by
    norm_num [catVar, varAttr, abs_of_nonneg, abs_of_nonpos]
  exact ⟨h1, h2, by rw [h1, h2]; exact lt_irrefl _⟩

/-- Control: the coarse sorting of the minority needs the number of categories
of Example 1. With two categories (Section 7, "Endogeneity of Categories") the
split by human capital, variation 18, is strictly better than the split by
race, variation 50, and than each of the other five two-category partitions of
the four types. -/
theorem control_two_categories :
    catVar 5 0 45 0 + catVar 0 5 0 45 = 18 ∧
      catVar 5 5 0 0 + catVar 0 0 45 45 = 50 ∧
      18 < catVar 5 5 0 0 + catVar 0 0 45 45 ∧
      18 < catVar 5 0 0 45 + catVar 0 5 45 0 ∧
      18 < catVar 5 0 0 0 + catVar 0 5 45 45 ∧
      18 < catVar 0 5 0 0 + catVar 5 0 45 45 ∧
      18 < catVar 0 0 45 0 + catVar 5 5 0 45 ∧
      18 < catVar 0 0 0 45 + catVar 5 5 45 0 := by
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_, ?_, ?_⟩ <;>
    norm_num [catVar, varAttr, abs_of_nonneg, abs_of_nonpos]


section Objects
variable {ι : Type*}

/-- Number of objects in `S` with attribute value 1. -/
noncomputable def nPlus (S : Finset ι) (x : ι → ℝ) : ℕ := (S.filter (fun o => x o = 1)).card
/-- Number of objects in `S` without the attribute. -/
noncomputable def nMinus (S : Finset ι) (x : ι → ℝ) : ℕ := (S.filter (fun o => ¬ x o = 1)).card
/-- The attribute is binary on `S`. -/
def Binary (S : Finset ι) (x : ι → ℝ) : Prop := ∀ o ∈ S, x o = 0 ∨ x o = 1

theorem card_split (S : Finset ι) (x : ι → ℝ) :
    (nPlus S x : ℝ) + nMinus S x = S.card := by
  unfold nPlus nMinus
  exact_mod_cast Finset.filter_card_add_filter_neg_card_eq_card _

theorem sum_abs_sub {S : Finset ι} {x : ι → ℝ} (hb : Binary S x) (c : ℝ) :
    ∑ o ∈ S, |x o - c| = nPlus S x * |1 - c| + nMinus S x * |0 - c| := by
  rw [← Finset.sum_filter_add_sum_filter_not S (fun o => x o = 1)]
  have h1 : ∑ o ∈ S.filter (fun o => x o = 1), |x o - c| =
      ∑ o ∈ S.filter (fun o => x o = 1), |1 - c| :=
    Finset.sum_congr rfl (fun o ho => by rw [(Finset.mem_filter.1 ho).2])
  have h2 : ∑ o ∈ S.filter (fun o => ¬ x o = 1), |x o - c| =
      ∑ o ∈ S.filter (fun o => ¬ x o = 1), |0 - c| := by
    refine Finset.sum_congr rfl (fun o ho => ?_)
    obtain ⟨hS, hn⟩ := Finset.mem_filter.1 ho
    rcases hb o hS with h | h
    · rw [h]
    · exact absurd h hn
  rw [h1, h2, Finset.sum_const, Finset.sum_const, nsmul_eq_mul, nsmul_eq_mul]
  rfl

/-- The mean of an attribute over `S`, eqs. (1) and (2). -/
noncomputable def mean (S : Finset ι) (x : ι → ℝ) : ℝ := (∑ o ∈ S, x o) / S.card

theorem sum_eq_nPlus {S : Finset ι} {x : ι → ℝ} (hb : Binary S x) :
    ∑ o ∈ S, x o = nPlus S x := by
  have : ∑ o ∈ S, x o = ∑ o ∈ S, |x o - 0| := by
    refine Finset.sum_congr rfl (fun o ho => ?_)
    rcases hb o ho with h | h <;> simp [h]
  rw [this, sum_abs_sub hb]; simp

/-- Eq. (4) with the city-block metric. -/
noncomputable def Var {m : ℕ} (S : Finset ι) (θ : ι → Fin m → ℝ) : ℝ :=
  ∑ o ∈ S, ∑ k, |θ o k - mean S (fun o => θ o k)|

/-- The city-block distance between two objects. -/
noncomputable def d {m : ℕ} (θ : ι → Fin m → ℝ) (o o' : ι) : ℝ := ∑ k, |θ o k - θ o' k|

theorem attr_var {S : Finset ι} {x : ι → ℝ} (hb : Binary S x) (hS : S.Nonempty) :
    ∑ o ∈ S, |x o - mean S x| = 2 * nPlus S x * nMinus S x / S.card := by
  have hc := card_split S x
  have hpos : (0 : ℝ) < S.card := by exact_mod_cast hS.card_pos
  rw [sum_abs_sub hb, mean, sum_eq_nPlus hb, ← hc]
  have hp : (0 : ℝ) ≤ nPlus S x := Nat.cast_nonneg _
  have hm : (0 : ℝ) ≤ nMinus S x := Nat.cast_nonneg _
  rw [← hc] at hpos
  have e : 1 - (nPlus S x : ℝ) / (nPlus S x + nMinus S x) =
      nMinus S x / (nPlus S x + nMinus S x) := by field_simp; ring
  rw [e, zero_sub, abs_neg, abs_of_nonneg (div_nonneg hm hpos.le),
    abs_of_nonneg (div_nonneg hp hpos.le)]
  field_simp
  ring

/-- Lemma 4, in the source's generality: any finite group, any number of binary
attributes. -/
theorem lemma4 {m : ℕ} {S : Finset ι} {θ : ι → Fin m → ℝ}
    (hb : ∀ k, Binary S (fun o => θ o k)) (hS : S.Nonempty) :
    Var S θ = ∑ k, 2 * (nPlus S (fun o => θ o k) : ℝ) * (nMinus S (fun o => θ o k) : ℝ)
      / (S.card : ℝ) := by
  unfold Var
  rw [Finset.sum_comm]
  exact Finset.sum_congr rfl (fun k _ => attr_var (hb k) hS)

theorem attr_pairs {S : Finset ι} {x : ι → ℝ} (hb : Binary S x) :
    ∑ o ∈ S, ∑ o' ∈ S, |x o - x o'| = 2 * nPlus S x * nMinus S x := by
  have inner : ∀ o ∈ S, ∑ o' ∈ S, |x o - x o'| =
      nPlus S x * |1 - x o| + nMinus S x * |0 - x o| := by
    intro o _
    rw [← sum_abs_sub hb]
    exact Finset.sum_congr rfl (fun o' _ => abs_sub_comm _ _)
  rw [Finset.sum_congr rfl inner, Finset.sum_add_distrib, ← Finset.mul_sum,
    ← Finset.mul_sum]
  have a1 : ∑ o ∈ S, |1 - x o| = nMinus S x := by
    rw [Finset.sum_congr rfl (fun o _ => abs_sub_comm 1 (x o)), sum_abs_sub hb]; simp
  have a0 : ∑ o ∈ S, |0 - x o| = nPlus S x := by
    rw [Finset.sum_congr rfl (fun o _ => abs_sub_comm 0 (x o)), sum_abs_sub hb]; simp
  rw [a1, a0]; ring

/-- Lemma 3, in the source's generality. -/
theorem lemma3 {m : ℕ} {S : Finset ι} {θ : ι → Fin m → ℝ}
    (hb : ∀ k, Binary S (fun o => θ o k)) (hS : S.Nonempty) :
    Var S θ = 1 / S.card * ∑ o ∈ S, ∑ o' ∈ S, d θ o o' := by
  rw [lemma4 hb hS]
  unfold d
  have : ∑ o ∈ S, ∑ o' ∈ S, ∑ k, |θ o k - θ o' k| =
      ∑ k, ∑ o ∈ S, ∑ o' ∈ S, |θ o k - θ o' k| := by
    have h2 : ∀ o ∈ S, ∑ o' ∈ S, ∑ k, |θ o k - θ o' k| =
        ∑ k, ∑ o' ∈ S, |θ o k - θ o' k| := fun o _ => Finset.sum_comm
    rw [Finset.sum_congr rfl h2, Finset.sum_comm]
  rw [this, Finset.mul_sum]
  refine Finset.sum_congr rfl (fun k _ => ?_)
  rw [attr_pairs (hb k)]
  ring


/-- Counts add over a disjoint union, so `merge` below is the union of groups. -/
theorem nPlus_union {ι : Type*} [DecidableEq ι] {S T : Finset ι} (x : ι → ℝ) (hd : Disjoint S T) :
    nPlus (S ∪ T) x = nPlus S x + nPlus T x := by
  unfold nPlus
  rw [Finset.filter_union, Finset.card_union_of_disjoint (Finset.disjoint_filter_filter hd)]

/-- Control for Lemma 4: binary attributes are needed. -/
theorem control_lemma4_needs_binary :
    let x : Fin 3 → ℝ := ![0, 1/2, 1]
    ∑ o, |x o - mean Finset.univ x| = 1 ∧
      2 * (nPlus Finset.univ x : ℝ) * (nMinus Finset.univ x : ℝ) / 3 = 4 / 3 := by
  intro x
  have hm : mean Finset.univ x = 1 / 2 := by
    simp [mean, x, Fin.sum_univ_three]; norm_num
  have hp : Finset.univ.filter (fun o => x o = 1) = {2} := by
    ext i; fin_cases i <;> simp [x]
  have hn : Finset.univ.filter (fun o => ¬ x o = 1) = {0, 1} := by
    ext i; fin_cases i <;> simp [x]
  refine ⟨?_, ?_⟩
  · rw [hm, Fin.sum_univ_three]; simp [x]; norm_num [abs_of_nonneg, abs_of_nonpos]
  · simp only [nPlus, nMinus, hp, hn]; norm_num

end Objects


/-- A group of objects summarised, as Lemma 4 allows, by its size and, for each
attribute `k`, the numbers `n_k^+` and `n_k^-` with and without it. -/
structure Grp (m : ℕ) where
  pos : Fin m → ℝ
  neg : Fin m → ℝ
  n : ℝ
  sum_eq : ∀ k, pos k + neg k = n
  n_pos : 0 < n

/-- Eq. (4) of a group through Lemma 4. -/
noncomputable def varG {m : ℕ} (G : Grp m) : ℝ := ∑ k, 2 * G.pos k * G.neg k / G.n

/-- The union of two disjoint groups. -/
def merge {m : ℕ} (A B : Grp m) : Grp m where
  pos := fun k => A.pos k + B.pos k
  neg := fun k => A.neg k + B.neg k
  n := A.n + B.n
  sum_eq := fun k => by rw [← A.sum_eq k, ← B.sum_eq k]; ring
  n_pos := add_pos A.n_pos B.n_pos

/-- The factor (6) of Theorem 1. -/
noncomputable def factor {m : ℕ} (A B : Grp m) : ℝ :=
  ∑ k, (A.pos k * B.neg k - A.neg k * B.pos k) ^ 2 / (A.n * B.n * (A.n + B.n))

/-- The cost of merging two groups is twice the factor (6). -/
theorem theorem1_cost {m : ℕ} (A B : Grp m) :
    varG (merge A B) - varG A - varG B = 2 * factor A B := by
  unfold varG factor merge
  simp only
  rw [← Finset.sum_sub_distrib, ← Finset.sum_sub_distrib, Finset.mul_sum]
  refine Finset.sum_congr rfl (fun k _ => ?_)
  have hA := A.sum_eq k
  have hB := B.sum_eq k
  have nA := A.n_pos
  have nB := B.n_pos
  rw [← hA, ← hB] at *
  field_simp
  ring

/-- Theorem 1, in the form proved in the appendix, eq. (10). Merging `A` and `B`
gives a larger total variation than merging `C` and `D` if and only if the factor
(6) of `A, B` exceeds that of `C, D`. Any number of attributes, heterogeneous
groups. -/
theorem theorem1 {m : ℕ} (A B C D : Grp m) :
    varG C + varG D + varG (merge A B) > varG A + varG B + varG (merge C D) ↔
      factor A B > factor C D := by
  have h1 := theorem1_cost A B
  have h2 := theorem1_cost C D
  constructor <;> intro h <;> linarith

/-- Replication by `k`. -/
def scale {m : ℕ} (k : ℝ) (hk : 0 < k) (G : Grp m) : Grp m where
  pos := fun i => k * G.pos i
  neg := fun i => k * G.neg i
  n := k * G.n
  sum_eq := fun i => by rw [← G.sum_eq i]; ring
  n_pos := mul_pos hk G.n_pos

theorem varG_scale {m : ℕ} (k : ℝ) (hk : 0 < k) (G : Grp m) :
    varG (scale k hk G) = k * varG G := by
  unfold varG scale
  simp only
  rw [Finset.mul_sum]
  refine Finset.sum_congr rfl (fun i _ => ?_)
  have := G.n_pos
  field_simp

/-- Lemma 2. A `k`-fold replication preserves the ranking of any two
categorizations, given as finite families of categories. -/
theorem lemma2 {m : ℕ} {I J : Type*} [Fintype I] [Fintype J] (k : ℝ) (hk : 0 < k)
    (F : I → Grp m) (G : J → Grp m) :
    ∑ i, varG (scale k hk (F i)) ≤ ∑ j, varG (scale k hk (G j)) ↔
      ∑ i, varG (F i) ≤ ∑ j, varG (G j) := by
  simp only [varG_scale, ← Finset.mul_sum]
  exact mul_le_mul_iff_of_pos_left hk

/-- Lemma 1, the triangle step (8). If every distance between objects of a
category is at most the sum of their distances `e` to the block's type, then
`Σ Σ d ≤ 2 n Σ e`. -/
theorem lemma1_triangle {ι : Type*} (T : Finset ι) (d : ι → ι → ℝ) (e : ι → ℝ)
    (htri : ∀ o ∈ T, ∀ o' ∈ T, d o o' ≤ e o + e o') :
    ∑ o ∈ T, ∑ o' ∈ T, d o o' ≤ 2 * T.card * ∑ o ∈ T, e o := by
  calc ∑ o ∈ T, ∑ o' ∈ T, d o o' ≤ ∑ o ∈ T, ∑ o' ∈ T, (e o + e o') :=
        Finset.sum_le_sum (fun o ho => Finset.sum_le_sum (fun o' ho' => htri o ho o' ho'))
    _ = 2 * T.card * ∑ o ∈ T, e o := by
        simp only [Finset.sum_add_distrib, Finset.sum_const, nsmul_eq_mul, ← Finset.mul_sum]
        ring

/-- One category's variation, by Lemma 3, when a share `x` of a block of `n`
objects of one type joins `n₁` other objects whose distances to the block's type
sum to `D` and whose pairwise distances sum to `P`. -/
noncomputable def g (n n₁ D P x : ℝ) : ℝ := (2 * x * n * D + P) / (x * n + n₁)

theorem g_chord {n n₁ D P x : ℝ} (hn : 0 ≤ n) (h1 : 0 < n₁) (hK : 0 ≤ 2 * n₁ * D - P)
    (hx0 : 0 ≤ x) (hx1 : x ≤ 1) :
    (1 - x) * g n n₁ D P 0 + x * g n n₁ D P 1 ≤ g n n₁ D P x := by
  have a : 0 < x * n + n₁ := by positivity
  have b : 0 < n + n₁ := by linarith
  have key : g n n₁ D P x - ((1 - x) * g n n₁ D P 0 + x * g n n₁ D P 1) =
      (2 * n₁ * D - P) * n ^ 2 * x * (1 - x) / (n₁ * (n + n₁) * (x * n + n₁)) := by
    unfold g
    field_simp
    ring
  have : 0 ≤ (2 * n₁ * D - P) * n ^ 2 * x * (1 - x) / (n₁ * (n + n₁) * (x * n + n₁)) := by
    apply div_nonneg
    · have : 0 ≤ 1 - x := by linarith
      positivity
    · positivity
  linarith

/-- Lemma 1, in the case where both categories hold other objects (`n₁, n₂ > 0`).
Splitting a block of one type between two categories, a share `x` in the first,
gives a total variation no smaller than putting the whole block in one of them. -/
theorem lemma1 {n n₁ n₂ D₁ D₂ P₁ P₂ x : ℝ} (hn : 0 ≤ n) (h1 : 0 < n₁) (h2 : 0 < n₂)
    (hK1 : 0 ≤ 2 * n₁ * D₁ - P₁) (hK2 : 0 ≤ 2 * n₂ * D₂ - P₂) (hx0 : 0 ≤ x) (hx1 : x ≤ 1) :
    min (g n n₁ D₁ P₁ 0 + g n n₂ D₂ P₂ 1) (g n n₁ D₁ P₁ 1 + g n n₂ D₂ P₂ 0) ≤
      g n n₁ D₁ P₁ x + g n n₂ D₂ P₂ (1 - x) := by
  have c1 := g_chord hn h1 hK1 hx0 hx1
  have c2 := g_chord (x := 1 - x) hn h2 hK2 (by linarith) (by linarith)
  simp only [sub_sub_cancel] at c2
  rcases le_total (g n n₁ D₁ P₁ 0 + g n n₂ D₂ P₂ 1) (g n n₁ D₁ P₁ 1 + g n n₂ D₂ P₂ 0) with h | h
  · rw [min_eq_left h]; nlinarith
  · rw [min_eq_right h]; nlinarith

/-- Control for Lemma 1: without the triangle inequality (`2 n₁ D − P < 0`) an
interior split can beat both endpoints. -/
theorem control_lemma1_needs_triangle :
    g 1 1 0 1 (1/2) + g 1 1 0 1 (1 - 1/2) <
      min (g 1 1 0 1 0 + g 1 1 0 1 1) (g 1 1 0 1 1 + g 1 1 0 1 0) := by
  norm_num [g]

/-- Proposition 2, equidistant world. Adding `nn` new identical objects, at
distance 1 from every existing object, to category `i` (size `nᵢ`, variation
`Vᵢ`) raises the total variation by no more than adding them to `j` if and only
if `(Vⱼ + 2nn)/(nⱼ + nn) ≤ (Vᵢ + 2nn)/(nᵢ + nn)`. The new variation is
`(nᵢ Vᵢ + 2 nᵢ nn)/(nᵢ + nn)` by Lemma 3. -/
theorem prop2 {ni nj Vi Vj nn : ℝ} (hi : 0 < ni) (hj : 0 < nj) (hnn : 0 < nn) :
    (ni * Vi + 2 * ni * nn) / (ni + nn) - Vi ≤ (nj * Vj + 2 * nj * nn) / (nj + nn) - Vj ↔
      (Vj + 2 * nn) / (nj + nn) ≤ (Vi + 2 * nn) / (ni + nn) := by
  have ei : (ni * Vi + 2 * ni * nn) / (ni + nn) - Vi = nn * (2 - (Vi + 2 * nn) / (ni + nn)) := by
    have : ni + nn ≠ 0 := by linarith
    field_simp; ring
  have ej : (nj * Vj + 2 * nj * nn) / (nj + nn) - Vj = nn * (2 - (Vj + 2 * nn) / (nj + nn)) := by
    have : nj + nn ≠ 0 := by linarith
    field_simp; ring
  rw [ei, ej, mul_le_mul_iff_of_pos_left hnn]
  constructor <;> intro h <;> linarith

/-- Two vertices of the cube are adjacent if they differ in exactly one
coordinate. -/
def adjacent {x : ℕ} (v w : Fin x → Bool) : Prop :=
  (Finset.univ.filter (fun i => v i ≠ w i)).card = 1

/-- The fact used in the proof of Lemma 5. Any set of more than `2^y` vertices of
the cube `{0,1}^(y+1)` contains two adjacent vertices. -/
theorem hypercube_adjacent {y : ℕ} (A : Finset (Fin (y + 1) → Bool)) (h : 2 ^ y < A.card) :
    ∃ v ∈ A, ∃ w ∈ A, v ≠ w ∧ adjacent v w := by
  have hc : (Finset.univ : Finset (Fin y → Bool)).card < A.card := by
    rw [Finset.card_univ, Fintype.card_fun, Fintype.card_bool, Fintype.card_fin]; exact h
  obtain ⟨v, hv, w, hw, hne, heq⟩ := Finset.exists_ne_map_eq_of_card_lt_of_maps_to hc
    (f := fun v : Fin (y + 1) → Bool => fun i : Fin y => v i.succ)
    (fun _ _ => Finset.mem_univ _)
  refine ⟨v, hv, w, hw, hne, ?_⟩
  have hs : ∀ j : Fin y, v j.succ = w j.succ := fun j => congrFun heq j
  have h0 : v 0 ≠ w 0 := by
    intro h0
    apply hne
    funext i
    refine Fin.cases h0 (fun j => hs j) i
  have : Finset.univ.filter (fun i => v i ≠ w i) = {0} := by
    ext i
    simp only [Finset.mem_filter, Finset.mem_univ, true_and, Finset.mem_singleton]
    constructor
    · intro hi
      refine Fin.cases (motive := fun i => v i ≠ w i → i = 0) (fun _ => rfl)
        (fun j hj => absurd (hs j) hj) i hi
    · rintro rfl; exact h0
  unfold adjacent
  rw [this, Finset.card_singleton]

/-- Control for the fact: the bound is tight. In `{0,1}²`, two opposite vertices
are `2^1` vertices with no adjacent pair. -/
theorem control_hypercube_tight :
    ¬ adjacent (![false, false] : Fin 2 → Bool) ![true, true] := by
  unfold adjacent; decide

/-- Lemma 5, the counting step. If `c` categories hold only minority types, each
holding at least one type and `s` of them exactly one, and there are `t ≤ N/2`
minority types, then `c > (3/8) N` gives `s > N/4`, so more than half of the
minority types have their own category. -/
theorem lemma5_count {s c t N : ℝ} (hst : s + 2 * (c - s) ≤ t) (hc : 3 / 8 * N < c)
    (ht : t ≤ N / 2) : N / 4 < s := by
  linarith

/-- Lemma 5, assembled. With `N = 2^(y+2)` types, the minority types form the
cube `{0,1}^(y+1)`. If more than `N/4 = 2^y` of them each have a category of
their own, two of these are adjacent. -/
theorem lemma5 {y : ℕ} (A : Finset (Fin (y + 1) → Bool)) {c t : ℝ}
    (hst : (A.card : ℝ) + 2 * (c - A.card) ≤ t) (hc : 3 / 8 * (2 : ℝ) ^ (y + 2) < c)
    (ht : t ≤ (2 : ℝ) ^ (y + 2) / 2) :
    ∃ v ∈ A, ∃ w ∈ A, v ≠ w ∧ adjacent v w := by
  apply hypercube_adjacent
  have := lemma5_count hst hc ht
  have e : (2 : ℝ) ^ (y + 2) / 4 = 2 ^ y := by rw [pow_add]; norm_num
  rw [e] at this
  exact_mod_cast this

/-- Corollary 1, the last step of the proof. With `n₂ ≤ n₁ ≤ r_I n₄` (both
minority groups, `n₄` the smallest), `n₃/n₄ ≥ r_E r_I` and `r_E (2 − r_I) > 1`,
the merging criterion `1/n₁ + 1/n₂ > 1/n₃ + 1/n₄` holds. The bound `n₁ ≤ r_I n₄` is
used and not stated in the printed proof (FJ-D4). -/
theorem corollary1_step {n1 n2 n3 n4 rE rI : ℝ} (h1 : 0 < n1) (h2 : 0 < n2) (h3 : 0 < n3)
    (h4 : 0 < n4) (h21 : n2 ≤ n1) (h1I : n1 ≤ rI * n4) (hE : 0 < rE)
    (h34 : rE * rI ≤ n3 / n4) (hc : 1 < rE * (2 - rI)) :
    1 / n3 + 1 / n4 < 1 / n1 + 1 / n2 := by
  have hrI : 0 < rI := by
    by_contra h; push_neg at h; nlinarith
  have h2I : 0 < 2 - rI := by
    by_contra h; push_neg at h; nlinarith
  set yv := n3 / n4 with hy
  have hn3 : n3 = yv * n4 := by rw [hy]; field_simp
  have step1 : 2 / n1 ≤ 1 / n1 + 1 / n2 := by
    have : 1 / n1 ≤ 1 / n2 := one_div_le_one_div_of_le h2 h21
    have e2 : 2 / n1 = 1 / n1 + 1 / n1 := by ring
    linarith
  have step2 : 2 * n3 / n1 ≥ 2 * yv / rI := by
    rw [hn3, ge_iff_le, div_le_div_iff₀ hrI h1]
    nlinarith [mul_pos (mul_pos hrI h4) (show 0 < yv by rw [hy]; positivity)]
  have step3 : 2 * yv / rI - yv ≥ rE * (2 - rI) := by
    have : 2 * yv / rI - yv = yv * (2 - rI) / rI := by field_simp
    rw [this, ge_iff_le, le_div_iff₀ hrI]
    nlinarith
  have key : 1 < 2 * n3 / n1 - n3 / n4 := by rw [← hy] ; linarith
  have : 1 / n3 + 1 / n4 < 2 / n1 := by
    have e : 2 * n3 / n1 - n3 / n4 = n3 * (2 / n1 - 1 / n4) := by ring
    rw [e] at key
    have : 1 / n3 < 2 / n1 - 1 / n4 := by
      rw [div_lt_iff₀ h3]; linarith
    linarith
  linarith

/-- Control for Corollary 1's step: with `r_E (2 − r_I) ≤ 1` it fails. Here
`r_E = 1`, `r_I = 19/10`, `n₁ = n₂ = n₃ = 19/10`, `n₄ = 1`. -/
theorem control_corollary1 :
    (1 : ℝ) * (2 - 19/10) ≤ 1 ∧ (19/10 : ℝ) ≤ 19/10 * 1 ∧ (1 : ℝ) * (19/10) ≤ (19/10) / 1 ∧
      ¬ (1 / (19/10 : ℝ) + 1 / 1 < 1 / (19/10) + 1 / (19/10)) := by
  norm_num


/-- Section 3. With the `p_k V_k` coded equal to `c > 0`, the expected utility of
a categorization is `c (N m) − c D`, `D` its total within-category distance, so
expected utility ranks categorizations in the reverse order of eq. (5). -/
theorem eu_iff_var {c Nm Df Dg : ℝ} (hc : 0 < c) :
    c * Nm - c * Dg ≤ c * Nm - c * Df ↔ Df ≤ Dg := by
  constructor
  · intro h; nlinarith
  · intro h; nlinarith

/-- Example 2. The race-based categorization earns `p_sV_s 100 + p_eV_e 95` and
the human-capital one `p_sV_s 91 + p_eV_e 100`, computed object by object from
the prototypes, and the second is better if and only if `p_eV_e > 1.8 p_sV_s`. -/
theorem example2 (x y : ℝ) :
    (100 : ℝ) = 50 + 45 + 5 ∧
      (95 : ℝ) = 90 + 10 * (1 - |(1 : ℝ) - 1/2|) ∧
      (91 : ℝ) = 50 + 45 * (1 - |(1 : ℝ) - 9/10|) + 5 * (1 - |(0 : ℝ) - 9/10|) ∧
      (x * 100 + y * 95 < x * 91 + y * 100 ↔ 18/10 * x < y) := by
  refine ⟨by norm_num, by norm_num [abs_of_nonneg], by norm_num [abs_of_nonneg, abs_of_nonpos], ?_⟩
  constructor <;> intro h <;> linarith

/-- Control for the equivalence of Section 3: with unequal weights the two
rankings part. At `p_sV_s = 1`, `p_eV_e = 2` the human-capital categorization
has the higher expected utility, 291 against 290, and the larger variation, 9
against 5 (`example1_table`). -/
theorem control_eu_unequal_weights :
    (1 : ℝ) * 100 + 2 * 95 < 1 * 91 + 2 * 100 ∧ catVar 5 5 0 0 < catVar 5 0 45 0 := by
  refine ⟨by norm_num, ?_⟩
  obtain ⟨a, b, -, -, -, -⟩ := example1_table
  rw [a, b]; norm_num

/-- Example 3. The printed categorization of the 1010 consumers, `{(1,1,1),
(1,0,1)}`, `{(1,1,0), (1,0,0)}` and all `(0,·,·)`, has total variation `12201/101`
(about 120.80) by Lemma 4. Its optimality among all three-category partitions was
checked by script only. -/
theorem example3_value :
    varAttr 400 4 + varAttr 400 4 + (varAttr 200 2 + varAttr 101 101) = 12201 / 101 := by
  norm_num [varAttr, abs_of_nonneg, abs_of_nonpos]

/-- Example 4, the two printed distances, which are reproduced: `160/230` rounds
to .696 and `3 · 28/118` to .712. -/
theorem example4_distances :
    |(160 : ℝ) / 230 - 0.696| < 0.0005 ∧ |(3 * 28 : ℝ) / 118 - 0.712| < 0.0005 := by
  constructor <;> norm_num [abs_lt]

/-- Proposition 1 from Theorem 1. For homogeneous groups the factor (6) has, on
each attribute where they differ, the term `(nᵢ nⱼ)²/(nᵢ nⱼ (nᵢ + nⱼ))`, and
twice it is the variation that attribute adds, `varAttr nᵢ nⱼ`. On an attribute
where they agree the term is 0. -/
theorem prop1_from_theorem1 {ni nj : ℝ} (hi : 0 < ni) (hj : 0 < nj) :
    2 * ((ni * nj - 0 * 0) ^ 2 / (ni * nj * (ni + nj))) = varAttr ni nj ∧
      (ni * 0 - 0 * nj) ^ 2 / (ni * nj * (ni + nj)) = 0 := by
  rw [lemma4_one hi.le hj.le (by linarith)]
  constructor
  · field_simp; ring
  · simp

end FryerJackson2008
