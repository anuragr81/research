/-
# Barberis (2012), "A Model of Casino Gambling"

*Management Science* 58(1), 35-51.  DOI 10.1287/mnsc.1110.1435.

Source read in full: the journal version (17 pp.); `p.N` is the journal page.

## The model (pp.39-42)

At each date `t = 0, …, T-1` the agent may play a 50:50 bet to win or lose `$h`;
the casino is a binomial tree whose node `(t, j)`, `j = 1, …, t+1` counted from
the top, carries accumulated winnings `h(t + 2 - 2j)` (p.46, eq. 14).  The agent
"decides what to do by maximizing the cumulative prospect theory value of his
accumulated winnings or losses at the moment he leaves the casino" (p.40), with
`v(x) = x^α` on gains and `w(P) = P^δ/(P^δ + (1-P)^δ)^{1/δ}`, `α, δ ∈ (0,1)`
(eqs. 5-6, p.38).  A plan maps each node to exit or continue (p.41); "his
planned action at time t depends only on his accumulated winnings at that time
and not on the path by which he accumulated those winnings" (fn. 13, p.42).

## What is formalized

  * **Winnings at a node** (eq. 14): `winnings`, top and bottom nodes, and the
    node a path of bets reaches (`node`, `winnings_of_path`).
  * **Path independence** (fn. 13): a plan's action depends on the path only
    through its node, so two paths with the same numbers of wins and losses in
    any sequence get the same action (`plan_path_independent`,
    `same_node_reversed`).
  * **The exit strategy of Figure 2** (p.42): gamble until `T = 5` or until node
    `(3,1)`; its accumulated winnings are $30, $10, -$10, -$30, -$50 with
    probabilities 7, 9, 10, 5, 1 in 32 (`fig2_distribution`), checked over all
    32 paths.
  * **The time inconsistency at node (4,1)** (p.41).  The agent leaves iff
    `v(40) ≥ v(50) w(1/2) + v(30)(1 - w(1/2))` (7), equivalently
    `v(40) - v(30) ≥ (v(50) - v(30)) w(1/2)` (8) (`cond7_iff_cond8`); and "it is
    straightforward to check that condition (8) holds for all α, δ ∈ (0,1)":
    `cond8_holds`, from `w(1/2) ≤ 1/2` (`w_half_le`) and the concavity of `x^α`.

## What is not formalized

The numerical solutions of problems (9), (13), (16) (Figures 3-5); Proposition 1
and Corollary 1 are checked numerically in `sympy/check_barberis2012.py`
(condition (12) first holds at T = 26 for (α, δ, λ) = (0.88, 0.65, 2.25)); the
online supplement.
-/
import Mathlib

namespace Literature.Barberis

/-! ## Nodes and winnings -/

/-- Accumulated winnings at node `(t, j)` (eq. 14, p.46). -/
def winnings (h : ℝ) (t j : ℕ) : ℝ := h * ((t : ℝ) + 2 - 2 * (j : ℝ))

theorem winnings_top (h : ℝ) (t : ℕ) : winnings h t 1 = h * t := by
  simp only [winnings]; push_cast; ring

theorem winnings_bottom (h : ℝ) (t : ℕ) : winnings h t (t + 1) = -(h * t) := by
  simp only [winnings]; push_cast; ring

/-- The node reached after a path of bets (`true` = won): the date is the number
of bets and `j` is the number of losses plus one. -/
def node (path : List Bool) : ℕ × ℕ := (path.length, path.count false + 1)

theorem count_true_add_count_false (path : List Bool) :
    path.count true + path.count false = path.length := by
  induction path with
  | nil => simp
  | cons b l ih => cases b <;> simp <;> omega

/-- The winnings of a path are those of the node it reaches. -/
theorem winnings_of_path (h : ℝ) (path : List Bool) :
    h * ((path.count true : ℝ) - path.count false)
      = winnings h (node path).1 (node path).2 := by
  have e := count_true_add_count_false path
  simp only [winnings, node]
  have : (path.length : ℝ) = path.count true + path.count false := by exact_mod_cast e.symm
  rw [this]; push_cast; ring

/-! ## Path independence (fn. 13) -/

/-- A plan maps each node to continue (`true`) or exit (`false`) (p.41). -/
abbrev Plan := ℕ × ℕ → Bool

/-- A plan's action depends on the path only through the node: paths with the
same length and the same number of losses, in whatever sequence, get the same
action. -/
theorem plan_path_independent (s : Plan) (p q : List Bool) (hl : p.length = q.length)
    (hc : p.count false = q.count false) : s (node p) = s (node q) := by
  simp [node, hl, hc]

/-- A win then a loss reaches the same node as a loss then a win. -/
theorem same_node_reversed : node [true, false] = node [false, true] := by decide

/-! ## The exit strategy of Figure 2 (p.42) -/

/-- All paths of `n` bets. -/
def allPaths : ℕ → List (List Bool)
  | 0 => [[]]
  | n + 1 => (allPaths n).flatMap (fun l => [true :: l, false :: l])

/-- Accumulated winnings (in units of $10) on leaving under the Figure 2 strategy:
leave at node (3,1) after three straight wins, otherwise at `T = 5`. -/
def fig2Exit (path : List Bool) : ℤ :=
  if path.take 3 = [true, true, true] then 3
  else (path.count true : ℤ) - path.count false

theorem allPaths_five_length : (allPaths 5).length = 32 := by decide

/-- **Figure 2's distribution**: $30, $10, -$10, -$30, -$50 with probabilities
7/32, 9/32, 10/32, 5/32, 1/32, and $50 is never reached. -/
theorem fig2_distribution :
    (allPaths 5).countP (fun p => fig2Exit p = 3) = 7 ∧
    (allPaths 5).countP (fun p => fig2Exit p = 1) = 9 ∧
    (allPaths 5).countP (fun p => fig2Exit p = -1) = 10 ∧
    (allPaths 5).countP (fun p => fig2Exit p = -3) = 5 ∧
    (allPaths 5).countP (fun p => fig2Exit p = -5) = 1 ∧
    (allPaths 5).countP (fun p => fig2Exit p = 5) = 0 := by
  decide

/-! ## The time inconsistency at node (4,1) (p.41) -/

/-- The weighting function (6), p.38. -/
noncomputable def w (δ P : ℝ) : ℝ := P ^ δ / (P ^ δ + (1 - P) ^ δ) ^ (1 / δ)

/-- `w(1/2) ≤ 1/2` for `δ ∈ (0,1]`: an even chance is not overweighted. -/
theorem w_half_le {δ : ℝ} (hδ0 : 0 < δ) (hδ1 : δ ≤ 1) : w δ (1 / 2) ≤ 1 / 2 := by
  unfold w
  have hhalf : (1 : ℝ) - 1 / 2 = 1 / 2 := by norm_num
  rw [hhalf]
  set a : ℝ := (1 / 2 : ℝ) ^ δ with ha
  have ha0 : 0 < a := Real.rpow_pos_of_pos (by norm_num) δ
  have h2a : 1 ≤ 2 * a := by
    have : (1 / 2 : ℝ) ^ (1 : ℝ) ≤ (1 / 2 : ℝ) ^ δ :=
      Real.rpow_le_rpow_of_exponent_ge (by norm_num) (by norm_num) hδ1
    rw [Real.rpow_one] at this
    linarith
  have hexp : (1 : ℝ) ≤ 1 / δ := by rw [le_div_iff₀ hδ0]; linarith
  have hpow : 2 * a ≤ (2 * a) ^ (1 / δ) := by
    have := Real.rpow_le_rpow_of_exponent_le h2a hexp
    rwa [Real.rpow_one] at this
  rw [show a + a = 2 * a by ring]
  have hpos : 0 < 2 * a := by linarith
  calc a / (2 * a) ^ (1 / δ) ≤ a / (2 * a) := div_le_div_of_nonneg_left ha0.le hpos hpow
    _ = 1 / 2 := by field_simp

/-- The value function on gains, `v(x) = x^α` (eq. 5). -/
noncomputable def v (α x : ℝ) : ℝ := x ^ α

/-- Condition (7), leaving at node (4,1), is condition (8). -/
theorem cond7_iff_cond8 (α δ : ℝ) :
    v α 40 ≥ v α 50 * w δ (1 / 2) + v α 30 * (1 - w δ (1 / 2)) ↔
      v α 40 - v α 30 ≥ (v α 50 - v α 30) * w δ (1 / 2) := by
  constructor <;> intro h <;> linarith

/-- **"Condition (8) holds for all α, δ ∈ (0,1)"** (p.41): once at node (4,1)
the agent leaves, contrary to his time-0 plan. -/
theorem cond8_holds {α δ : ℝ} (hα0 : 0 < α) (hα1 : α < 1) (hδ0 : 0 < δ) (hδ1 : δ < 1) :
    v α 40 - v α 30 ≥ (v α 50 - v α 30) * w δ (1 / 2) := by
  have hc := (Real.strictConcaveOn_rpow hα0 hα1).concaveOn
  have hmid := hc.2 (show (50 : ℝ) ∈ Set.Ici 0 by norm_num) (show (30 : ℝ) ∈ Set.Ici 0 by norm_num)
    (show (0 : ℝ) ≤ 1 / 2 by norm_num) (show (0 : ℝ) ≤ 1 / 2 by norm_num) (by norm_num)
  simp only [smul_eq_mul] at hmid
  rw [show (1 / 2 : ℝ) * 50 + 1 / 2 * 30 = 40 by norm_num] at hmid
  have hmono : (30 : ℝ) ^ α ≤ (50 : ℝ) ^ α :=
    Real.rpow_le_rpow (by norm_num) (by norm_num) hα0.le
  have hw := w_half_le hδ0 hδ1.le
  unfold v
  calc ((50 : ℝ) ^ α - 30 ^ α) * w δ (1 / 2) ≤ ((50 : ℝ) ^ α - 30 ^ α) * (1 / 2) :=
        mul_le_mul_of_nonneg_left hw (by linarith)
    _ ≤ (40 : ℝ) ^ α - 30 ^ α := by linarith

end Literature.Barberis
