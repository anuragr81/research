/-
  Fu, Jiao & Lu (2015), "Contests with endogenous entry",
  International Journal of Game Theory 44:387-424.

  LITERATURE.tex, section "Endogenous entry into contests", says of P7:

      "That is the same accounting logic as P7. P7's content is therefore not
       a new inequality but the deterministic, heterogeneous, utility-unit
       version of a known one [...] The paper should say exactly that."

  This file makes "the same accounting logic" precise instead of asserting it.
  One abstract lemma is proved, and BOTH bounds are derived as instances of
  it, so the shared content and the residual differences are both visible.

  Their equation (2), IJGT 44 p.397:

      [1 - (1-q)^M] V  >=  Mq (Delta + E(x^alpha))

  Rent dissipated is at most the prize mass; entrants pay entry cost Delta
  plus effort cost E(x^alpha). Since the bracket is at most 1 and effort cost
  is non-negative, the expected entrant count Mq obeys Mq * Delta <= V.

  P7 (PROOFS.tex): Delta(m) <= V/(m+1), so entry at index m forces
  (m+1) * kappa <= V.

  Everything here is linear integer arithmetic over `Int`. No Mathlib. The
  probabilistic content of (2) -- that the bracket is a probability and that
  E(x^alpha) is an expectation -- is NOT formalised; it enters as the
  hypotheses `rent_le_budget` and `effort_nonneg`, in the same explicit-
  hypothesis style as `step_nonpos` in EntryContest.lean.
-/

namespace FuJiaoLu

/-- **FJL-L1. The accounting lemma.**
    If a count's unit costs plus some non-negative residual cost are covered
    by dissipated rent, and that rent is capped by the budget, then the count
    times the unit cost is capped by the budget.

    This is the whole of "the accounting logic": everything else in either
    paper is interpretation of the three hypotheses. -/
theorem accounting_bound
    (count unitCost residual rent budget : Int)
    (hcount : 0 ≤ count)
    (hresidual : 0 ≤ residual)
    (hdissipation : count * (unitCost + residual) ≤ rent)
    (hbudget : rent ≤ budget) :
    count * unitCost ≤ budget := by
  have hexp : count * (unitCost + residual) = count * unitCost + count * residual :=
    Int.mul_add count unitCost residual
  have hnn : 0 ≤ count * residual := Int.mul_nonneg hcount hresidual
  omega

/-- **FJL-L2. Their equation (2) gives the expected-count bound.**
    `bracket` stands for `[1 - (1-q)^M] V`, capped by `V` because the bracket
    is a probability; `mq` is the expected number of entrants `Mq`; `eff` is
    `E(x^alpha)`. The conclusion `mq * delta <= V` is the bound
    `Mq <= V/Delta` in multiplicative form. -/
theorem fjl_expected_count_bound
    (mq delta eff bracket V : Int)
    (hmq : 0 ≤ mq)
    (heff : 0 ≤ eff)
    (heq2 : mq * (delta + eff) ≤ bracket)
    (hbracket : bracket ≤ V) :
    mq * delta ≤ V :=
  accounting_bound mq delta eff bracket V hmq heff heq2 hbracket

/-- **FJL-L3. P7's cap is the same lemma.**
    Here the count is `m+1`, the unit cost is `kappa`, and the residual is
    zero: the entry_contest model has no bidding stage, so there is no effort
    cost to carry. `dissipation` stands for `(m+1) * Delta m`. -/
theorem p7_entry_count_bound
    (m : Nat) (kappa dissipation V : Int)
    (hdiss : ((m : Int) + 1) * (kappa + 0) ≤ dissipation)
    (hbudget : dissipation ≤ V) :
    ((m : Int) + 1) * kappa ≤ V :=
  accounting_bound ((m : Int) + 1) kappa 0 dissipation V
    (by omega) (by omega) hdiss hbudget

/-- **FJL-L4. What actually differs.**
    The shared lemma is silent about the *nature* of the count. FJL's `mq` is
    an expected count and need not be an integer; P7's count is `m+1`, an
    integer arising from a pure-strategy profile. This theorem records the
    integrality that P7 has and FJL's bound does not: the cap on a natural
    count yields a genuine finite index bound. It is the formal residue of
    "deterministic ... version of a known one". -/
theorem p7_count_is_integral
    (m : Nat) (kappa V : Int) (hkappa : 0 < kappa)
    (h : ((m : Int) + 1) * kappa ≤ V) :
    (m : Int) + 1 ≤ V := by
  have h1 : (m : Int) + 1 ≤ ((m : Int) + 1) * kappa := by
    have : ((m : Int) + 1) * 1 ≤ ((m : Int) + 1) * kappa :=
      Int.mul_le_mul_of_nonneg_left (by omega) (by omega)
    omega
  omega

end FuJiaoLu
