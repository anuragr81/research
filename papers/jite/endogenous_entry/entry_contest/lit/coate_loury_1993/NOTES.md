# Notes — Coate and Loury (1993), pass 1 (AER, read in full)

Run `python3 lit/coate_loury_1993/verify_coate_loury.py` from the repository
root.

## Verdict for C12, C13, C6 and C10

- **C12** (the sign of M28 on the entry margin, for a rule that weighs talent
  more, is new): narrowed, and no longer pending.
  - What the paper has: a binary investment, read through a noisy test, in which
    those who do not invest still compete. In its uniform example, at a fixed
    employer policy, both investment rates fall as the test becomes less accurate
    (`rates_fall_with_band`), the direction of M28.
  - What it does not have: a fixed prize, a rank, a weight on talent, or a fee.
    The standard is chosen by employers and moves with their beliefs. The
    direction is a by-product of the formulas of one example, not a stated
    comparative static.
  - C12 must cite it and claim the sign for a fixed prize awarded by rank, with
    a fixed fee, for every law of talent and of the bought component.
  - With this paper read, C12 waits on nothing, so it moves to the settled table.
- **C13** (the payers' share as a function of how many pay): unaffected.
  - The paper splits the assignment rate between investors and non-investors,
    `ρ(s, π) = π[1 - F_q(s)] + (1 - π)[1 - F_u(s)]`. It analyses that rate as a
    function of the standard.
  - It does not analyse the share of task-one places held by investors as a
    function of how many invest.
  - The number of places is not fixed, so there is no prize to share.
  - C13 still waits on Corcoran and Karels (1985).
- **C6** (count invariance): a contrast. With heterogeneous private costs and a
  binary investment, the investment rate is not unique (Proposition 1, CO-9),
  because the employer's standard responds to the rate it expects. With the rule
  fixed, the rate (Fryer and Loury) or the count (M6) is pinned.
- **C10**: with unequal cost laws, B's invest less and do less well (CO-26,
  CO-27). This is a further instance of the group gap cited from Fryer and Loury
  (2013).

## Findings from the formalisation

1. The convergence of (9) to the patronizing belief is asserted ("The reader can
   easily verify"). It holds from every initial belief in [0, π_ℓ)
   (`iter_tendsto`), and the slopes at the two fixed points are (1-π_ℓ)/π_ℓ < 1
   and π_ℓ/(1-π_ℓ) > 1 (`patronizing_stable`, `colorblind_unstable`).
2. Footnote 21's numbers are exactly Proposition 3's hypotheses when r = 2/3, as
   the page image reads (`fn21`). With the r = 3 of the text layer they are
   inconsistent (CO-D1).
3. The derivative behind eq. (5) is f_q(φ(η - 1) - η) (`rhoHat_deriv_eq`).
   Where φ ≤ 1 it is negative unconditionally (`rhoHat_deriv_neg_of_le_one`),
   which is the paper's remark that the rate necessarily falls on [ŝ, 1].
4. No misprint was found.
