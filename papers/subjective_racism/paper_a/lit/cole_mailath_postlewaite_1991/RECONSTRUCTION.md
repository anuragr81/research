# Cole, Mailath and Postlewaite (1991), reconstruction

Harold L. Cole, George J. Mailath and Andrew Postlewaite, "Social Norms,
Savings Behavior, and Growth", CARESS Working Paper 91-14, University of
Pennsylvania, June 1990, revised June 1991. A version was published in the
*Journal of Political Economy* in 1992. The journal version is not read, its
volume and pages are not checked here, and the record is cited as the working
paper (CM-D1).

## Source read

- A scan of the working paper, 36 PDF pages, with a text layer. Cached at
  `~/.cache/paper_a/sources/cole_mailath_postlewaite_1992_wp.pdf`
  (1,475,712 bytes), sha256 `48ec38df…974d`, and its extracted text
  `cole_mailath_postlewaite_1992_wp.txt`, sha256 `bada0e26…3e74`. The file
  name says 1992; the cover says CARESS 91-14, revised June 1991.
- The text of every page, PDF pages 1 to 36, was read on 16 October 2026,
  so the evidence tag is [F]. The text garbles most equations, so PDF pages
  2, 7, 9, 10, 12, 13, 14, 18, 19, 20, 22, 23, 24, 34 and 36 were also
  rendered at 110 dpi with `pdftoppm` and read from the image. Every
  equation used below is taken from an image.
- Page numbers in this record are PDF pages. PDF page 1 is the cover, PDF
  page 2 is unnumbered, and PDF page `n` carries the printed number `n − 1`
  for `n` from 3 to 35. PDF page 36 repeats the reference list in another
  style (CM-D6).

## The model, from its own primitives

1. **Agents** (PDF pp. 6–7). A continuum of one-period-lived men indexed by
   `i ∈ [0, 1]` and women indexed by their endowment `j`, uniform on
   `[0, 1]`, of a non-traded good. Each pair has one son and one daughter. The
   son inherits the father's index (the family line). Only the son's welfare
   enters the parents' utility. Utility is `u(c) + j` plus `β` times the son's
   utility, with `u` CRRA of degree `γ`.
2. **Information and preferences over mates** (PDF p. 7). Matching is
   voluntary and "based upon complete current information" (CM-Q4). Men
   prefer higher `j`. Women prefer men who give high consumption or whose sons
   will have high status. All agents share these preferences (CM-Q2). No
   characteristic is hidden, and no one infers anything.
3. **Status and social norm** (PDF pp. 4, 7). Status is a ranking of men that
   determines the outcome of matching (CM-Q1, CM-Q5). A social norm is a
   status assignment rule together with status-dependent matching behaviour.
   An equilibrium is a social norm and market decisions from which no agent
   wishes to deviate. Status does not enter utility. It matters only through
   the matches it brings in equilibrium (CM-Q3).
4. **Matching protocol** (PDF pp. 7–8). Each man may propose to any woman.
   A woman takes the most attractive offer. A man whose offer fails is matched
   with a zero-endowment woman.
5. **Technology** (PDF pp. 8, 12). `c = Ak − k'`, `A > 1`, with
   `1 < Aβ < A^γ`.
6. **Exogenous matching** (PDF pp. 12–13). Eq. (3) is the standard growth
   problem with the term `j_t` added. The Euler equation (4) and the
   consumption ratio give eq. (6),
   `λ_{t+1}^γ = λ_t^γ/(1 − λ_t)^γ · A^{1−γ} β`, whose stationary solution is
   `λ* = 1 − (A^{1−γ} β)^{1/γ}`. Eq. (8) gives `V*(k_0)`.
7. **Wealth-is-status equilibrium** (PDF pp. 13–16). A woman accepts the
   wealthiest proposer, so a man's match depends only on his rank in the
   capital distribution (CM-Q6). With `m_{t+1}(k) = k_{t+1}^{-1}(k)`, the
   Bellman equation (9) and the Euler equation (10),
   `u'(Ak_t − k_{t+1}) = β{A u'(Ak_{t+1} − k_{t+2}) + m'_{t+1}(k_{t+1})}`,
   add a matching term to (4). Bequests rise when the match is more sensitive
   to capital (CM-Q7), so a more equal capital distribution raises saving.
   Proposition I gives existence (Appendix A.1, by Mas-Colell 1984).
   Properties 1 to 6: no family changes rank, every family saves at least
   as much as without matching, no atoms in the capital distribution,
   continuity, over-saving vanishing for `γ < 1`, and for `γ > 1` either the
   poorer line's consumption ratio goes to 0 or the wealth ratio diverges.
8. **Two-period example** (PDF pp. 8–11). With equal initial capital all men
   get `V(0)`, sons match with different women, and the consumption share
   `λ(j)` falls in `j`. Saving rises with matching, and the more compact the
   wealth distribution, the higher the saving.
9. **Aristocratic equilibrium** (PDF pp. 17–21). A man's status is his
   father's if the father matched with the woman of endowment equal to his
   status, and zero forever otherwise (eq. 12, CM-Q8). On the path
   `V(k, s) = V*(k) + s/(1 − β)`, and saving equals the exogenous-matching
   path. A woman `j` is indifferent between her prescribed match with capital
   `k_t(j)` and a man with capital `L_t(j)` at
   `V*(L_t(j)) + j = V*(k_t(j)) + j/(1 − β)`. A man `i` does not seek a
   woman `j` if `(1 − β) j ≤ i`, eq. (13), which fails for low-status men. The
   Lemma (for `γ > 1`, `β` near 1) keeps everyone above an index `i'` in
   line. Proposition II gives existence when `k_0(0) = 0` and capital is
   "spread out" at the bottom, so that no low-status man can bequeath
   `L_t(j)`. The equilibrium is a Nash equilibrium, not necessarily subgame
   perfect. A proportional rather than total loss of status also works.
10. **Hybrid equilibrium** (PDF pp. 22–25). Men and women above an index
    `i` form an upper group that saves as under exogenous matching and matches
    at random within `[i, 1]` (CM-Q10). Deviators are banished, with their
    line, to the lower group, where matching follows wealth. The expected
    value of a match in the upper group is the group mean `(1 + i)/2`, printed
    as `(1 − i)/2` (CM-Q11, CM-D2), and banishment costs at least
    `(1 − i)/2`. Proposition III gives existence for `γ > 1` when the upper
    group's initial capital is high enough. Low-status men compete by wealth
    and high-status men by birth (CM-Q12). Men below the upper group's mean
    gain from the hybrid relative to wealth-is-status, and the man at the top
    loses.
11. **Discussion** (PDF pp. 25–29). Matching is chosen because a good
    allocated by status rather than by price would need an account of why the
    allocator does not sell it (CM-Q13). Section 5.6 leaves the origin of
    norms open.

## Derivation chain, as formalised

- **Two-period example.** The first-order condition of (2) in ratio form,
  `((1 − λ)/λ)^γ = β A^{1−γ}`, is solved by `λ(0)` (`lambda0_foc`). Since
  `V(j) = V(0)` fixes `g(λ(j)) = V(0) − β j` with `g` rising below `λ(0)`,
  `λ(j)` falls in `j` (`lambda_j_antitone`).
- **Exogenous matching.** Stationarity of (6) is `(1 − λ*)^γ = A^{1−γ} β`
  (`stationary_ratio`, `lamStar_stationary`). Capital follows (7) (`eq7`).
  The identity `β((1 − λ*) A)^{1−γ} = 1 − λ*` (`discount_identity`) makes the
  utility of consumption a geometric series in `1 − λ*`, which sums to (8)
  (`eq8`).
- **Wealth-is-status.** With capital strictly increasing in the index, the
  mass of poorer men equals the index, so `m(i) = i` (`wis_rank`). In (10) a
  larger matching term forces a larger bequest when `u′` is decreasing
  (`bequest_rises`). Property 1 follows from an exchange inequality for
  strictly concave `u` (`property1_exchange`, `property1_no_switch`).
  Property 3 rests on the best woman of a pool beating its average
  (`property3_step`).
- **Aristocratic.** The status rule (12) (`eq12`). Writing
  `V(k, s) = V*(k) + s/(1 − β)`, (13) is the cancellation of `V*(k)`
  (`eq13`). The compensating capital satisfies
  `V*(L) − V*(k) = β j/(1 − β)` (`L_ge_k`), and with `V*(x) = C x^{1−γ}` from
  (8) it has the printed closed form (`L_closed_form`, `C_inv`).
- **Hybrid.** A random match in `[i, 1]` is worth `(1 + i)/2`
  (`upper_group_mean`), banishment costs at least `(1 − i)/2`
  (`banishment_loss`), members below the group mean gain in matching and the
  top man loses `(1 − i)/2` (`hybrid_welfare`, `top_man_loss`).
- **Section 5.3.** One unit a period is worth `1/(1 − β)`
  (`match_value_bound`).
- **For the contrast with Paper A.** The payoff is linear in `j`, so a group's
  spread does not enter (`spread_blind`), while a quadratic loss adds `s²/3`
  for a uniform group of half-width `s` (`quadratic_spread`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` gives one row per result. In short:

- Not formalised, 12 rows. Proposition I (Mas-Colell's existence theorem is
  not in Mathlib), Proposition II with (15), Proposition III, the Lemma,
  Properties 2, 4, 5 and 6, eqs. (11) and (14), the unequal-wealth case of the
  two-period example, and the modified aristocratic rule. They need the whole
  equilibrium path, limits in `t` or `β`, or are verbal.
- Special cases, 8 rows. Eq. (2) is checked in ratio form without the
  concavity step. `∂λ(j)/∂j < 0` uses a monotone inverse with the
  monotonicity of `g` as a hypothesis. That the constant `λ*` solves (3) is
  not checked, only its stationarity. Eq. (10) holds the matching term fixed.
  Properties 1 and 3 are their one-period steps. `V(k, s)` takes its
  additive split as given. The welfare comparison covers matching only.
- The journal version (JPE 1992) is not read, so nothing here is attributed
  to it.
