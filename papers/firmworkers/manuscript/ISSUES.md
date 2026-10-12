# Issues outstanding (12 Oct 2026)

One row per option. "Depends on" lists the ledger rows the issue turns on; ramifications marked *(not in Lean)* are consequences read off the proved forms, not yet proved themselves. Ids: M = model rows, L = literature rows, X = measurement-map rows in `claims.yaml` and `measurement_map.yaml`.

## A. Model decisions

| # | Issue | Depends on | Option | Ramifications |
|---|---|---|---|---|
| 1 | Fallback of a displaced worker (M32 reading) | M32 (DECIDED, reading a); affects M20, M30, M33, M34, M35; evidence L26 (JLS) | (a) Keep the bottom rung `x_0` | No new parameter. Loss share `1 − x_0/x_i` rises with income; JLS report roughly constant shares (about 25%) across groups, so nothing in L26 supports a common fallback. M33–M35 stand as proved. M34's form-free sorting is of order `1/(|P|−1)`, so in a large pool M20's direction must come from the shape of `u` (issue 7). |
| | | | (b) Proportional step `x_i(1 − δ)` | One parameter `δ` (JLS's 0.25 is a reading). First-order dominance of the lottery (M33) no longer holds unconditionally: stability needs the poorest entrant's prize to cover the richest entrant's fallback *(not in Lean)*. Richer entrants have the higher fallback, so the rank sorting of M20 appears for every nondecreasing `u`, with gap of order `δ(x_j − x_i)` rather than `1/(|P|−1)` *(not in Lean)*. M35's unit dependence remains (`δx_i` against `(δx_i)^b`). |
| | | | (c) Best vacated position below | Fallback is an equilibrium object (the queue of vacated positions); no Lean; least tractable; M33–M35 would need restating over the queue. |
| 2 | Scale of the cost function (M7, M35) | M7, M35; affects M20, M21, M23 | (a) Income in units of `x_0` (`x_0 = 1`) | No new parameter. The return beats the downgrade iff `x_i < 2x_0`; on the ladder of M3 that is the bottom `1 − 2^{−a}` of workers (three quarters at `a = 2`) *(not in Lean)*. The top `2^{−a}` downgrade under risk neutrality. |
| | | | (b) Scale-free cost, `C = x_i (loss/x_i)^b` | For `b > 1` the return always beats the downgrade *(not in Lean)*, so the three-route choice collapses to return against lottery; the downgrade is never chosen. |
| | | | (c) Explicit scale `κ`, `C = κ(loss)^b` | One free parameter; the return cut is `loss < κ^{−1/(b−1)}`; identifiable only from observed training costs (an X14-type observation, now UNVERIFIED). |
| | | | (d) Leave as is, state the unit | M35 stays a statement about the unit; no headline may rest on the return-versus-downgrade comparison. |
| 3 | Meaning of stakes in `a` (Q7; M21's `a` part) | M3, M4, M5 (REFUTED), M8, M9 | (a) Stakes = inequality `1/a` | M21 reads "lottery play rises with inequality"; consistent with the source's intent; the manuscript must say `a` is inverse inequality. |
| | | | (b) Keep `a`, flip M21's sign | "Lottery play falls in `a`"; literally what M3 and M4 give; reads against the source's wording. |
| | | | (c) Stakes = prize spread of the pool (`max − min` former income among entrants) | The stake becomes an object of the pool (M14), not of `a` alone; needs a pool-composition result before M21 can be stated. |
| | | | (d) Drop the `a` part of M21 | M21 reduces to "mobility falls in `b`", which still needs a mobility measure (issue 5). |
| 4 | Repairs of the ill-posed rows (Q8) | M1 → M2–M4; M6 → M7–M9; M17 → M18 | (a) Adopt all repairs as the model's statements | Already assumed by `repaired_by`; M1, M6, M17 go to Appendix E with their counterexamples; nothing else changes. |
| | | | (b) Adopt some, restate others | Not adopting M7 reopens M23 (kernel from the cost) and M35; not adopting M18 keeps the contradiction with the macro's constant `p_L, p_H` (issue 14). |
| 5 | Rank kernel and equilibrium concept (M23; Q5) | M7, M8, M9; M32, M35 (issues 1, 2); M20, M21, M22 wait on it. Decided already: global rank (Q6), three channels (Q6), friction = closure in the pool (M29) | (a) One-shot comparison of the three routes at each rank (Q5 proposal) | Kernel = stay / downgrade / return / lottery mixture over ranks; tractable; gives M20 and a mobility measure for M21 (share of workers moving rank); depends on issues 1 and 2 for which route wins where. |
| | | | (b) Dynamic kernel with repeated displacement | Needs a displacement rate as a parameter and the lottery pool's steady state; closer to the source's "long-run equilibrium"; no Lean. |
| | | | (c) Leave M23 OPEN | M20–M22 stay in the Pending table; no headline on mobility. |
| 6 | Object linking upskilling to the pool (M22) | M22; M14, M28, M32; X14 (UNVERIFIED) | (a) Upskilling = the return route (M7's cost) | M22 becomes exclusivity of routes in the one-shot choice (a definition, not a claim). |
| | | | (b) A micro `λ` (share to upskilling against the lottery) as in the macro | Needs a technology from `λ` to rank; `h` is fixed (M28) and the lost job's skills are worth nothing (M32), so `λ` can act only through a position-specific return; X14 would then measure it. |
| | | | (c) Drop M22 or merge into M23 | One fewer pending row; the source's "upward mobility falls as a worker upskills" is not represented. |
| 7a | Shape of `u` for the reference-dependent sub-section (M19) | M19 (UNDERSPECIFIED), M20; L21 (KT), L22 (FS), L23 (Markowitz), L19 (KR), L20 (GR), L16–L18; L27 (LR) | (a) Kahneman–Tversky: convex below `R_h`, concave above, loss-averse at `R_h` | Workers below their group norm gamble; a displaced worker with an unadapted reference is in the loss domain and accepts gambles (KT p.287, in Lean), which links the sub-section to M32. Large-loss caveat (issue 7b). Novelty, per GR: group-based aspirations are named as an extension, so the new element is the group reference × closure `σ` in the cross-group pool. |
| | | | (b) Friedman–Savage: convex segment between class incomes | Gambling by those between the two groups' norms; sorting by class position, not own rank within the group; matches the appendix's citation but not its description (which is KT's shape, recorded in L22). Precedents: Friedman (1953) assigns occupations by endowed wealth (unread, issue 12); LR's wealth sorting runs the other way (L27). |
| | | | (c) Markowitz: reference at own customary income | Contradicts the `R_h` decision (group norm), so a contrast only; shape near the reference is the reverse of KT's; convex at large losses. |
| 7b | Large-loss regime | M19; L21 (KT p.279), L19 (KR), L23 (Markowitz) | (a) Convex throughout the loss domain (KT's main case) | Every displaced worker below `R_h` gambles; the lottery-versus-downgrade claim is uniform in rank. |
| | | | (b) Concave again at large losses (KT's caveat, KR) | Under M32(a) the richest displaced have the largest loss and stop gambling: middle ranks lottery, top ranks downgrade or return; an interior pattern the source does not state. |
| | | | (c) Stay form-free | M33 already covers every nondecreasing `u` for lottery against downgrade; the regime then matters only for who gambles and for the premia (M16, M18). |
| 7c | Statistic for `R_h` | L25 (CO), L20 (GR), L28 (HK); planned X row (issue 11) | (a) Median of the own group's income | A quantile, so it sits on M2's quantile ladder and is observed directly. |
| | | | (b) Mean of the own group's income | On M3's ladder the mean is `x_0 a/(a − 1)`, infinite for `a ≤ 1` *(not in Lean)*; sensitive to the top tail. |
| | | | (c) Predicted pay of comparable workers (CO's `y*`) | Matches CO's evidence; a regression construct over many characteristics, not a norm by `h` alone; not a function of the ladder. |
| 8 | Which headline and concluding claims to admit (Q9) | All M rows; `check.py`: a headline may not rest on an OPEN M row, an UNVERIFIED observation or a pending X row; headline list is empty | (a) Admit now from proved rows (M30–M31 unravelling, M33 displaced-pool stability, M34 sorting order, M25–M27 closure with X25) | Headlines are about the lottery mechanism and closure, not the source's two questions (inequality → investment, inequality → growth), which sit in the deferred macro model. |
| | | | (b) Wait for issues 1–7 | No headline yet; the skeleton has model and literature tables only. |
| | | | (c) Admit sub-section headlines once M19 is decided | Headlines conditional on a form of `u`, against the rule that main claims are form-free; they belong to the sub-section only. |

## B. Measurement map

| # | Issue | Depends on | Option | Ramifications |
|---|---|---|---|---|
| 9 | Confirm the PROPOSED observations (X1–X26 except X12) | every X row with a referent | Confirm, revise or reject per row | Until confirmed the empirical key of every headline is provisional; a rejected row removes the symbol from any claim that declares it. |
| 10 | Raise the UNVERIFIED observations | X7 (entry cost `L`), X14 (`λ`), X24 (`h` label): need an empirical paper measuring the object; X8 (uniform partner draw): M14 now DECIDED, needs the implication proved; X19 (threshold under curved `u`): needs identification | (a) Find and record a measuring paper (X7, X14, X24) | Each adds a `lit/` record with Lean; raises the row to LITERATURE support. |
| | | | (b) Prove the implication (X8) | A Lean theorem that under M14 the rank change after job loss is independent of own rank raises X8 to PROVEN. |
| | | | (c) Mark CONSTRUCT | A headline may rest on it with a note. |
| | | | (d) Leave UNVERIFIED | No headline may use the symbol. |
| 11 | New rows: X25 citing CJP/Coleman (`σ = IH/H`); `R_h` with CO as referent; the downgrade with JLS's definition (1) and model (2) | L24, L25, L26; M29, M19, M32; `check.py`: every X row must be used by some claim | (a) Add rows now | The `R_h` and downgrade rows need a claim that declares them (an M row for the sub-section, or a downgrade claim), or the checker fails; X25's citation is immediate. Mismatches to state: CJP's `σ` is for friendships, ours for lottery partners; JLS's loss starts three years before separation, M32's displacement is a discrete event (measuring from the year before understates the downgrade by half, JLS-25). |
| | | | (b) Add them with the sub-section rows after issue 7 | Nothing added now; the CO and JLS records wait. |

## C. Literature

| # | Issue | Depends on | Option | Ramifications |
|---|---|---|---|---|
| 12 | Friedman (1953), "Choice, Chance, and the Personal Distribution of Income", JPE 61(4) | Sub-section novelty (issue 7); L27 quotes it (LR-28 to LR-30); not on Drive | (a) Upload, read, record with Lean | The sub-section's novelty statement rests on the primary source. |
| | | | (b) Rely on LR's description | Against the rule of 10 Oct 2026 (novelty needs the primary source); the novelty statement would cite a secondary description. |
| 13 | Cooper, García-Peñalosa and Funk (2001), EJ 111(473) | M29 (closure) in a growth setting; cited by L28 (HK-7) | (a) Triage from the abstract | Record only if it bears on closure in growth. |
| | | | (b) Record now | A `lit/` record with Lean for a paper no claim may need. |
| 14 | Macro-sync items: M18 (premium scales with `m²`, macro treats `p_L, p_H` as constants), Spence (1973), the Tullock lottery | M18; the macro model (not in the ledger) | (a) Keep deferred | The micro ledger contradicts the macro's constants until the sync. |
| | | | (b) Start the macro ledger now | Opens a second set of M rows and records before the micro decisions are made. |
| 15 | `inputs/displacement_upskilling_v4.tex` | TODO sanity-check list | (a) Keep outside the ledger | As now. |
| | | | (b) Admit its lottery or utility forms | Its two-agent lottery without entry cost contradicts M14/M32; its group-specific quadratic utilities contradict the common-`u` decision. |

## Outstanding but not decisions

- Novelty statements to record when prose starts: LR against M34 (direction, channel, object); LR's productivity rationing against closure by `h` (M29); HK's spending race against access through `σ`, and their exogenous-status benchmark for `h`; GR's group-based aspirations as the named extension.
- X8's Lean theorem (issue 10b) can be written without a decision.
- M23's `waits_on` text should be trimmed to what is still open (issue 5).
