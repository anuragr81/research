# Issues outstanding (12 Oct 2026)

Open decisions only. Decided items are not listed: decided model rows (M14, M28, M29, M32) are in Appendix D of the skeleton; decisions without a model row (Q1, Q2, Q4–Q6, Q9, the reference-dependent sub-section with `R_h`, the macro deferral) are in `TODO.md`, Decisions recorded, and `HANDOVER.md`.

One row per option. "Depends on" lists the ledger rows the issue turns on; ramifications marked *(not in Lean)* are consequences read off the proved forms, not yet proved themselves. Ids: M = model rows, L = literature rows, X = measurement-map rows in `claims.yaml` and `measurement_map.yaml`.

## A. Model

| # | Issue | Depends on | Option | Ramifications |
|---|---|---|---|---|
| 1 | Scale of the cost function (M7, M35) | M7, M35; affects M20, M21, M23 | (a) Income in units of `x_0` (`x_0 = 1`) | No new parameter. The return beats the downgrade iff `x_i < 2x_0`; on the ladder of M3 that is the bottom `1 − 2^{−a}` of workers (three quarters at `a = 2`) *(not in Lean)*. The top `2^{−a}` downgrade under risk neutrality. |
| | | | (b) Scale-free cost, `C = x_i (loss/x_i)^b` | For `b > 1` the return always beats the downgrade *(not in Lean)*, so the three-route choice collapses to return against lottery; the downgrade is never chosen. |
| | | | (c) Explicit scale `κ`, `C = κ(loss)^b` | One free parameter; the return cut is `loss < κ^{−1/(b−1)}`; identifiable only from observed training costs (an X14-type observation, now UNVERIFIED). |
| | | | (d) Leave as is, state the unit | M35 stays a statement about the unit; no headline may rest on the return-versus-downgrade comparison. |
| 2 | Meaning of stakes in `a` (Q7; M21's `a` part) | M3, M4, M5 (REFUTED), M8, M9 | (a) Stakes = inequality `1/a` | M21 reads "lottery play rises with inequality"; consistent with the source's intent; the manuscript must say `a` is inverse inequality. |
| | | | (b) Keep `a`, flip M21's sign | "Lottery play falls in `a`"; literally what M3 and M4 give; reads against the source's wording. |
| | | | (c) Stakes = prize spread of the pool (`max − min` former income among entrants) | The stake becomes an object of the pool (M14), not of `a` alone; needs a pool-composition result before M21 can be stated. |
| | | | (d) Drop the `a` part of M21 | M21 reduces to "mobility falls in `b`", which still needs the mobility measure of the one-shot kernel (Q5). |
| 3 | Repairs of the ill-posed rows (Q8) | M1 → M2–M4; M6 → M7–M9; M17 → M18 | (a) Adopt all repairs as the model's statements | Already assumed by `repaired_by`; M1, M6, M17 stay in Appendix E with their counterexamples; nothing else changes. |
| | | | (b) Adopt some, restate others | Not adopting M7 reopens M23 (kernel from the cost) and M35; not adopting M18 removes the micro result that the premium coefficient varies with income, which the macro sync must otherwise address. |
| 4 | Object linking upskilling to the pool (M22) | M22; M14, M28, M32; X14 (UNVERIFIED) | (a) Upskilling = the return route | M22 becomes exclusivity of routes in the one-shot choice (a definition, not a claim). |
| | | | (b) A micro `λ` (share to upskilling against the lottery) as in the macro | Needs a technology from `λ` to rank; `h` is fixed (M28) and the lost job's skills are worth nothing (M32), so `λ` can act only through a position-specific return; X14 would then measure it. |
| | | | (c) Drop M22 or merge into M23 | One fewer pending row; the source's "upward mobility falls as a worker upskills" is not represented. |
| 5a | Shape of `u` for the reference-dependent sub-section (M19) | M19 (UNDERSPECIFIED), M20; L21 (KT), L22 (FS), L23 (Markowitz), L19 (KR), L20 (GR), L16–L18; L27 (LR). Form-free sorting (M34) is of order `1/(|P|−1)`, so in a large pool M20's direction must come from this shape | (a) Kahneman–Tversky: convex below `R_h`, concave above, loss-averse at `R_h` | Workers below their group norm gamble; a displaced worker with an unadapted reference is in the loss domain and accepts gambles (KT p.287, in Lean), which links the sub-section to M32. Large-loss caveat (5b). Novelty, per GR: group-based aspirations are named as an extension, so the new element is the group reference × closure `σ` in the cross-group pool. |
| | | | (b) Friedman–Savage: convex segment between class incomes | Gambling by those between the two groups' norms; sorting by class position, not own rank within the group; matches the appendix's citation but not its description (which is KT's shape, recorded in L22). Precedents: Friedman (1953) assigns occupations by endowed wealth (unread, see below); LR's wealth sorting runs the other way (L27). |
| | | | (c) Markowitz: reference at own customary income | Contradicts the `R_h` decision (group norm), so a contrast only; shape near the reference is the reverse of KT's; convex at large losses. |
| 5b | Large-loss regime | M19; L21 (KT p.279), L19 (KR), L23 (Markowitz) | (a) Convex throughout the loss domain (KT's main case) | Every displaced worker below `R_h` gambles; the lottery-versus-downgrade claim is uniform in rank. |
| | | | (b) Concave again at large losses (KT's caveat, KR) | Under M32 the richest displaced have the largest loss and stop gambling: middle ranks lottery, top ranks downgrade or return; an interior pattern the source does not state. |
| | | | (c) Stay form-free | M33 already covers every nondecreasing `u` for lottery against downgrade; the regime then matters only for who gambles and for the premia (M16, M18). |
| 5c | Statistic for `R_h` | L25 (CO), L20 (GR), L28 (HK); planned X row (issue 8) | (a) Median of the own group's income | A quantile, so it sits on M2's quantile ladder and is observed directly. |
| | | | (b) Mean of the own group's income | On M3's ladder the mean is `x_0 a/(a − 1)`, infinite for `a ≤ 1` *(not in Lean)*; sensitive to the top tail. |
| | | | (c) Predicted pay of comparable workers (CO's `y*`) | Matches CO's evidence; a regression construct over many characteristics, not a norm by `h` alone; not a function of the ladder. |

## B. Measurement map

| # | Issue | Depends on | Option | Ramifications |
|---|---|---|---|---|
| 6 | Confirm the PROPOSED observations (X1–X26 except X12) | every X row with a referent | Confirm, revise or reject per row | Until confirmed the empirical key of every headline is provisional; a rejected row removes the symbol from any claim that declares it. |
| 7 | Raise the UNVERIFIED observations | X7 (entry cost `L`), X14 (`λ`), X24 (`h` label): need an empirical paper measuring the object; X8 (uniform partner draw): needs the implication proved; X19 (threshold under curved `u`): needs identification | (a) Find and record a measuring paper (X7, X14, X24) | Each adds a `lit/` record with Lean; raises the row to LITERATURE support. |
| | | | (b) Prove the implication (X8) | A Lean theorem that under M14 the rank change after job loss is independent of own rank raises X8 to PROVEN. |
| | | | (c) Mark CONSTRUCT | A headline may rest on it with a note. |
| | | | (d) Leave UNVERIFIED | No headline may use the symbol. |
| 8 | New rows: X25 citing CJP/Coleman (`σ = IH/H`); `R_h` with CO as referent; the downgrade with JLS's definition (1) and model (2) | L24, L25, L26; M29, M19, M32; `check.py`: every X row must be used by some claim | (a) Add rows now | The `R_h` and downgrade rows need a claim that declares them (an M row for the sub-section, or a downgrade claim), or the checker fails; X25's citation is immediate. Mismatches to state: CJP's `σ` is for friendships, ours for lottery partners; JLS's loss starts three years before separation, M32's displacement is a discrete event (measuring from the year before understates the downgrade by half, JLS-25). |
| | | | (b) Add them with the sub-section rows after issue 5 | Nothing added now; the CO and JLS records wait. |

## C. Sources

| # | Issue | Depends on | Option | Ramifications |
|---|---|---|---|---|
| 9 | `inputs/displacement_upskilling_v4.tex` | TODO sanity-check list | (a) Keep outside the ledger | As now. |
| | | | (b) Admit its lottery or utility forms | Its two-agent lottery without entry cost contradicts M14/M32; its group-specific quadratic utilities contradict the common-`u` decision. |

## Decided, with evidence that arrived after the decision

Listed so the decision can be checked while the issues it bears on are settled; it stays decided unless the author reopens it.

| Decided item | Evidence since | Bears on | If reopened |
|---|---|---|---|
| M32: a displaced worker falls to the bottom rung `x_0` (12 Oct 2026, reading (a) of three) | L26 (JLS, read later on 12 Oct): losses of about 25% of former earnings six years on, reported as shares and roughly equal across sex and age. The bottom rung implies shares rising with income; the only income contrast JLS report (men against women) goes the other way, which is suggestive, not a test | Issue 1 (M35 scales the loss `x_i − x_0`); issue 5b(b) (the richest have the largest loss only under a common fallback); M33–M35 | (b) proportional step `x_i(1 − δ)`: M33's stability needs a condition, and rank sorting appears for every nondecreasing `u` with gap of order `δ(x_j − x_i)` *(not in Lean)*; (c) best vacated position below: the fallback becomes an equilibrium object, no Lean |

## Actions that need no decision

- Upload Friedman (1953), "Choice, Chance, and the Personal Distribution of Income", JPE 61(4), 277–290: a direct novelty threat for issue 5, named by L27; not on Drive. Novelty needs the primary source (rule of 10 Oct 2026).
- Triage Cooper, García-Peñalosa and Funk (2001), EJ 111(473), from its abstract (cited by L28 as the full-closure limit).
- Build the one-shot comparison of the three routes at each rank, the equilibrium Q5 left to tractability; it gives M20 and the mobility measure for M21, and its outcome depends on issue 1.
- Prove X8's implication in Lean (issue 7b).
- Trim M23's `waits_on`: the rank kernel is decided (global rank and three channels, Q6; friction as closure, M29), but the skeleton's Pending table still shows it as open.
- Record the novelty statements when prose starts: LR against M34 (direction, channel, object); LR's productivity rationing against closure by `h` (M29); HK's spending race against access through `σ`, and their exogenous-status benchmark for `h`; GR's group-based aspirations as the named extension.
