# Claims about Hopkins & Kornienko

**Papers.** All three read from the PDFs in the Drive folder, evidence level
**[F]**.

| Short | Full |
|---|---|
| HK2004 | "Running to Keep in the Same Place: Consumer Choice as a Game of Status", *American Economic Review* 94(4), Sep. 2004, pp. 1085–1107. |
| HK2009 | *Games and Economic Behavior* 67 (2009), pp. 552–568. |
| HK2010 | "Which Inequality? The Inequality of Endowments versus the Inequality of Rewards", *AEJ: Microeconomics* 2(3), Aug. 2010, pp. 106–137. |

## Claims as stated in `LITERATURE.tex` §sec:hk

| ID | Claim | Source locator |
|---|---|---|
| HK-A | No extensive margin: every contestant competes; the choice is how much performance to buy, never whether to compete. | HK2010 model section |
| HK-B | The contest is perfectly discriminating; the authors contrast this with the partly stochastic award mechanisms of the surrounding literature. | HK2010 |
| HK-C | Endowment is a budget divided between performance and consumption, not the utility cost of a fixed fee. | HK2010 |
| HK-D | **Definition 1**: `F <=d G` whenever `G^{-1}(r) - F^{-1}(r)` is weakly increasing on `(0,1)`; equivalently, at a fixed rank the more dispersed distribution is the less dense one. | HK2010 p.128, Definition 1 and eq. (18) |
| HK-E | There is no clear relation between the dispersive order and SOSD, because SOSD compares on both mean and dispersion while the dispersive order concerns dispersion alone. | HK2010 p.130 |
| HK-F | **The bridge.** Writing `T = Lambda_beta^{-1} o Lambda_alpha`, the dispersive order says `T(w) - w` is increasing; if in addition `T(w) - w` changes sign, it does so once, at `x_0`, giving `T(w) >= w` above `x_0` and `T(w) <= w` below — which is the pivot-spread hypothesis. `LITERATURE.tex` says this "should be checked formally and then stated". | Our own claim, built on HK-D |
| HK-G | **Proposition 4** (HK2010): assuming the minimum endowment is higher ex post, endowments less dispersed ex post, and the maximum endowment lower ex post, performance is higher ex post on `[0, r̂]` where `r̂` is the *only* crossing point; utility rises at the bottom but falls for middle and top. | HK2010 Proposition 4 |
| HK-H | HK2004 uses income indexing and the ULR order; its Proposition 4 partitions into three zones by **two** thresholds — a two-crossing result, more general than a single pivot. | HK2004 Proposition 4 |
| HK-I | HK2009 switches to rank indexing and introduces the dispersive order because income indexing cannot compare distributions with different supports. Propositions 4 and 5 pin welfare signs by a single crossing `r̂`, structurally analogous to P9-gen's pivot. | HK2009 |
| HK-J | Both papers work in *income* space; no saving, no wealth stock. | HK2004, HK2009 |

## What is checkable here

- **HK-D** is checkable in two halves: the quantile slope identity that makes
  Definition 1 and eq. (18) equivalent (HK-1, HK-2), and the location-freeness
  and variance consequences the paper states (HK-3, HK-5).
- **HK-E** is checkable by exhibiting a dispersively-equal pair that SOSD
  separates (HK-4).
- **HK-F** is the substantive one and is **machine-checked in Lean**
  (`Dispersive.lean`, theorems HK-L1..HK-L5). This discharges the "should be
  checked formally" instruction in `LITERATURE.tex`.
- **HK-A, HK-B, HK-C, HK-G, HK-H, HK-I, HK-J** are structural and textual
  readings. Not arithmetic; deliberately unchecked. HK-G's statement was
  nevertheless confirmed verbatim against the source — see `NOTES.md`.
