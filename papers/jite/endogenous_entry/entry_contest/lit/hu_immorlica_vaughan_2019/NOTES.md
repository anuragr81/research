# Notes — Hu, Immorlica and Vaughan (2019), pass 1 (read in full)

Run `python3 lit/hu_immorlica_vaughan_2019/verify_hu_immorlica_vaughan.py` from the repository
root.

## Verdict for C1 to C14

No claim is overturned. The paper is a positioning neighbour for the wealth channel.

- **C1, C5** (the sign rule for a spread of wealth): unaffected. There are two groups and no
  distribution of wealth, so no spread is signed.
- **C10, C13** (the share of positions): unaffected. There is no fixed number of positions and
  no choice to pay; the result is the learner's errors by group. C13 can cite it beside
  Liu, Garg and Borgs.
- **K3** (the sorting needs only that the fee's utility cost falls with wealth): the paper's
  derivation of its cost condition (HIV-5) is the same reading at group level, and can be cited
  in K3's scope note as a precedent for the reading, not for the result.

## Findings from the formalisation

1. A cleaner proof of Proposition 1's order: compare the two groups' costs over `[τ_A, σ_A]`
   directly (`sigma_order`).
2. The sign misprint in the appendix's `ℓ_m` (HIV-D2) and the label slip in Example 3
   (HIV-D3).
3. The thresholds of Examples 1-3 check; the subsidy penalties are not recomputed.
