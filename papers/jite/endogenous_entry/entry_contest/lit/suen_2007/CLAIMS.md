# Claims about Suen (2007)

**Paper.** Wing Suen, "The comparative statics of differential rents in
two-sided matching markets", *J Econ Inequal* 5 (2007), pp. 149–158.
Evidence level **[F]**, published version.

## Claims as stated in `LITERATURE.tex` §sec:assignment and §Verdict

| ID | Claim | Source locator | Relevance claimed to `PROOFS.tex` |
|---|---|---|---|
| SU-A | One-to-one transferable-utility matching with `Q = θ(x)φ(y)`, positive assortative matching, matching function `μ = G⁻¹∘F`, "which they note can itself be read as a distribution function". | §2, eq. (1), p.151 | Same map class as our `T = Λ_β⁻¹∘Λ_α` |
| SU-B | **Proposition 2**: if `G₁` is more dispersed than `G₀` (same mean, second-order), total earnings on the other side fall, under concavity of `θ, φ` and log-concavity of `1−F`. | Prop 2, p.154 | Template for signing an aggregate under a general spread |
| SU-C | The proof handles a **general** RS spread with an arbitrary finite number of crossings `k₀ < k₁ < … < k_{2n}` of the quantile functions: the quantile difference alternates in sign, each partial integral is signed by second-order dominance, concavity controls the recombination, and the result is integrated by parts against `ρ = (1−F)/f`. | Proof of Prop 2, eqs. (4)–(6) | **Open item 2** — the technique to transplant |
| SU-D | Proposition 3 and its corollaries give conditions under which every agent loses. | Prop 3, Cors 1–2 | Stronger conclusion, unused |
| SU-E | Suen's `μ` is cross-side (the other market's distribution moves) whereas our `T` is same-side (before and after), "but the map class is identical". | Survey's comparison | Positioning of P9-gen |
| SU-F | **§Verdict / open item 2:** "Two **independent** published sources — Suen's Proposition 2 and Costrell–Loury's Proposition 5 — sign an aggregate under a general mean-preserving spread using the same technique [...]" and the proposed extension is to "replace the pivot hypothesis with log-concavity of `1 − Λ` and apply the Suen integration to `k*`". | Survey's synthesis | Drives the whole proposed route for open item 2 |

## What is checkable here

- **SU-A** — `μ = G⁻¹∘F` increasing with `μ(0)=0`, `μ(1)=1` (SU-1); the wage
  ODE `(2)` and its SOC gap (SU-2).
- **SU-B/SU-C** — the operative role of the log-concavity hypothesis: `1−F`
  log-concave is *exactly* `ρ' ≤ 0` (SU-3); and the quantile-SOSD step the
  proof imports (SU-4).
- **SU-F is where the survey errs**, on the word *independent*. See
  `NOTES.md`.
- Two things the paper says that the survey does not carry, both checked:
  concavity is **sufficient but not necessary** (SU-6), and **footnote 1's
  warning that rank-rescaling destroys concavity** (SU-5). The second bears
  directly on the proposed route.
- **SU-D, SU-E** are textual/structural; confirmed by reading, unchecked.
