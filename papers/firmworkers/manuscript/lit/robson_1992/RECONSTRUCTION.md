# Reconstruction of Robson, 1992

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| ROB | "Status, the Distribution of Wealth, Private and Social Attitudes to Risk", *Econometrica* 60(4), July 1992, pp.837–857 | scan without a text layer, 21 PDF pages (one per printed page), Drive `status_distribution_wealth.pdf`, id `1ruG0oIOhG_pil7oVFQLRazuMXc_sRoQq`, uploaded 12 Oct 2026; cached `~/.cache/firmworkers/robson_1992.pdf`, sha256 `fc361a3878e81928f1698445034447e411abafe22d30824133249095345cc632` | [F], published |

The scan has no text layer and no OCR tool is installed locally, so the text is
Drive's OCR of the scan, cached `~/.cache/firmworkers/robson_1992.ocr.txt`,
sha256 `b51d77285f100ae9f62e0c8ed22b39a28aacf54682752b4eb1be0f48975a239e`.
Every printed page number from 838 to 857 stands on a line of its own, so page
`p` is the text from the line `p` to the line `p + 1` (p.837 runs from the top).
The OCR garbles displays, so Assumption 2 (p.839), Example A and the scaling
formula (p.840) and Example A′ (p.845) were read from page images at 110 dpi.
Read in full on 12 Oct 2026.

**Why this paper.** The author's decision of 12 Oct 2026: specific forms of
utility go in a separate sub-section, with a reference income set by the income
norm of the worker's own status group rather than by a free parameter. Robson
is the closest precedent found so far: rank in the wealth distribution enters
utility, and the convex region of reduced-form utility is then located by the
distribution, not by a parameter. It is needed for the novelty check of that
sub-section.

## Primitives (§2, pp.838–839)

- A continuum of individuals of measure one. Wealth distribution `F`,
  nondecreasing, left continuous, `F(0) = 0`, `F(W) = 1` for some `W`; `F(w)`
  is the measure with wealth below `w`, `F⁺(w)` the measure at or below `w`.
- Assumption 1: von Neumann–Morgenstern utility `U(w, N)`, strictly increasing
  in wealth `w` and in status `N ∈ [0, 1]`.
- Assumption 2: status `S_F(w) = ½F(w) + ½F⁺(w)`, the measure below `w` plus
  half the measure at `w`. Total status is one half whatever `F` (p.839).
- Definition 2: reduced-form utility `V_F(w) = U(w, S_F(w))`, which governs
  attitudes to risk.
- Hypothesis 1 (used where stated): `U` strictly concave in `w`, strictly
  convex in `N`.

## Derivation chain

1. **Friedman–Savage shape from status (§2, Example A, p.840).**
   `U = u(w) + kN^β`, `k > 0`, `β ≥ 2`, `u(w) = w^α/α` (or `ln w`) above a small
   `w̲`, `α < 1`; `F` uniform on `[0, 1]`, so `S_F(w) = w`. Then
   `V″ = (α − 1)w^{α−2} + kβ(β − 1)w^{β−2}` on `[w̲, 1]`, negative below
   `w̃ = [(1 − α)/(kβ(β − 1))]^{1/(β−α)}` and positive between `w̃` and 1; above 1
   status is constant and `V` is concave. `w̃ < 1` iff `1 − α < kβ(β − 1)`.
   Example A′ (p.845): `U = ln w + ¾N²` gives `w̃ = √(2/3)`.
2. **Scaling (p.840).** With `F` uniform on `[0, γ]`,
   `w̃/γ = [(1 − α)/(kβ(β − 1))]^{1/(β−α)} γ^{α/(β−α)}`: the share below the
   inflection rises in `γ` if `α > 0`, falls if `α < 0`, and is constant for
   logarithmic utility. In Friedman–Savage, whose inflection is a fixed wealth
   level, scaling up all wealth lowers the share below it (the drawback stated
   on p.840).
3. **Stable wealth distribution (§3, Definition 5, p.842).** `F` is an SWD if
   almost every individual rejects every free fair finite gamble. Each
   individual has measure zero, so gambling does not move `F`, and in an SWD
   there is essentially no gambling.
4. **No mass points (Lemma 2, p.843).** At a mass point status jumps from `F`
   to `F⁺`; a small fair bet won with high probability moves the individual up
   to `F⁺` and down only to `F`, so for `p` near one the bet beats the status
   `½F + ½F⁺` of staying. An equal distribution is never an SWD.
5. **Characterisation (Theorem 1, p.843; proof in the Appendix).** `F` is an
   SWD iff every point `(ŵ(x), V(ŵ(x)))` lies on the boundary of the convex hull
   of the hypograph of `V`. Corollary: if `V` is concave, `F` is an SWD.
6. **Middle class (pp.844–845).** Against Friedman (1953), who found no middle
   class in equilibrium with utility of wealth alone, an SWD here may have one:
   Example A′ with `F(w) = 4w/3 − 4w²/9` on `[0, 3/2]` (proof omitted in the
   paper).
7. **Existence (Theorem 2, p.845).** An SWD exists for every total wealth if
   `U` is strictly concave in `w` and the cross-partial stays bounded; Example B
   shows the bound is needed.
8. **Pareto efficiency (§4, Lemma 3, Theorem 3, pp.847–848).** With utility
   convex in status, an interval of equal positive wealth is Pareto inefficient:
   a fair lottery splitting the interval's status into its lower and upper halves
   raises expected utility. Theorem 3 characterises efficiency through
   `U_w(ŵ(x), x)`, whereas private incentives follow `V`.
9. **Externality (§5, pp.848–851).** `V` ignores the status a change in one
   individual's wealth imposes on others; an SWD may be Pareto inefficient,
   including through too little gambling (Example C); a tax/subsidy makes stable
   after-tax distributions exactly the efficient ones (Theorem 4).
10. **Conclusions (pp.851–852).** Extensions: a market for gambles clearing with
    positive gambling; utility depending on average wealth, which with exogenous
    total wealth would leave the externality without effect.

## What was reconstructed

Items 1 (the second derivative and its sign, Example A′'s inflection), 2 (the
scaling identity, the logarithmic case and the rising case), 4 (the limiting
step of Lemma 2), the Corollary to Theorem 1 for finite gambles, the status
rule's total of one half in a finite population, the core step of Lemma 3, and
the closed-economy reading of the remark in item 10. See `CLAIMS.md`.

## What was not reconstructed

Theorem 1's converse (via Carathéodory), Theorem 2 and Example B, Theorem 3,
Theorem 4, Example C, the Dutch-book argument for pricing at expectation
(p.842), the concavity of `V` for the two Example A′ distributions (proofs
omitted in the paper; checked by hand only, see `NOTES.md`), and the falling
case `α < 0` of item 2.
