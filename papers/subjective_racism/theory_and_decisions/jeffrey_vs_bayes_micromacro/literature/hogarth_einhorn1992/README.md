# Hogarth, R. M. & Einhorn, H. J. (1992), "Order Effects in Belief Updating: The Belief-Adjustment Model"

*Cognitive Psychology* 24, 1-55.

**Source.** The copy used is a compiled plain-LaTeX **transcription** (42 pages,
`hogarth_einhorn1992.pdf` / `.tex`). It is not the journal scan. Page references
are to the transcription and written `T-p.N`. Journal pages cannot be checked.
The transcriber reconstructed the equations "from the (OCR-mangled) source".
Every displayed identity used below was re-derived in Lean and sympy, and all of
them hold, so the equations as transcribed are internally consistent. Read in
full, including Appendices A-C, on 2026-09-29.

## Claims formalized

The model (T-p.6-12):

* **Eq. (1)** `S_k = S_{k-1} + w_k [s(x_k) - R]`, with `0 ≤ w_k ≤ 1` and
  `0 ≤ S_k ≤ 1`.
* **Encoding.** The evaluation mode has `R = 0` and bipolar evidence
  `-1 ≤ s ≤ 1`, which gives Eq. (2), `S_k = S_{k-1} + w_k s(x_k)`. The estimation
  mode has `R = S_{k-1}` and unipolar evidence `0 ≤ s ≤ 1`, which gives Eq. (3).
  Rearranged, Eq. (3) is the averaging form, **Eq. (4)**:
  `S_k = (1 - w_k) S_{k-1} + w_k s(x_k)`.
* **Adjustment** (the "contrast assumption", Eqs. 6a/6b): `w_k = α S_{k-1}` if
  `s(x_k) ≤ R`, and `w_k = β (1 - S_{k-1})` if `s(x_k) > R`. Here `0 ≤ α, β ≤ 1`.
  Eqs. (7a/7b) substitute these weights into Eq. (1).
* **Processing.** In Step-by-Step (SbS) processing, Eq. (1) is applied to each
  item in turn. In End-of-Sequence (EoS) processing, there is one adjustment by
  an aggregate, **Eq. (5)** `S_k = S_0 + w_k [s(x_1..x_k) - R]`. When there is no
  explicit prior, the first item serves as the anchor, which gives
  **Eq. (8)** `S_k = s(x_1) + w_k [s(x_2..x_k) - R]`. HE add that "the effective
  weight accorded to `s(x_1)` must be greater than the weight attached to any of
  the other pieces of evidence" (T-p.12).

Their order-effect claims:

* **T-p.11 and Appendix B (T-p.34-35):** "when `R = S_{k-1}`, the SbS process
  always predicts recency (for α, β ≠ 0)". The proof in Appendix B assumes
  Anderson and Hovland's (1957) weights `w_a = w_ba`, `w_b = w_ab`, so that each
  item keeps its own weight wherever it stands. It concludes
  `D = w_a w_b [s(x_b) - s(x_a)] > 0` (B.5). HE then add: "in our model it is not
  necessarily the case that `w_a = w_ba` and that `w_b = w_ab`. However, this
  assumption considerably simplifies the algebraic proof" (T-p.35).
* **T-p.11 and Appendix C (T-p.35-36):** with `R = 0`, consistent evidence gives
  no order effect (C.2, C.4). Mixed evidence gives recency,
  `D = -αβ s(x⁻) s(x⁺)` (C.7), and this does not depend on `S_0`.
* **T-p.12:** EoS "contains within it a force toward primacy" (Eq. 8).
* **Table 2 (T-p.12)** gives these predictions for short simple series:
  EoS gives primacy in every column. SbS gives recency, except for `R = 0` with
  consistent evidence, where it gives no effect.
* **T-p.13.** Over long series, "decrements in α and β … will eventually induce
  primacy".
* **T-p.13-14 (variants).** Constant `w` with `R = 0` gives no order effect.
  Reversing the contrast assumption gives primacy for mixed evidence.

## Result

`lean/HogarthEinhorn.lean` is a symlink to `lean/Literature/HogarthEinhorn.lean`.
It is standalone on Mathlib. It checks with `lake env lean` and contains no
`sorry`. Its 43 theorems all use only the axioms `[propext, Classical.choice,
Quot.sound]`.

| Lean theorem | Content |
|---|---|
| `estStep_averaging` | Eq. (3) gives Eq. (4) |
| `contrastWeight_mem_Icc` | the weights (6a/6b) lie in `[0,1]` |
| `appB_orderEffect` | **(B.3)** for arbitrary item- and position-specific weights |
| `appB_recency`, `appB_recency_pos` | **(B.5)** `D = w_a w_b (s_b - s_a) > 0` under Anderson-Hovland weights |
| `positional_orderEffect`, `positional_primacy_iff` | weights by position (`w₁`, `w₂`): `D = (s_b - s_a)(w₂(1+w₁) - w₁)`, so primacy holds iff `w₂(1+w₁) < w₁` |
| `contrast_mixed_orderEffect`, `contrast_mixed_recency` | **HE's own weights, `R = S_{k-1}`, mixed evidence** (`s_a < S_0 < s_b`): `D = αβ[uv + S₀(1-S₀)(u+v) + αS₀²u² + β(1-S₀)²v²] > 0`, with `u = S₀ - s_a` and `v = s_b - S₀` |
| `contrast_consistent_pos_nocross` | HE's weights with consistent evidence above the anchor, where the anchor does not overshoot: `D = β²(1-S₀)²(s_b - s_a)(1 - β(s_a + s_b - 2S₀))` |
| `contrast_consistent_primacy_asym`, `contrast_consistent_primacy_equal` | **explicit primacy under HE's own SbS `R = S_{k-1}` model**: (α, β, S₀) = (1/10, 1, 1/10) with items 1/2 and 1 gives 0.7516 vs 0.87269; α = β = 1, S₀ = 9/10 with items 0 and 1/5 gives 0.1901 vs 0.1971 |
| `appC_consistent_neg`, `appC_consistent_pos` | (C.1)-(C.4) |
| `appC_mixed_orderEffect`, `appC_mixed_recency` | (C.7) |
| `variant_constant_noEffect`, `variant_reversed_mixed_primacy` | the variants on T-p.13-14 |
| `eq8_estimation`, `eq8_evaluation` | effective weights under Eq. (8): `1 - w` (estimation) or `1` (evaluation) on `s(x_1)`, and `w c_j` on each later item |
| `eq8_estimation_first_dominates`, `eq8_evaluation_first_dominates` | the first item dominates if `w < 1/2` (estimation) or `w < 1` (evaluation), for any averaging aggregate |
| `eq8_estimation_uniform_iff`, `eq8_estimation_not_always` | with a plain mean over `n` later items, the first item dominates iff `w < n/(n+1)` |
| `eos2_est_orderEffect`, `eos2_eval_orderEffect` | two-item EoS: `D = (2w-1)(s_b - s_a)` and `D = -(1-w)(s_b - s_a)` |
| `eos_symmetric_noEffect` | Eq. (5) with an explicit `S_0` and a symmetric aggregate gives no order effect |
| `oneSided_eq_eq8`, `oneSided_eq_positional` | the one-sided construction (first cue in full, second damped by `w`) **is** Eq. (8) with `k = 2`, and is the positional model with `w₁ = 1` |
| `oneSided_orderEffect`, `oneSided_primacy_iff` | one-sided: `D = (2w-1)(s_b - s_a)`, so primacy holds iff `w < 1/2` |
| `twoSided_orderEffect`, `twoSided_recency` | HE's SbS rule, same `w` on both cues: `D = w²(s_b - s_a) > 0` |
| `sided_gap`, `sided_disagree` | the two differ by `(1-w)²(s_b - s_a)` and give opposite predictions for `0 < w < 1/2` |
| `twoAttr_twoSided_noEffect`, `twoAttr_oneSided_orderEffect` | two attributes, each cue moving only its own attribute: HE's rule gives no order effect, and the one-sided rule gives `(1-w)(s_A - S_A)` |

`sympy/check_order_effects.py` (34/34 PASS) checks all of the above
symbolically or in exact rationals. It also includes:

* a 4000-draw exact sweep of mixed evidence under HE's weights, in which every
  draw showed recency;
* a consistent-evidence sweep, in which 619 of 12812 draws (4.8%) showed
  primacy;
* HE's own illustration (T-p.7-8: `S_0 = .5`, items `.6`/`.9`), which shows
  recency for every `(α, β)` in `{.1,…,1}²`;
* Experiment 5's stimulus values (60% and 80%, consistent evidence, estimation
  mode; T-p.23). On a grid of `S_0`, `α` and `β`, these give primacy only when
  `S_0 ≤ .25`, and recency whenever `S_0 ≥ .3`;
* Table 2's short-simple cells, re-derived.

`sympy/check_counts.py` (38/38 PASS) checks the following:

* Table 1's row totals.
* The head counts on T-p.5 and T-p.12: 76 data points, 43 short-simple, 19 of
  27, 16 of 16, 9 of 11 and 14 of 16.
* Table 3's column totals and the 61% and 75% on T-p.18.
* The other percentages and differences of means quoted in Experiments 1-4.

The only discrepancy is 35/47, which HE print as 75%. It is 74.5%, so this is a
rounding slip.

## What formalizing revealed

1. **Appendix B does not prove the text's claim for HE's own model.** The
   sentence "when `R = S_{k-1}` the SbS process always predicts recency (for
   α, β ≠ 0)" (T-p.11; Table 2) is proved only under Anderson-Hovland weights.
   HE say this themselves on T-p.35. With the contrast weights (6a/6b):
   * **Mixed evidence** (one item below the prior anchor, one above) always
     gives recency. `contrast_mixed_recency` proves this for all
     `α, β ∈ (0,1]` and `S_0 ∈ [0,1]`, and it is new. The closed form has only
     nonnegative terms.
   * **Consistent evidence** can give **primacy**. This happens even with
     equal sensitivities (`α = β = 1`), and even without overshoot. In the
     no-overshoot case, primacy holds exactly when
     `β(s_a + s_b - 2S_0) > 1`. So Table 2's "Recency" in the `R = S_{k-1}`
     SbS column, and the recency prediction that Experiment 5 tests
     (T-p.22), need a parameter restriction that HE do not state.
     HE's own numerical illustration lies inside the recency region. So do
     the Experiment 5 stimuli for any initial anchor of `.3` or more. The
     prediction HE tested is therefore safe in practice, but it is not the
     theorem their text states.
2. **EoS primacy (Eq. 8) is unconditional only in evaluation mode.** In
   estimation mode the anchor item's effective weight is `1 - w`. It beats a
   later item only if `w < n/(n+1)` with a plain mean, or if `w < 1/2` for every
   averaging aggregate. With two items, EoS estimation gives primacy iff
   `w < 1/2` and recency iff `w > 1/2`. HE's hedge "for a wide range of
   functions" is therefore needed.
3. **Where primacy comes from in HE.** Primacy comes from the first item being
   the anchor, that is, from its being adopted in full with weight 1 (Eq. 8). It
   also comes from weights that fall with position. In the positional form,
   primacy holds iff `w₂ < w₁/(1+w₁)`. A decline in the weight is necessary but
   not sufficient. Primacy does **not** come from partial adjustment as such:
   partial adjustment of every cue from a prior anchor gives `D = w²(s_b - s_a)`,
   which is recency.
4. **The project's one-sided construction is HE's Eq. (8), not their SbS
   model.** First cue in full and second damped by `w`, on one scalar, is
   exactly Eq. (8) with `k = 2`, estimation mode and anchor `s(x_1)`
   (`oneSided_eq_eq8`). Its order effect is `(2w-1)(s_b - s_a)`. HE's SbS model
   with the same `w` gives `w²(s_b - s_a)`. The two disagree in sign for every
   `w ∈ (0, 1/2)`.
5. **In the two-attribute setting the whole order effect comes from full
   adoption.** Suppose each cue moves only its own attribute, as at `c = 0` in
   `JeffreyOrder/Anchoring.lean`. Then HE's rule, applied with the same weight
   to every cue, gives no order effect at all. The one-sided rule gives
   `(1-w)(s_A - S_A)` on the attribute read first, which is the scalar form of
   `orderEffect_damped_at_indep`'s `(1-δ)(α - q₀)`. That attribute is
   "protected" because it is adopted in full, not because anything is damped.

## Bearing on Paper B

HE are the source of Paper B's rival mechanism, and the attribution has to
follow the process distinction that HE themselves draw. The damped family in
`JeffreyOrder/Anchoring.lean` is not "their model". It has the structure of
their **End-of-Sequence** anchoring equation (Eq. 8, two items, estimation mode,
constant weight), embedded in a two-attribute joint by Jeffrey steps. It drops
the contrast weights that HE call "critical to the belief-adjustment model"
(T-p.28). Their **Step-by-Step** partial adjustment, in which every cue
including the first is damped from a prior anchor, points the other way:
recency, with the later cue weighted more.

Paper B's identification argument, that the location of the `c`-invariant
marginal separates `δ = 1` from `δ = 0`, is untouched. Only the attribution and
the primacy/recency vocabulary change.

`w = 0` is inside HE's range (`0 ≤ w_k ≤ 1`, T-p.6). With Eq. (8), `w = 0`
gives `S = s(x_1)`, a first impression never moved. So the `ω = 0` endpoint has
a counterpart in HE. What HE do not have is the two-attribute Jeffrey embedding.

## Audit findings (2026-09-29)

This section summarises `notes/citation_audit/verify_hogarth_asch.md` §1 for
this paper and sets it against what is proved above.

* **Source (§0).** The PDF is a LaTeX transcription. Journal pages, the issue
  number and the DOI cannot be checked. CP:332/450/1264 say "checked against the
  text (Drive: `hogarth_einhorn_1992.pdf`)" but do not say that the text is a
  transcription. Confirmed.
* **H1-H5, H9, H11, H18, H20, H21 (VERIFIED)** are confirmed on re-reading:
  * Eq. (1) is on T-p.6 and Eqs. (2)-(4) are on T-p.7.
  * The memory quote is on T-p.29.
  * The attention-decrement passage is on T-p.6.
  * The SbS/EoS definitions are on T-p.3-4.
  * The bibliographic entry matches.
* **H7 (CONTRADICTED): "primacy in their model comes from decaying weights over
  a long series".** Confirmed, with detail added. The main source of primacy is
  EoS anchoring (Eq. 8, T-p.12), which is matched to 19 of 27 studies. Decaying
  weights are the secondary source (T-p.11, T-p.13). The formalization adds two
  points:
  * EoS primacy in estimation mode needs `w < 1/2` for two items (point 2
    above).
  * A decline in weight produces primacy only when `w₂ < w₁/(1+w₁)`.
* **H8 (CONTRADICTED, partly): "the ω = 0 endpoint is ours, not theirs".**
  Confirmed, and strengthened. HE allow `w = 0`, and Eq. (8) with `w = 0` leaves
  the first impression unmoved. The audit's residual claim "the asymmetric
  construction is new" should be narrowed too. The asymmetry itself (first item
  unweighted, later items weighted by `w`) is Eq. (8). What is new is the
  two-attribute Jeffrey embedding and the absence of a prior anchor under
  SbS-style reporting.
* **H13/H14 (VWC): "their model", "the rival is their model, not a
  strawman".** Confirmed. The family is HE's Eq. (3)/(8) algebra with a constant
  weight and no contrast weights. HE assign the constant-weight `R = S_{k-1}`
  model to Anderson and Hovland (T-p.11). One qualification to the audit's
  wording "HE also apply w to every item, including the first": this is true of
  SbS, but under EoS (Eq. 8) HE do not weight the first item.
* **H15 (CONTRADICTED): "partial adjustment in the manner of Hogarth and
  Einhorn protects the *first* impression"** (PD:164-166, CP:1300-1303,
  RL:802-803). **Confirmed.** In HE, SbS partial adjustment gives recency:
  * `appB_recency` proves this under Anderson-Hovland weights.
  * `contrast_mixed_recency` proves it under HE's own weights for mixed
    evidence.
  * `appC_mixed_recency` proves it for `R = 0` with mixed evidence.

  **Qualification to the audit.** The audit says HE "prove" that SbS with
  `R = S_{k-1}` "always" gives recency. HE assert this but prove it only under
  Anderson-Hovland weights. Under HE's own weights, consistent evidence can give
  primacy (`contrast_consistent_primacy_*`). This does not rescue the project's
  sentence. The primacy cases are a parameter region that HE never identify,
  and HE state the opposite. What protects the first impression in HE is
  anchoring on it, which is full adoption. It is not partial adjustment.
* **H16 (CONTRADICTED): the "position channel … weights the later cue less",
  cited to HE and Asch** (IO:62-65, CP:203-205). Confirmed for HE. SbS gives the
  later cue *more* effective weight (`twoSided_orderEffect`: `w` on the last cue
  vs `w(1-w)` on the first). Only EoS anchoring gives it less. For Asch, see
  `literature/asch1946/README.md`.
* **H22 (UNSUPPORTED): interior ω is "the primacy regime"** (IO:323-324).
  Confirmed: `oneSided_primacy_iff` gives primacy iff `w < 1/2` on a single
  scalar, and recency for `w > 1/2`.
* **H6, H10, H12, H17, H19, H23 (VWC)** were not re-examined beyond the
  following:
  * Table 1 and the 76 data points are checked in `check_counts.py`.
  * HE's own experiments found no primacy (Table 3; Exps 1-5).

### What HE's model predicts, precisely

The table below is for two items with `s_a < s_b`. `D = S_ab - S_ba`, where
`D > 0` means recency.

| Process | Encoding | Weights | Prediction |
|---|---|---|---|
| SbS, prior anchor `S_0` | `R = S_{k-1}` | per item (Anderson-Hovland) | recency, `D = w_a w_b (s_b - s_a)` |
| SbS | `R = S_{k-1}` | by position `w₁`, `w₂` | recency iff `w₂(1+w₁) > w₁`; primacy iff `w₂(1+w₁) < w₁` |
| SbS | `R = S_{k-1}` | HE's contrast weights (6a/6b) | mixed evidence: recency always (new proof). Consistent evidence: parameter-dependent, primacy possible |
| SbS | `R = 0` | HE's contrast weights | mixed: recency, `-αβ s⁻ s⁺`; consistent: none |
| EoS, first item as anchor (Eq. 8) | `R = s(x_1)` | constant `w` | primacy iff `w < 1/2`; recency iff `w > 1/2` |
| EoS, first item as anchor (Eq. 8) | `R = 0` | constant `w` | primacy for every `w < 1` |
| EoS with explicit `S_0` (Eq. 5) | any | symmetric aggregate | no order effect |

**Verdict.** "Partial adjustment protects the first impression" is **not
attributable to Hogarth and Einhorn.** In their model, damping every cue from a
prior anchor protects the *last* impression. The first impression is protected
only when it *is* the anchor (EoS, Eq. 8), and in estimation mode only when
`w < 1/2`. The project's construction (first cue in full, second damped) has
exactly that EoS structure. It can be attributed to HE under that description,
but not as "partial adjustment" in general, and not as their model.

### Proposed corrected wording (not applied)

* **PD:164-166**, replace "The rival rational account of order effects, partial
  adjustment in the manner of Hogarth and Einhorn, protects the \emph{first}
  impression; amnestic updating protects the \emph{last}." with:
  > The rival account of order effects is anchoring and adjustment in the manner
  > of Hogarth and Einhorn. When the first cue sets the anchor and later cues
  > move belief only part of the way (their End-of-Sequence process), it
  > protects the \emph{first} impression; amnestic updating protects the
  > \emph{last}. (Their Step-by-Step process, which damps every cue including
  > the first, predicts recency instead.)
* **CP:1298-1303** (proposed 6.A text), replace from "Under partial adjustment"
  to "the last one here." with:
  > Under anchoring and adjustment \citep{HogarthEinhorn1992}, belief is anchored
  > on the first cue and later cues move it only part of the way to their
  > targets. At zero adjustment weight the first impression is never moved.
  > (When every cue, the first included, is only partly adopted from a prior
  > anchor, the same authors predict recency.) Anchoring on the first cue and the
  > amnestic model point in opposite directions: the protected impression is the
  > first one there, and the last one here.
* **IO:62-65** (and CP:203-205, writing_discipline.md:84), replace "a
  \textbf{position} channel, in which the observer weights the later cue less
  whatever the attributes are \citep{HogarthEinhorn1992,Asch1946}" with:
  > a \textbf{position} channel, in which the weight a cue receives depends on
  > where in the sequence it arrives, whatever the attributes are (in
  > \citet{HogarthEinhorn1992} anchoring on the first cue favours it, while
  > step-by-step partial adjustment favours the later one)

  Drop `Asch1946` from this citation. Asch denies that "sheer temporal position"
  is what matters (p.272).
* **AL:14-21** (docstring of `lean/JeffreyOrder/Anchoring.lean`), replace
  "This is the averaging form … Eq. 4 … not theirs." with:
  > This is the averaging form `S_k = (1-w_k) S_{k-1} + w_k s(x_k)` of Hogarth and
  > Einhorn (1992, Eq. 4), applied to the second cue only. With the first cue as
  > the anchor, it is their End-of-Sequence form (Eq. 8) for two items, with a
  > constant weight and without their contrast weights (Eqs. 6a/6b). Their
  > "memory is limited to the location of one's current anchor and not how this
  > was reached" (General Discussion) is a path-independence property, shared by
  > every `δ`. In their model primacy arises mainly from anchoring on the first
  > item under End-of-Sequence processing, and also from weights that decline
  > over a long series. Their Step-by-Step process, which damps every cue from a
  > prior anchor, predicts recency (Appendix B). `δ = 0` corresponds to `w = 0`
  > in their Eq. 8, which their range `0 ≤ w_k ≤ 1` allows. What is this file's
  > construction is the embedding in a two-attribute joint by Jeffrey steps.
* **CP:446-448 and CP:1265-1267**, replace "(Hogarth--Einhorn's primacy comes
  from decaying weights over long series)" / "primacy in their model comes from
  weights decaying over a long series, so the ω = 0 endpoint is ours, not
  theirs" with:
  > in Hogarth--Einhorn, primacy comes mainly from anchoring on the first item
  > under End-of-Sequence processing (their Eq. 8), and from weights declining
  > over long series; their Step-by-Step partial adjustment predicts recency. The
  > ω = 0 endpoint corresponds to w = 0 in their Eq. 8; the two-attribute
  > embedding is ours.
* **CP:787-788**, replace "where Hogarth--Einhorn's primacy comes from weights
  decaying across many cues" with "where Hogarth--Einhorn obtain primacy from
  End-of-Sequence anchoring and from weights declining across many cues".
* **RL:721-723 / RL:800-801 / DP:322-325**, replace "This is Hogarth-Einhorn's
  own adjustment equation … so the rival is their model, not a strawman" /
  "the family is their eq. (1) with `R = S_{k-1}`" with:
  > the damped step is Hogarth--Einhorn's estimation-mode adjustment (their
  > Eq. 3/4) with a constant weight on the second cue only; with the first cue as
  > the anchor, this is their End-of-Sequence form (Eq. 8). It omits their
  > contrast weights, so it is a stripped-down relative of their model and not
  > the model itself.
* **IO:323-324**, replace "The primacy regime is taken for the interior, since
  with ω on the second cue every interior value weakens the later impression."
  with:
  > The interior is taken as the anchoring regime: with ω on the second cue every
  > interior value leaves the first-read attribute at its delivered credence and
  > the last-read one short of it. (In Hogarth--Einhorn's single-scalar sense this
  > is primacy only for ω < 1/2.)
* **CP:298-299**, replace "\citet{HogarthEinhorn1992} find primacy, recency or
  no order effect, depending on the characteristics of the task" with:
  > \citet{HogarthEinhorn1992} classify earlier studies as showing primacy,
  > recency or no order effect, and predict which will occur from the task and
  > the response mode: primacy for short series of simple items judged at the end,
  > recency when a judgment follows each item

## Not formalized

* Long series with α and β declining over more than two items. Only the
  two-item positional form is covered.
* Boundedness of `S_k` in `[0,1]` along a whole sequence. Only the weight bound
  is covered.
* The complexity and length conditions of Fig. 1, and the Complex and Long rows
  of Table 2. These are assumptions about which process subjects use, not
  consequences of the equations.
* The inferential statistics of Experiments 1-5. Only the counts and quoted
  percentages are checked.
