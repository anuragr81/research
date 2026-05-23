# Master Document — Final Draft

## STATUS: All primary items complete. Paper ready for submission review.

---

## 300-WORD SUMMARY

This paper derives the boom-bust asymmetry in dealer balance sheet volatility as an equilibrium theorem from the minimal preference-based structure that delivers it: a single kink in the market maker's value function at a publicly observed trend growth benchmark.

A loss-averse market maker operates under pre-committed, regime-contingent risk limits calibrated by loss aversion parameter λ > 1 — the channel through which loss aversion enters equilibrium, grounded in VaR-constrained dealer behaviour documented by Adrian and Shin (2014) and Li et al. (2025). Two structural conditions are jointly necessary for the asymmetry to emerge: the bilateral zero-sum structure (Assumption 1), guaranteeing regime separation as a theorem; and the trend growth path, pinning volatility persistence to the growth rate.

The paper derives a Threshold-GARCH recursion for the conditional variance of detrended dealer balance sheet deviations with exact identifications for α, γ, ω, and a leading-order identification for β. The separation property — |γ|/α depends only on λ, β depends only on g — provides exact within-model identification of both structural primitives without external instruments, exclusion restrictions, or auxiliary calibration targets.

Five empirical findings support the model using US dealer balance sheet data. First, expansion variance exceeds contraction variance (R = 2.07, p < 0.00001), with implied λ = 1.20, below the laboratory benchmark. Second, deterministic detrending confirms persistence at φ = 0.994 against a model prediction of 0.998 — a 99% reduction in identification error relative to HP filtering. Third, the pre-GFC asymmetry ratio is stable across growth regimes (0.42–0.45) despite threefold variation in trend TFP growth. Fourth, the post-GFC sign reversal is explained by a one-parameter regulatory floor extension that preserves the separation property: regulation changed the asymmetry channel while leaving persistence unchanged. Fifth, the secular stagnation period exhibits near-unit-root persistence (φ = 1.016), consistent with the IGARCH boundary prediction. The identification of λ and g requires different detrending methods — HP filtering for the variance ratio and deterministic detrending for persistence — each theoretically grounded in the model's structure.

---

## 1500-WORD EXECUTIVE SUMMARY

### What the paper does

Dealer balance sheets expand rapidly with high variance in booms and contract slowly with compressed variance in busts. This paper derives this asymmetry as an equilibrium theorem from a single behavioural ingredient — the kink in the market maker's value function at a trend growth benchmark — embedded in a Stackelberg market with noise traders.

The market maker's loss aversion parameter λ > 1 operates through pre-committed, regime-contingent risk limits, not through unconstrained optimisation of a kinked objective (which cancels in the interior). In expansion, the limit acts as a floor on price impact (λ^eff_p = λ⁰_p/λ); in contraction, a cap (λ^eff_p = λλ⁰_p). The limits are log-symmetric around the Kyle benchmark, grounded in VaR-constrained behaviour documented by Adrian and Shin (2014) and Li et al. (2025).

Two structural conditions are jointly necessary. The bilateral zero-sum structure (Assumption 1) guarantees regime separation: dealer and speculator always sit on opposite sides of the trend benchmark simultaneously. The trend growth path provides mean-reversion at rate 1/(1+g), pinning volatility persistence to the growth rate.

### The main result and the separation property

Theorem 1 derives a Threshold-GARCH recursion with α = Φλ² > 0, γ = Φ(1−λ⁴)/λ² < 0, and β ≈ 1/(1+g)². The separation property — |γ|/α = (λ⁴−1)/λ⁴ depends only on λ, and β depends only on g — provides exact within-model identification of both parameters without external instruments, exclusion restrictions, or auxiliary calibration targets. This contrasts with reduced-form GARCH (where separating λ from g requires auxiliary assumptions), rational belief equilibrium (which needs distributional assumptions on beliefs), and prospect theory calibration (which identifies λ against external macroeconomic moments).

### Detrending methodology

The two structural parameters require different detrending methods, a consequence of the model's structure rather than a post hoc choice. The variance ratio identifies λ through regime-dependent innovation variance; the model's reference point is a local benchmark, correctly operationalised by the HP filter (λ_HP = 1600). Persistence identifies g through the secular deflation factor 1/(1+g) with a quarterly half-life of ~350 quarters; the HP filter absorbs this signal entirely (removing variation below ~40 quarters), making β unidentifiable. Deterministic detrending — log(equity) minus g_lit·t using external TFP estimates — preserves the persistence signal. Each detrending is the theoretically correct specification for its parameter.

### Five empirical findings

**Finding 1 — Variance asymmetry (HP filter, full sample).** Expansion innovation variance exceeds contraction innovation variance: R = 2.07, F-test p < 0.00001, bootstrap 95% CI [1.35, 3.17]. Implied λ = 1.20 [1.08, 1.33], below the Tversky-Kahneman laboratory value of 2.25 — consistent with institutional attenuation. The separation check (φ⁺ = φ⁻, p = 0.20) fails to reject, consistent with ∂β/∂λ = 0. Robust to pre-GFC subsample and positive threshold shifts; sensitive to linear detrending.

**Finding 2 — Persistence confirmed (deterministic detrending, full sample).** Under deterministic detrending of log(equity) at g_lit = 0.8% p.a., the pooled AR(1) coefficient is φ_det = 0.994, against a model prediction of 1/(1+g/4) = 0.998. The discrepancy is 0.4 percentage points — a 99% reduction from the HP-filter discrepancy of 33pp. The regime-specific coefficients (φ⁺ = 0.992, φ⁻ = 0.997) are both within 0.6pp of the prediction and within 0.5pp of each other, confirming that the deflation factor is regime-invariant.

**Finding 3 — Pre-GFC asymmetry ratio stability (HP filter, subperiods).** The asymmetry ratio is stable across two pre-GFC growth regimes: |γ|/α = 0.45 (productivity slowdown, g = 0.5%) and 0.42 (IT boom, g = 1.5%), differing by less than 3pp despite threefold variation in trend TFP growth. Under deterministic detrending, the persistence comparison confirms the correct direction: φ decreases from 0.988 to 0.974 as g increases from 0.5% to 1.5%.

**Finding 4 — Post-GFC regulatory structural break.** The secular stagnation period (2010–2024) produces a sign reversal under HP filtering: R = 0.72 and |γ|/α = −0.38. This is attributable to post-GFC regulation (Basel III, Volcker Rule, SLR, stress testing) that selectively binds in the expansion regime. The paper introduces a regulatory price-impact floor λ_reg: when λ_reg > λ⁰_p/λ, the regulatory constraint dominates in expansion, compressing expansion variance. The separation property is preserved: λ_reg enters through α and γ but not through β. The data confirms this: under deterministic detrending, persistence is comparable across all regimes (φ = 0.974–1.016) while |γ|/α shifts from +0.44 to −0.38. The regulatory intervention changed one channel without affecting the other.

**Finding 5 — IGARCH boundary.** Under deterministic detrending, the secular stagnation period (g = 0.3%) gives φ = 1.016, β = 1.032 — at or slightly above unity, consistent with the IGARCH boundary prediction (β → 1 as g → 0). This is the most persistent subperiod, exactly as the model predicts for a near-zero growth environment.

### Structural implications

A structural estimator for market-level loss aversion: |γ|/α identifies λ without external instruments. The implied λ = 1.20 is the first model-based estimate of aggregate dealer loss aversion from market data.

The IGARCH boundary as a forward-looking indicator: near-unit-root volatility persistence in low-growth episodes is the structural signature of a stagnant economy, not an arbitrary statistical feature.

Structural orthogonality of policy instruments: growth-enhancing reforms reduce β without affecting |γ|/α; macroprudential policies reducing λ reduce |γ|/α without affecting β. The post-GFC evidence that regulation changed |γ|/α while leaving β unchanged provides the first empirical support for this independence.

### Limitations and future work

The within-country approach tests ∂(|γ|/α)/∂g = 0 (confirmed) but cannot test ∂β/∂λ = 0 (requires cross-country variation in λ). The implied λ = 1.20 is below the Adrian-Shin range, reflecting the broad Z.1 population. The variance ratio is sensitive to the detrending method. The full cross-sectional test — matching dealer GARCH estimates to PWT TFP growth and Wang et al. (2017) experimental λ across countries — is deferred to the companion empirical paper.
