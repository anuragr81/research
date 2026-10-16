# Laouénan and Rathelot (2022), reconstruction

Morgane Laouénan and Roland Rathelot, "Can Information Reduce Ethnic
Discrimination? Evidence from Airbnb", *American Economic Journal: Applied
Economics* 14(1), January 2022, pp. 107–132, doi:10.1257/app.20190188.

## Source read

- The published article, 26 PDF pages, cached at
  `~/.cache/paper_a/sources/laouenan_rathelot_2022.pdf` (160,772 bytes),
  sha256 `4b567652…a5c77589`, with its text layer at
  `~/.cache/paper_a/sources/laouenan_rathelot_2022.txt`, sha256
  `3c753d24…a42d1`. The text has 27 form-feed segments. Segment `n − 1` is PDF
  page `n`, and journal page `n + 106`. Segment 26 is empty.
- Every page, 1 to 26, was read from the text on 16 October 2026, so the
  evidence tag is [F]. PDF pages 11, 12 and 13, where the extraction drops `𝒩`
  and garbles eq. (3), were also read from 110 dpi images rendered with
  `pdftoppm`. The online appendix (Sections C to F, with the proofs of
  Predictions 0 to 2) is not in the file and is not read. Nothing beyond these
  pages is attributed.
- A second copy, the author's accepted manuscript from WRAP
  (`laouenan_rathelot_wrap.pdf`, 32 PDF pages, 920,188 bytes, sha256
  `0d60f518…c5908a`, text sha256 `f7fcdedb…f4ce8a9`), was read and compared at
  the level of results. It agrees with the article (LR-D5), and no separate
  record is made.

## The setting

- Airbnb hosts post a listing and a daily price. Guests book at that price, and
  the host may accept or reject (PDF pp. 5–6). Guests are treated as price
  takers, so a fall in demand for a minority host's listing shows up as a lower
  price (PDF p. 6).
- Data (PDF pp. 3, 6, 8). 663,090 listings in 19 cities of North America and
  Europe, 21 waves from June 2014 to November 2017, restricted to the 220,939
  listings with at least one review, 2,474,551 listing-wave observations in the
  regressions. Minority hosts are those coded Black from the profile picture
  and those with an Arabic, Muslim or Sub-Saharan African first name, 6.2
  percent of observations.
- Raw gaps (PDF pp. 8–9). Within city and wave the minority gap in log price is
  −0.169. Property characteristics bring it to −0.111, location to −0.067, and
  both to −0.032 (Table 2).

## The model, from its own primitives (Section II)

1. **Host's program** (PDF p. 11). The host splits time `L` and `1 − L` between
   renting, with `L^{α̃}` nights, and a job paying `W`. Demand is
   `D = Q^β/(P^κ Γ^m)`, with `Γ > 1` the taste-based discount on a minority
   host (`m = 1`). The host chooses `P` to maximise
   `P D(P) + (1 − D(P)^{1/α̃}) W`.
2. **Eq. (1)** (PDF p. 11). `p = p₀ + λαw + λβq − λγm`, with `p = log P`,
   `w = log W`, `q = log Q`, `γ = log Γ`, `α = α̃/(1 − α̃)`, `λ = (κ + α)⁻¹`.
3. **Imperfectly observed quality** (PDF pp. 11–12). `q = ζ + ν`, with `ζ` seen
   by guests and `ν | m ~ 𝒩(ν̄_m, σ_ν²)`. Each review is a signal of `ν` with
   error variance `σ²`, and the average of `K` reviews is `r ~ 𝒩(ν, σ²/K)`.
   With `ρ = σ²/σ_ν²`, `E(ν | r, K, m) = (K r + ρ ν̄_m)/(K + ρ)`.
4. **Eq. (2)** (PDF p. 12). The host prices at the guests' expected quality,
   `p = p₀ − λγm + λαw + λβζ + λβ (K r + ρ ν̄_m)/(K + ρ)`.

## Derivation chain (Section III)

- **Eq. (3), Prediction 0** (PDF p. 12). Write the posterior mean as
  `ν̄_m + f(K)(r − ν̄_m)` with `f(K) = K/(K + ρ)` (`postMean_eq`). The terms
  constant in time (`p₀`, `λγm`, `λαw`, `λβζ`, `λβν̄_m`) go into the listing
  effect `μ_i`. What moves with reviews is `λβ f(K)(r − ν̄_m)`. Proxying `r` by
  the last observed rating `r̄_i`, in bins, gives eq. (3), whose coefficient on
  `m_i f(K_it)` is `−βm` with `βm = λβ(ν̄₁ − ν̄₀)` (`eq3_form` at accurate
  beliefs).
- **Predictions 1 and 2** (PDF pp. 13–14). Guests hold the true `ν̄₀` and a
  possibly wrong `ν̃₁`. Conditioning on the rating, eq. (3) identifies
  `βm = λβ(ν̃₁ − ν̄₀)` (`eq3_form`). Without the rating interactions, `r`
  averages to the group's true mean and eq. (4) identifies
  `β̃m = λβ(ν̃₁ − ν̄₁)` (`eq4_form`). Their difference is the true gap
  `λβ(ν̄₁ − ν̄₀)` (`decomposition`). With accurate beliefs `β̃m = 0`
  (`accurate_beliefs_tilde_zero`), and with equal true means `βm = β̃m`
  (`equal_means_same`, `control_equal_means_needed`).
- **What vanishes and what does not** (PDF pp. 2–3). Two listings of the same
  quality differ in price by `−λγ + λβ(ν̄₁ − ν̄₀) ρ/(K + ρ)`
  (`gap_same_quality`). Without a taste term this tends to zero
  (`gap_tendsto_zero`), with one it tends to `−λγ`
  (`control_taste_gap_survives`). The group averages keep `λβ(ν̄₁ − ν̄₀)` at
  every `K` (`group_average_gap_constant`).
- **Estimates** (PDF pp. 14–15, Table 4). `−βm = 0.034`, `−β̃m = 0.025`,
  `ρ = 13.7`. Three-quarters of the statistical gap is inaccurate beliefs and
  one quarter the true difference. The Table 3 gap for unreviewed listings,
  0.034, equals `−βm`, which the authors read as the whole initial gap being
  statistical.
- **Robustness and alternatives** (PDF pp. 16–23). Linear and quadratic forms in
  `K` (Table 5) and splines for majority hosts (Figure 2) agree with the
  concave `K/(K + ρ)`. By subsample, Table 6. Ethnic matching of guests to hosts
  is 0.81 points (Table 7). Minority guests do not rate minority hosts
  differently (Table 8). Minority hosts do not upgrade more (Table 9). Initial
  under-pricing by pessimistic minority hosts is small (Table 10).

## What could not be reconstructed

The coverage table in `CLAIMS.md` has one row per numbered result. What is
short of general is this.

- Normal–normal conjugacy. The posterior mean is checked as the
  precision-weighted average (`precision_weighted`). That this is the Bayesian
  posterior under normal priors and signals is taken from the paper, since
  Mathlib has no Gaussian conjugacy.
- Predictions 0, 1 and 2. Their content as statements about the conditional
  mean of the log price is formalised (`model_is_eq3`, `model_is_eq4`), with the
  signal `r` known. The paper proxies `r` by the last rating in four bins and
  estimates by nonlinear least squares. The proxy step and the properties of
  the estimator are not formalised, and the proofs in the online appendix,
  Section F, are not read. Under the deterministic reading eq. (4) has `βk = 0`.
- Eq. (1). The interior first-order condition is solved
  (`eq1_from_foc`). The second-order condition and existence of a maximum are
  not formalised. The printed constant `p₀` has the opposite sign (LR-D2).
- Eq. (6) and the Table 8 equation are specifications with no derived claim and
  are not formalised. The beta-prior variant of the online appendix, Section E,
  is not read.
- The paper's conclusions about taste-based discrimination are limited by its
  design. A level penalty, whether taste or an unobserved differential, is
  absorbed by the listing effect (LR-Q9, LR-Q14, `control_zeta_varies`).
