# Fang and Moro (2010), reconstruction

Hanming Fang and Andrea Moro, "Theories of Statistical Discrimination and
Affirmative Action: A Survey", NBER Working Paper 15860, April 2010. It was prepared
for the *Handbook of Social Economics*, Vol. I, edited by J. Benhabib,
A. Bisin and M. Jackson (North-Holland, published 2011). The working paper is
the version read and is cited as such (FM-D1).

## Source read

- The NBER working paper, 78 PDF pages with a text layer. Cached at
  `~/.cache/paper_a/sources/fang_moro_2011.pdf` (692,208 bytes), sha256
  `30fc4e8f…e31a`, and its extracted text `fang_moro_2011.txt`, sha256
  `ce41bc90…a09e`. The cache file names carry
  2011, the Handbook year, but the file is the 2010 working paper.
- The text of every page, PDF pages 1 to 78, was read on 16 October 2026, so
  the evidence tag is [F]. PDF pages 9, 27, 57 and 66 were also rendered at
  110 dpi and read from the image, for the equations of Sections 2.2.1, 4.2
  and 7.1 and for the figure page 57 (FM-D8).
- Pages in this record are PDF pages. PDF page `n` is printed page `n − 4`
  from PDF page 5. PDF pages 69 to 78 are the references.

## The survey, from its own structure

1. **Introduction** (pp. 5–7). Statistical discrimination is the use of
   observable characteristics as a proxy for unobservable, outcome-relevant
   ones. The marker can be chosen, "such as club membership or language"
   (FM-Q1). The literature is mostly about labour markets (FM-Q2).
2. **Exogenous differences** (pp. 7–11).
   - Signal extraction, eq. (1), with `q ~ N(μ_j, σ_j²)`, `θ = q + ε`,
     `E(q|θ) = σ_j²/(σ_j² + σ_εj²) θ + σ_εj²/(σ_j² + σ_εj²) μ_j`.
   - Phelps's Case 1 (lower mean) and Case 2 (noisier signal). In Case 2 the
     groups' average wages are equal (FM-Q3).
   - Aigner and Cain (1977). With CARA utility and
     `Var(q|θ) = σ_j²σ_εj²/(σ_j² + σ_εj²)`, the noisier group is paid less on
     average (FM-Q5, FM-Q6).
   - Lundberg and Startz (1983). The noisier group invests less, eq. (2).
   - Cornell and Welch (1996). A more informative signal gives a higher
     variance of expected productivity, so the dominant group fills the top
     slots (FM-Q7).
3. **Coordination failure** (pp. 11–19). Arrow (1973); Coate and Loury
   (1993a), with the posterior eq. (4), threshold rule eqs. (6) to (9), investment
   incentive eqs. (10) to (14), and equilibrium eqs. (15) to (17), in which the
   firms' belief `π_j` about a group equals the group's realised investment.
   Groups are separate "islands" with no interaction.
4. **Inter-group interactions** (pp. 19–35). In Moro and Norman
   (2004), complementarities in production make groups specialise. Mailath,
   Samuelson and Shaked (2000). Firms search only one group, with full
   information about skill (FM-Q8). Fang (2001). An a priori irrelevant
   cultural activity becomes an observable marker of skill through
   self-selection, with an equilibrium fixed point `Ψ(π_A) = π_A` on the share
   of skilled among those who take it (FM-Q9). Chaudhuri and Sethi (2008)
   have peer effects through a segregation parameter `η` (FM-Q10).
5. **Dynamics** (pp. 35–36). Antonovics (2006), Fryer (2007), Blume (2006),
   Levin (2009), and Eeckhout (2006)'s marriage-market search with race as a
   public randomisation device (FM-Q12).
6. **Affirmative action** (pp. 36–66). Coate and Loury's quota and the
   patronising equilibrium; Moro and Norman (2003a); Fang and Norman (2006);
   Chung (2000) on role models; Chan and Eyster (2003) and Fryer and Loury
   (2008) on colour-blind admission; Epple, Romano and Sieg (2008) on colleges
   competing with aid and admission (FM-Q13); Chung (1999), Abdulkadiroğlu
   (2005), Fu (2006).
7. **Efficiency** (pp. 66–71). Phelps's model with and without the use of
   race, the pooled variance (FM-D3); Lundberg and Startz; Schwab (1986), a
   standardised market that pays the pool average (FM-Q14); Norman (2003);
   Moro and Norman (2004).
8. **Conclusion** (pp. 71–73). Open questions on the evolution of stereotypes
   (FM-Q15), agency in affirmative action, and identification. Empirical
   tests are mentioned in footnotes, among them Altonji and Pierret's test
   with employer learning (FM-Q11).

## Derivation chain of what Paper A1 uses

The Lean file has one section per model the survey presents, in the survey's
notation (`CLAIMS.md`, Lean results).

- Phelps (Section 2.1). Eq. (1) (`eq1_weights_sum`), Case 1
  (`phelps_case1`), Case 2 with equal averages (`phelps_case2`,
  `phelps_case2_average`), and the precise signal that removes the group
  (`phelps_precise_signal`).
- Aigner and Cain, Lundberg and Startz, Cornell and Welch (Section 2.2).
  `condVar_strictMono_noise`, `aignerCain_average_wage`, `ls_eq2`,
  `ls_noisier_invests_less`, `cw_variance`.
- Arrow and Coate and Loury (Section 3). `arrow_wage_relation`,
  `arrow_lower_wage`, eqs. (4) to (17) (`post_strictMono_prior`, `eq7_iff`,
  `cl_eq8_unique`, `cl_eq9`, `cl_eq12`, `cl_eq13`, `cl_trivial_equilibrium`).
- Moro and Norman, Mailath, Samuelson and Shaked, Fang, Chaudhuri and Sethi
  (Section 4). `mn_eq18`, `mn_eq20`, `mss_eq24_25`, `mss_VB_lt_VW`,
  `fang_low_cost`, `fang_eq38`, `fang_pareto`, `cs_peer_gap`.
- Affirmative action (Section 6). `cl_aa_sufficient`, eqs. (47) to (51)
  (`patron_eq47`, `patron_eq50`, `patron_eq51`, `patron_dynamics_slope`),
  `mnaa_threshold_order`, Fang and Norman's eqs. (54), (55) (`fn_eq54`,
  `fn_eq55`, `fn_precision`), Chung (`chung_role_model`, `chung_fn20`), Chan
  and Eyster (`ce_eq58`), Fryer and Loury (`fl_colorsighted`, `fl_eq79`).
- Efficiency (Section 7). `eff_total_product`, `pool_variance`,
  `ls_net_product`, `norman_example`, `norman_eq82`, `mn_pareto_chain`.

## What could not be reconstructed

The coverage table in `CLAIMS.md` has a row for each model's numbered
equations and displayed claims, 31 general, 10 special cases and 13 not
formalised. Short of general are these.

- Results the survey states in words or with figures only, which are Cornell and
  Welch's limit, Arrow's and Coate and Loury's multiple equilibria and Pareto
  ranking, Moro and Norman's factor-ratio, cross-group and group-size claims,
  the dynamic models of Section 5, Schwab, the two-job mismatch, and the
  results attributed to Fang and Norman and to Chan and Eyster beyond the
  equations.
- Results that need a printed equation the survey omits or a limit. Eqs. (24), (25) need the firm's value equation (FM-D12); (31) to (36) are
  stated without derivation; the patronising dynamics are formalised to local
  stability only; Fang's `Δ ≥ 0` to necessity only.
- Fryer and Loury's eqs. (60) to (68) and (80), (81), quantile calculus with
  a sign problem in eq. (61) (FM-D16).
- Every result attributed to a cited model rests on the survey and is tagged
  [C] unless that model is read (FM-D10).
