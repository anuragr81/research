# Novelty check of C2 and C4, 23 October 2026

A planning note, not part of the paper. The parent `MANUSCRIPT.tex` claims two
results new, C2 and C4. Both are candidates for the theory paper, whose
skeleton has no rows yet. This note records an internal audit, a search-level
pass (pass 0) and four full-text passes (passes 1 to 4). Further passes are planned
(author, 23 October 2026, "happy to do this multiple passes").

The claims, as the parent states them.

- **C2.** Under the two-branch regulatory cap
  $u(x)=\min\{(x-1)/a_1,(x-a_2)/a_3\}$, wherever the optimal policy binds the
  cap near both ends, buffer variance tends to the deficit limit
  $\nu_1^2=\sigma^2/a_1^2-2c\sigma\sigma_L/a_1+\sigma_L^2$ at the distress
  boundary and to the surplus limit
  $\nu_3^2=\sigma^2/a_3^2-2c\sigma\sigma_L/a_3+\sigma_L^2$ deep in surplus
  (`CapGeometry.deficit_limit`, `CapGeometry.surplus_limit`), so their ratio
  carries no information about the asymmetry parameter $\lambda_S$. Not
  claimed new: constant buffer variance on the solvency branch, the capital
  buffer itself.
- **C4.** The asymmetry parameter $\lambda_S$ and the fixed issuance cost $K$
  are locally identified from the dividend barrier and the recapitalisation
  trigger, because both raise the barrier and they move the trigger in
  opposite directions (M4 to M6). Not claimed new: the responses of the
  barrier and the target to $K$ alone.

## Rules of the passes

Only text read in a paper's own PDF counts. Each PDF was fetched from arXiv or
the author's repository, converted with `pdftotext -layout`, and quoted with
its PDF page. A formula derived by us from a paper's dynamics is labelled as
ours. Under `papers/MANUSCRIPT_SKELETON.md` a full-text reading here checks the
claim and does not yet make a `lit/` record. A paper that enters a row needs
its record, with its suite, before it is cited.

Reachable hosts on 23 October 2026: arxiv.org and eprints.gla.ac.uk.
academic.oup.com answers 403 to scripted requests. Every other paper host
tried was refused by the network policy (nber.org, bis.org, ssrn.com,
jstor.org, sciencedirect.com, epfl.ch, tse-fr.eu, banque-france.fr,
projecteuclid.org, core.ac.uk, hal.science, econstor.eu, web.archive.org,
the university hosts of the cached candidates).

## 1. Internal audit

1. **Placement.** The parent puts C2 and C4 in its table of conclusions and
   leaves the pending table empty. Section 4.4 of the skeleton guideline keeps
   a claim that a result is new in the pending table "until every paper that
   could contain the result has been read in full". By that rule C2 and C4
   are pending until the readings of section 5 are done.
2. **C4's mechanism in Peura and Keppo's Table 2.** Peura and Keppo is read in
   full (`lit/peura_keppo_2006`), and our replication reproduces its Table 2
   (`replication/peura_keppo_2006/results_stage1.md`). Printed values, issue
   barrier $u_1$ and dividend barrier $u_2$ in percent.

   | Delay $\Delta$ | $K=0.10$ | $K=0.25$ | $K=0.50$ | $K=1.00$ |
   |---|---|---|---|---|
   | 0.08 | 0.9 / 2.4 | 0.8 / 2.7 | 0.7 / 2.9 | 0.6 / 3.1 |
   | 0.5 | 1.6 / 3.2 | 1.3 / 3.4 | 1.1 / 3.5 | 0.9 / 3.7 |
   | 1.0 | 1.7 / 3.5 | 1.4 / 3.7 | 1.1 / 3.7 | 0.6 / 3.8 |

   $K$ lowers the issue barrier and raises the dividend barrier in every row.
   $\Delta$ raises the dividend barrier in every column and the issue barrier
   at $K=0.10$ and $0.25$. At those points the two parameters move the
   trigger in opposite directions and the barrier in the same direction, the
   configuration of M6. Peura and Keppo state no identification.

## 2. Pass 1, full text

Seventeen papers. Every quotation below was checked against the PDF text.

### Read in full, bearing on C4

| Paper | Version, sha256 | What it has | Overlap with C4 |
|---|---|---|---|
| Dai, Huang and Keppo (2019), "Opaque bank assets and optimal equity capital", JEDC 100:369-394, read as chapter 2 of Huang, "Two stochastic control problems in capital structure and portfolio choice", NUS thesis, arXiv:2107.02242 | v1, `985a281d96915a84`, 167 pp | The bank model of Peura and Keppo with noisy accounting: dividend barrier $u_2$, recapitalisation barrier $u_1$, issue delay, a fixed cost $K$ "of equity issuance" (p. 30), a Basel floor $\kappa$ (p. 28). Table 2.1 (p. 76) gives the elasticities of $u_2$ and $u_1$ in seven parameters, among them $K$ (0.0017, −0.0543) and $\Delta$ (0.0808, −0.1444). The regulator's liquidation asymmetry $a=c_2/(c_1+c_2)$ (p. 35) lowers both barriers (pp. 71-72). $K$ is fixed from the literature (p. 77), the rest calibrated "by fitting the partially observed value function (2.19) to the realized market equity values in the least square sense" (p. 84) | Close in setting, partial in result. The $K$ column has C4's signs, numerically. A full threshold response matrix is tabulated, so pairs of parameters are locally identified from $(u_2,u_1)$ by their numbers, which the paper does not state. No shareholder shortfall penalty or asymmetry parameter. Liabilities deterministic ("We ignore debt volatility and debt financing", p. 26, n. 2) |
| Angelis (2025), "Price setting and price stickiness: A behavioral foundation of inaction bands", JEEA 23(3):1095-1132, accepted manuscript of November 2023, eprints.gla.ac.uk/342107 | `5b94e1179e46e49c`, 52 pp | Loss aversion $\lambda$ alone gives an inaction band with two targets. The dynamic model "follows Nakamura and Steinsson (2008) in all dimensions but the menu-cost assumption" (p. 14), so there is no fixed cost. $\lambda$ is calibrated with $\sigma_\nu,\rho$ to three moments, $\lambda=7$ (p. 20). Static band edges $\lambda^{\pm1}\frac{\epsilon}{\epsilon-1}A^{-1}w$ (p. 8) | Partial. A preference parameter pinned by a threshold pair has a closed-form one-parameter analogue (the ratio of the band edges is $\lambda^2$, our reading of p. 8, not stated there). No fixed cost, so no joint identification |
| Wang, Wang and Wu, arXiv:1807.11171, "Dividend and capital injection optimization with transaction cost for spectrally negative Lévy risk processes" | v4, `e3e26db73dece43f`, 32 pp | Fixed cost on lump-sum dividends, proportional cost $\phi$ on injection, injection a reflection at 0. Tables 1 and 2 (pp. 29-30): "z1 is seen to have a slow but steady downward trend when c increases while z2 has a solid upward trend"; "z1 and z2 are seen to have steady upward trends when φ increases" | Partial. Two dividend thresholds moving with two cost parameters, numerically. No injection trigger, no penalty, no identification |
| Lakner and Reed, arXiv:2206.04107 | v1, `89721a52b8646611`, 27 pp | Fixed and variable costs on deposits and withdrawals, band $a<\alpha\le\beta<b$ (p. 6), a symmetric quadratic cost around a target (p. 18). No comparative statics | Partial, structure only |
| Mguni, arXiv:1805.01578, "Optimal capital injections with the risk of ruin" | v3, `a51b251b884a033e`, 38 pp | Fixed plus proportional injection cost (p. 4), no dividends, no comparative statics | Structure only |
| Junca, Moreno-Franco and Pérez, arXiv:1808.02182 | v2, `0bcdd661b172c4ad`, 25 pp | Fixed cost on dividends, injection a reflection with a unit cost $\Lambda$ (p. 4) | None |
| Federico, Ferrari and Torrente, arXiv:2309.16303 | v2, `d7aa4d11f5dd426e`, 22 pp | Fixed cost on activating reinsurance; "The function K ↦ x∗b∗ is strictly increasing" (p. 9) | None |
| Dudziak and Schmidli, arXiv:2510.25494 | v2, `5946ce1d5039967c`, 25 pp | Indicator drawdown penalty weighted by $\beta$, dividends at a bounded rate, no injection | Partial, a penalty weight moving dividend thresholds only |
| Tse, arXiv:1810.03501 | v1, `bd5cd18ff8448ce1`, 33 pp | Log utility, no equity issuance ("we rule out the possibility of equity issuance", p. 6) | None |
| Guan and Xu, arXiv:2604.04641 | v1, `a755d2c336e11bd3`, 38 pp | Ratcheting, proportional injection cost | None |
| Aubert, Chevalier and Ly Vath, arXiv:2511.19701 | v2, `4543f45c6eb65dd4`, 38 pp | Proportional injection cost $\delta$, numerical statics in $\delta$ (p. 22) | None |

### Read in full, bearing on C2

| Paper | Version, sha256 | What it has | Overlap with C2 |
|---|---|---|---|
| Bayraktar, Chevalier, Ly Vath and Wang (2026), arXiv:2603.14557, already recorded in `lit/bayraktar_2026` | v2, `e080555878991988`, identical to the pinned copy | Besides the two-branch cap of eq. (4), a direct ceiling on the risky share. "Because π > 1 represents a leveraged position, we treat the π ≤ 1 case as the main banking interpretation and the unrestricted case as a mathematical benchmark" (p. 19). Under it, "the direct ceiling π ≤ 1 is tighter than the LCR constraint, so π a (y) is independent of a2 and a3" (p. 20), and "the LCR constraint is nonbinding within the parameter range considered" (p. 20). When $\pi_{\max}=1$ "the LCR is everywhere weaker than the direct leverage ceiling and is therefore redundant" (p. 17) | Not an overlap but a scope finding (section 3) |
| Paulot and Lacroze, arXiv:0901.1218, "Efficient pricing of CPPI using Markov operators" | v1, `cf05a252144a057c`, 40 pp | "The process C − H is supposed to be log-normally distributed with volatility wmax σ where wmax is the maximum exposure, equal to the multiplier if no specific maximum exposure is set" (p. 16), an assumption to bound a grid | Partial. Constant log-cushion volatility, which C2 does not claim |
| Paulot and Lacroze, arXiv:0905.2926, "One-dimensional pricing of CPPI" | v2, `484f77447ff0079c`, 19 pp | Uncapped CPPI, "a shifted lognormal distribution with volatility mσ" (p. 10). A capped rule appears at p. 16 with no volatility result | Partial, as above |
| arXiv:1902.06505, "Options on CPPI with guaranteed minimum equity exposure" | v1, `b5c6cb752990c739`, 19 pp | Exposure $\max\{\min\{L_{\max}; M C_t/V_t\};\alpha_{\min}\}$ (p. 6), no volatility result. Cites Black and Perold only in reading lists (pp. 1-2) | Partial, the rule only |
| arXiv:1005.1360, "Optimal dividend and investing control of a insurance company with higher solvency constraints" | v2, `fcd2349854080da2`, 31 pp | Two-branch reinsurance retention $a^*(x)=\lambda x$ below $x_0$ and 1 above (p. 7), constant log-reserve volatility near 0 in a proof (p. 14). The slope comes from the optimisation | Partial. No liability shock, no large-wealth limit |
| Azcue and Muler (2010), arXiv:1010.4988, Ann. Appl. Probab. 20(4) | v1, `2981a454b8bd0d2e`, 51 pp | No-borrowing cap $\gamma\in[0,1]$ (p. 4). By Prop. 6.3(b) "γ∗ = 1 for small surpluses. This means that the whole surplus should be invested in stocks" (p. 29). No volatility statement | None on the result. A proof that a cap binds near ruin, the hypothesis K1 takes from the numerical solve |
| arXiv:1805.09068 v4, "Non-concave optimal investment with Value-at-Risk constraint" | `cafc9c329fc08fdf`, 41 pp | Terminal-wealth constraints only; "increasing the loss aversion η leads the insurer to more prudent investment strategies in case of bad performance" (p. 20) | None. Loss aversion moves the exposure where no cap binds |
| arXiv:1905.12431, "An assets-liabilities dynamical model of banking system" | v1, `a22cb2d88fedfd59`, 40 pp | A liability shock, uncorrelated with assets (p. 7), no exposure rule | None |

## 2b. Pass 2, full text from the author's Drive

Two of the cached bank models, read in full through the Drive connector's
extracted text (every quotation below checked against it).

| Paper | Version | What it has | Overlap |
|---|---|---|---|
| Klimenko, Pfeil, Rochet and De Nicolò (2016), "Aggregate bank capital and credit dynamics", SFI RP 16-42, Drive `14E9yQNQN_kgT1us3mdi7G2vuUGDZpu5N` | "This version: June, 2016", 54 PDF pages | State is aggregate book equity, with a loan-default shock only, since "the total volume of deposits is constant" (PDF p. 16). Proportional cost only, "issuing new equity entails a deadweight cost proportional to the size of the issuance" (p. 4, n. 1), reflecting barriers at zero and $E_{\max}$. "All agents are risk-neutral and have the same discount rate" (p. 8). A one-branch cap $e_t\ge\Lambda k_t$, "binding in poorly-capitalized states and is slack in well-capitalized states" (p. 5). "Corollary 1 Dividend threshold Emax is increasing with the magnitude of financial friction γ" (p. 19). The recapitalisation boundary "is increasing in the minimum capital ratio Λ" (p. 24). Where the cap binds, $d\log E$ has the constant volatility $\sigma_p/\Lambda$ (our reading of their p. 24 equation); "in the unconstrained region (i.e., in the well capitalized states), endogenous volatility is almost as high as in the unregulated set-up" (p. 6) | C2 partial, one branch only, binding near distress, constant log volatility there, no liability shock, no second limit, no preference parameter. C4 partial, barrier statics in a proportional cost and in the capital ratio, no fixed cost, no penalty parameter, no identification |
| Bertsch and Mariathasan (2021), "Optimal bank leverage and recapitalization in crowded markets", BIS WP 923, Drive `1R0I5FdE7qjQJq3qPkWjbP1qKTia_6fhK` | January 2021, 65 PDF pages | Three dates, binary returns, "All agents are risk-neutral" (p. 7). Recapitalisation cost an endogenous premium rising with the system-wide need. No barriers, no diffusion, no capital volatility result | None |

## 2c. Pass 3, Black and Perold

Black and Perold (1992), "Theory of constant proportion portfolio insurance",
*Journal of Economic Dynamics and Control* 16(3-4):403-426, uploaded by the
author to the Drive on 23 October 2026 (`12Az94OgOB92_mfmZCEkrh15gP2swn718`,
"28665427.pdf", PII 0165-1889(92)90043-E). Read in full, pp. 403-426 and the
appendix, from the Drive connector's extracted text. The extraction garbles
some formulas and Greek letters (ρ appears as "p", λ as "A" or "h"); a
quotation that depends on such a symbol says so.

- The rule is our cap's geometry. "E(t) =min{m.C(t),b\* W(t)). (2.1)"
  (p. 406), with "the cushion is the difference between wealth and a
  specified floor" (p. 404) and b the maximum leverage ratio. Our solvency
  branch is the multiplier branch with $m=1/a_1$ and the floor at the
  liabilities; our liquidity branch is the borrowing limit with $b=1/a_3$,
  shifted by $a_2$.
- The constant log-cushion volatility away from the limit is proved.
  Proposition 2 (p. 410), with "d In(C) = m d In(S) - i(rn2 - m)a2 dt"
  (appendix A.1, p. 422; read $d\ln C=m\,d\ln S-\tfrac12(m^2-m)\sigma^2dt$).
  At the limit "the cushion grows only linearly in S" (p. 411), from which
  the log-cushion volatility tends to $\sigma_S$ for large wealth (our
  derivation, not stated). No limit, and no ratio of the two, is stated.
- The safe asset may carry liability risk: "'safe' may mean that it closely
  tracks a liability stream" (p. 404), and the numeraire is shares of it, so
  that liability risk enters through the index ratio $S=A/R$ (pp. 407-408).
  Floor and safe asset then move together, so the liability is hedged. In our
  model the riskless asset does not track the liabilities, which is why
  $\nu_1^2=\sigma^2/a_1^2-2c\sigma\sigma_L/a_1+\sigma_L^2$ rather than
  $m^2\sigma_S^2$ (our reading of the two structures).
- The slopes come from preferences. Proposition 11 (pp. 420-421) makes CPPI
  optimal for a piecewise-HARA utility with a consumption floor; the maximum
  leverage ratio is set by relative risk aversion ("The maxi- mum leverage
  ratio b will exceed unity for sufficiently small A, i.e., an investor with
  sufficiently large risk tolerance", p. 421, A standing for λ), and "The
  multiple depends on the utility function only through p" (p. 421, p
  standing for ρ, the impatience to consume).

Overlap with C2: close in geometry, a contrast in substance. The two-branch
rule and the one-branch constant log volatility are in print since 1992. In
Black and Perold the two slopes, and so the two volatility levels and their
ratio, are pinned by preferences. C2's case for novelty must therefore rest on
the slopes being regulatory, so that the asymmetry parameter is silent, and on
the liability shock being unhedged, which changes the limits.

## 2d. Pass 4, Hugonnier and Morellec

Hugonnier and Morellec, "Bank capital, liquid reserves, and insolvency risk",
*Journal of Financial Economics* 125(2):266-285 (2017), read as the accepted
manuscript ("Forthcoming: Journal of Financial Economics", "This version:
August 31, 2016"), uploaded by the author to the Drive on 23 October 2026
(`1cYFvmyZjH5RvxZK2vIachQBfYxTh5RWq`, "Banking.pdf"). Read in full, the main
text pp. 1-40 (printed numbers) and the figure and table captions. The proofs
are in "a separate Appendix" (p. 7) that the file does not contain.

- No preference or penalty parameter. "all agents are risk neutral and
  discount cash flows at a constant rate ρ > 0" (p. 7).
- No exposure choice. "Because the bank's risky assets are fixed, the paper
  does not explore how regulatory policies may affect the loan portfolio
  choice of banks" (p. 7, n. 3).
- A fixed issuance cost. "the bank has to pay a lump-sum cost φ upon raising
  outside funds" (p. 10).
- The recapitalisation point is not chosen in the interior. "because of the
  fixed costs of financing, the bank only considers raising equity when cash
  flow shocks deplete its liquid reserves, i.e. when s ≤ 0" (p. 12), and the
  bank refinances "back to b∗ 0", the dividend barrier, or defaults
  (Proposition 3, p. 22). A liquidity requirement shifts the refinancing point
  to T (Proposition 4, p. 25).
- Barrier statics in the fixed cost, proof in the missing appendix. "we show
  in the Appendix that the target level of liquid reserves b∗ 0 increases
  with refinancing costs φ and decreases with the liquidation value Λ of
  assets" (p. 23).

Overlap with C4: partial, on the part C4 does not claim (the barrier rising in
the fixed cost). With the trigger pinned at zero reserves or at the
regulatory level, the paper has no trigger to move, no asymmetry parameter and
no identification. Overlap with C2: none, since exposure is fixed.

## 3. What pass 1 changes

**C2.** No paper read states the two limits, their ratio, or their
independence from a preference parameter under a two-branch cap with a
liability shock. The constant log-cushion volatility on a capped branch is in
the CPPI pricing literature, as C2 already concedes. The finding that matters
comes from Bayraktar et al. In their units $\pi$ is the risky share of assets,
and our cap is $u(x)=x\,\pi(x)$. The liquidity branch exceeds the no-borrowing
ceiling $u\le x$ exactly when $x\ge a_2/(1-a_3)$, so for every $a_2\le 1-a_3$
it never binds on $x\ge1$ once borrowing is ruled out (our derivation, which
their p. 20 computes for their grid). In the case the authors take as "the
main banking interpretation", C2's surplus limit $\nu_3^2$ therefore never
applies. Deep in surplus the ceiling binds, and the same algebra as
`CapGeometry.surplus_limit` gives the limit
$\sigma^2-2c\sigma\sigma_L+\sigma_L^2$, again free of $\lambda_S$ but free of
$a_3$ too (our derivation, not yet a Lean theorem). The parent's E1 agrees.
Its capped risky share of assets, 2.38, is above one. So C2 as stated is a
result about the leveraged benchmark. The silence of $\lambda_S$ survives under
no borrowing, while the role of the liquidity branch does not. L1 should
record the ceiling and the authors' reading.

**C4.** No paper read states identification of a preference or penalty
parameter jointly with a fixed cost from the two thresholds, or proves the
signs C4 proves. Two bank papers tabulate the mechanism numerically, Peura and
Keppo (section 1) and Dai, Huang and Keppo (Table 2.1), each for parameters
other than a shareholder asymmetry. Angelis pins loss aversion from a band
with no fixed cost. What C4 can still claim is the analytic signs (M4, M5),
the joint identification statement (M6), and a shareholder preference
parameter as one of the pair. The row should cite Peura and Keppo and Dai,
Huang and Keppo as the numerical precedents.

## 4. Pass 0, search level

Superseded where pass 1 read the paper. Kept as the reading list. Black and
Perold (1992, JEDC) for C2; Alvarez, Le Bihan and Lippi (2014, Banque de France
WP 492), Bonaparte and Cooper (2009, NBER w15227) and Bonaparte, Cooper and
Zhu (2011, NBER w16957) for C4; Grossman and Vila (1992, JFQA), Hilscher,
Raviv and Wiener (2024, FRL), Décamps, Mariotti, Rochet and Villeneuve (2011,
JF), Federico, Modena and Regis (2025, CRC TR224 DP 622). And the cached bank
models of `lit/CANDIDATES.md`, none reachable (Hugonnier and Morellec;
Bolton, Li, Wang and Yang; Glover et al.; Lambertini and Uysal; Klimenko et
al.; Bertsch and Mariathasan).

## 5. What must still be read

For C2. Choulli, Taksar and Zhou (2003,
SIAM J. Control Optim. 41(6):1946-1979), "A diffusion model for optimal
dividend distribution for a company with constraints on risk control", cited
by Bayraktar et al.

For C4. Alvarez, Le Bihan and Lippi's identification section. Décamps, Mariotti, Rochet and Villeneuve (2011, JF).
Bolton, Li, Wang and Yang (2025, JF).

For both, the remaining cached bank models (Bolton,
Li, Wang and Yang; Glover et al.; Lambertini and Uysal; De Nicolò et al.).
None is reachable by network or in the author's Drive on 23 October 2026,
and neither are Black and Perold, Choulli, Taksar and Zhou, Décamps et al.
or Alvarez, Le Bihan and Lippi.

## 6. Verdict after passes 1 to 4

Pass 4 reads Hugonnier and Morellec: a fixed issuance cost and the barrier
rising in it, but the recapitalisation point is fixed at zero reserves and
there is no asymmetry parameter, so C4's joint identification is not in it.


Pass 3 reads Black and Perold. C2 as now stated overreaches. Its
geometry is capped CPPI, and the one-branch volatility is proved there.
A defensible C2 says that under a regulatory two-branch cap with an
unhedged liability shock the ratio of the two limits is silent on the
asymmetry parameter, contrasted with Black and Perold, where the same
ratio is set by preferences, and scoped to the leveraged case, or
restated for the no-borrowing ceiling of Bayraktar et al.


Pass 2 adds Klimenko et al., the closest bank model to C2's deficit end so
far: a cap binding near distress pins a constant log volatility there. It
has no second branch, no liability shock and no preference parameter, so
C2's ratio is not in it.

Neither claim can be called new yet, since the papers of section 5 are unread.
No paper read in full states either result. C2 needs its scope narrowed to the
leveraged benchmark, or restated for the no-borrowing ceiling. C4 needs its
contrast with two numerical precedents in bank models. Under the guideline
both stay in the pending table.
