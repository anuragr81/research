# Reconstruction: Décamps, Mariotti, Rochet and Villeneuve (2011)

From the author's Google Drive, downloaded on 25 October 2026 and pinned as
`~/.cache/wealth_garch/JF_unknown.pdf`, sha256 `9313eeb2e141a186cb97ce9430d3c1be5fc20023a746c233e26935410650321c`.
The sections below are the reading notes made for the theory paper's Section 3.1.

## 1. Source

- **Citation as printed in the PDF:** Jean-Paul Décamps, Thomas Mariotti, Jean-Charles Rochet, and Stéphane Villeneuve, "Free Cash Flow, Issuance Costs, and Stock Prices," *The Journal of Finance*, Vol. LXVI, No. 5, October 2011, pp. 1501–1544. The page range is taken from the first and last printed page numbers.
- **File:** /root/.cache/wealth_garch/JF_unknown.pdf
- **PDF pages:** 44.
- **Page mapping:** printed page = PDF page + 1500 (PDF 1 = p. 1501, PDF 44 = p. 1544). The number sits at the foot of p. 1501 and in the running head of the other pages.
- **Coverage:** read in full, PDF pages 1 to 44, including Appendices A and B and the references. The Internet Appendix, which holds the proofs of Lemmas 1, A.1–A.3 and Proposition A.1, is not in the PDF and was not read.
- **Text-layer note:** the text layer drops primes, so V*′ and V*″ appear as V*. In this file they are restored from context.

## 2. Reconstruction

**Primitives** (Section I, pp. 1507–1511).
- A firm with one fixed-size project has cumulative cash flow dR = µ dt + σ dW, with constant σ (eq. 1, p. 1507).
- The firm is all-equity and held by **risk-neutral** shareholders who discount at r (p. 1507). Debt is suboptimal because there are no taxes and no asymmetric information (pp. 1502 fn. 2, 1507).
- Liquidation value is zero (p. 1507).
- Cash reserves follow dM = (r − λ)M dt + dR + dI/p − dF − dL (eq. 4, p. 1509). Here λ ∈ (0, r] is an **agency cost of free cash flow** that lowers the return on retained cash (p. 1509). **Notation warning:** this λ is a running cost on cash held, the surplus side. It is not the theory paper's λ_S.
- The firm is liquidated when M hits 0 and no issue takes place (eq. 5, p. 1510).

**Frictions** (pp. 1508–1509).
- **Fixed cost f > 0 per issue**, for taxes and registration fees "largely independent of the size of the issue" (p. 1508).
- **Proportional cost p > 1**: each dollar of new shares yields 1/p in cash. Raising cash m costs p(m + f) in total (fn. 8, p. 1508). Equivalently, in the notation of Gomes (2001) as cited, λ₀ = pf and λ₁ = p − 1 (fn. 8, p. 1508).
- Total fixed costs F_t = Σ f·1{τ_n ≤ t} (eq. 3, p. 1509).
- Dividends are costless (p. 1509).
- **There is no capital requirement or regulator.** The only threshold is M = 0.

**Control problem.** Value (eq. 6, p. 1510) is the expected discounted dividends net of gross issuance proceeds. The optimal value is V*(m) (eq. 7, p. 1510). The problem mixes an **impulse** component (lumpy issues) with a **singular** component (dividends) (p. 1510). It nests Jeanblanc-Picqué and Shiryaev (1995) and Radner and Shepp (1996) when issuance is prohibitive, and Løkka and Zervos (2008) when f = 0 (pp. 1510–1511).

**First best** (f = 0, p = 1; Section II, pp. 1511–1513). Value is m + µ/r (eq. 8). Cash is held at zero. Return volatility is constant at σr/µ (eqs. 12, 15; Proposition 1, pp. 1512–1513).

**Solution structure** (Section III; Proposition 2, p. 1516). Value solves the variational system (eqs. 24–27, p. 1515):
- −rV + [(r − λ)m + µ]V′ + (σ²/2)V″ = 0 on (0, m₁*);
- V linear with slope 1 above m₁* (smooth pasting, eq. 22, p. 1515; super contact V″(m₁*) = 0, eq. 23, p. 1515);
- at zero, V(0) = max over m of {V(m) − p(m + f)}⁺ (eq. 25, p. 1515).

Proposition 2 (p. 1516):
- V* is increasing and concave.
- V*′ > 1 below m₁* and V*′ = 1 above.
- If issuance costs are high (condition 31), the firm never issues and is liquidated at zero cash.
- If they are low (condition 32), the firm issues i* = p(m₀* + f) whenever cash hits zero and jumps to the target m₀*, defined by V*′(m₀*) = p (p. 1515). It is never liquidated.
- Cash is reflected at the dividend barrier m₁*.

The issuance **trigger is always zero cash**: issues "should be delayed as much as possible" (p. 1514). The optimal policy and the Skorokhod representation are eqs. 33–36 (p. 1517). Proofs are in Appendix A (pp. 1534–1538), including the verification theorem (Proposition A.2, p. 1538).

**Limiting cases** (pp. 1518–1519):
- λ = 0 gives m₁* = ∞, and no optimal dividend policy exists.
- p = 1 (fixed cost only) with f small gives m₀* = m₁*: the firm raises up to the barrier.
- f → 0 gives infinitesimal, clustered issues (singular control) with V′(0) = p.
- Lumpy, infrequent issues therefore need f > 0.

**Comparative statics.**
- Corollary 1 (p. 1520; proof Appendix B, pp. 1538–1539): the dividend barrier m₁* and the elasticity of V in m are **increasing in f and in p**. V* falls and V*′ rises with f and p.
- The issue target m₀* is unambiguously increasing in f, provided condition (32) still holds. In p the effect is ambiguous (price effect against income effect), but m₀* is decreasing in p when p is close to 1 (p. 1521, fn. 21) and numerically decreasing and nearly linear (Fig. 7, pp. 1526–1527).
- Corollary 2 (p. 1521; proof pp. 1539–1541): m₁* is increasing in σ and decreasing in µ.
- Numerically, m₁* is decreasing in λ (Fig. 3, p. 1524; hard to prove analytically, fn. 24, pp. 1523–1524). m₀* is increasing in σ and decreasing in µ (Figs. 4–5, pp. 1524–1525).

**Numerical baseline** (p. 1522). r = 2%, λ = 1%, µ = 0.18, σ = 0.09, f = 0.078 and p = 1.028, the last two following Gomes (2001). Units are $ million (fn. 22). The implied cash ratio of 7.9% is compared with Bates, Kahle and Stulz (pp. 1522–1523). This is a plausibility check, not an estimation.

**Stock prices** (Section IV, pp. 1526–1533).
- Market capitalisation is N*S* = V*(M*) (eq. 38, p. 1527). The price is the present value of dividends per share because shareholders are risk-neutral (eq. 39, p. 1527).
- The price does not jump at issues (Lemma 1, p. 1528). Dilution per issue is constant (p. 1528).
- Proposition 3 (pp. 1528–1529): between issues the return volatility is σ*(v) = σ·V*′(V*⁻¹(v))/v (eqs. 42–43).
- Corollary 3 (p. 1529): return volatility is **decreasing in the stock price**. This is the asymmetric-volatility result.
- Corollary 4 (p. 1530): dollar price volatility is also decreasing in the price.
- Market-cap volatility is at least σ, with equality only at the dividend barrier (p. 1530).
- Corollary 5 (p. 1531): return volatility is **increasing in issuance costs**. This underlies the countercyclical-volatility interpretation (p. 1531).
- Agency cost and volatility (Figs. 8–10, pp. 1532–1534): a higher λ raises volatility at given cash and lowers it at given market capitalisation.

## 3. Answers to (a), (b), (c)

**(a) Cost of being below a requirement or threshold.**
- There is no capital requirement. The only threshold is zero cash. At zero cash the firm either issues (low costs) or is **liquidated** at value zero (high costs) (pp. 1510, 1516, 1518).
- There is no graduated or running cost of being near or below any level. Cash is never negative (p. 1507).
- The running friction is on the **surplus** side: the agency cost λ lowers the return on cash held, so it is proportional to cash and charged on holding, not on shortfall (p. 1509).
- Shareholders bear all costs.

**(b) Costs of issuing equity.**
- Both kinds: a **fixed cost f per issue** and a **proportional cost p − 1 per dollar**, with total cost p(m + f) for net cash m (pp. 1508–1509, fn. 8).
- Issues are **lumpy** impulses of size i* = p(m₀* + f), made only when cash hits zero (pp. 1515–1516).
- The fixed cost is what makes issues lumpy and infrequent. With f → 0 issuance becomes singular (pp. 1503, 1508, 1518–1519).
- The authors cite estimates of the two costs (Gomes; Altınkılıç and Hansen; Hennessy and Whited) and justify not using a quadratic term (p. 1508, fn. 7).

**(c) Both frictions; interaction; comparative statics; identification.**
- The paper pairs fixed and proportional **issuance** costs with an agency cost of cash. It does **not** pair a shortfall penalty with a fixed issuance cost.
- The stated key interaction is agency cost against issuance cost. With either one absent the problem is trivial (p. 1509), and with zero issuance costs return volatility is constant (p. 1504).
- Comparative statics of the thresholds:
  - dividend barrier m₁*: up in f and p (Corollary 1, p. 1520), up in σ, down in µ (Corollary 2, p. 1521), down in λ (numerical, Fig. 3, p. 1524);
  - issue target m₀*: up in f; ambiguous in p but down for p near 1 and numerically down (p. 1521, fn. 21; Fig. 7, p. 1527); up in σ and down in µ numerically (Figs. 4–5, pp. 1524–1525);
  - issuance trigger: fixed at zero, so it does not respond to any parameter (pp. 1514–1516).
- The paper does **not** ask whether f and p (or f and λ) can be told apart from the policy, and it identifies or estimates nothing. Parameters are taken from Gomes (2001) and set by assumption (p. 1522). The authors only say that the parameters "can be easily estimated", as for issuance costs and cash-flow volatility, or proxied, as for agency costs (pp. 1504–1505).

**Buffers and volatility.**
- The cash buffer m₁* is a target cash level. It rises with issuance costs and with cash-flow volatility (pp. 1503, 1520–1521).
- The paper derives the **volatility of stock returns and prices** as a function of the cash state: state-dependent and higher when cash and price are low (Proposition 3, Corollaries 3–5, pp. 1528–1531). The source is the concavity of V in cash, "neither debt issuance nor a volatility risk premium" (p. 1504). With risk-neutral investors and no leverage, neither volatility feedback nor the leverage effect applies (p. 1530).
- It does **not** derive the variance or stationary distribution of the cash reserve process itself. It has no cap on exposure (project size is fixed, p. 1507).

**Stated limitations and future research.**
- Constant cash-flow volatility, which could be extended to general diffusions (pp. 1507, 1531).
- No debt or credit lines (p. 1519).
- Random profitability, giving a two-state model of liquidity and solvency (p. 1533).
- Issuance costs that vary randomly over the business cycle (p. 1533).
- Firm size is fixed (pp. 1506, 1533; extended by Bolton, Chen and Wang as cited).

## 6. Verdict for the theory paper

1. DMRV is the canonical fixed-plus-proportional issuance-cost model with lumpy, impulse issues (pp. 1503, 1508, 1515–1516). The dividend barrier rises in both f and p (Corollary 1, p. 1520), which matches the theory paper's K raising y*.
2. It has **no capital requirement and no graduated shortfall penalty**. The only threshold is zero cash, where the firm issues or is liquidated (pp. 1510, 1516). The issuance trigger is therefore fixed at zero, and the paper has no trigger comparative statics comparable to x_L rising in λ_S and falling in K (pp. 1514–1516).
3. It does **not** ask whether any two frictions can be told apart from the thresholds, and it estimates nothing. Parameters come from Gomes (2001) and assumption (pp. 1504–1505, 1522). The differing signs of f and p on the issue target (p. 1521) are only implicit.
4. Owners are risk-neutral (pp. 1507, 1527, 1530).
5. Its asymmetric volatility is the conditional volatility of stock returns, decreasing in the price and increasing in issuance costs, from concavity of value in cash (Proposition 3, Corollaries 3 and 5, pp. 1528–1531). It is a conceptual neighbour of, not a precedent for, the theory paper's buffer-variance results: it derives no variance of the cash or buffer process, has no cap, and has no shortfall-asymmetry parameter.
