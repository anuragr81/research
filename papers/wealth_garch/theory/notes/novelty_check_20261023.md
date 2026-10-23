# Novelty check of C2 and C4, 23 October 2026

A planning note, not part of the paper. The parent `MANUSCRIPT.tex` claims two
results new, C2 and C4. Both are candidates for the theory paper, whose
skeleton has no rows yet. This note records an internal audit and a search
run on 23 October 2026, extending the searches of 10 October 2026 in the
parent `TODO.md` ("C2 search", "C4 search").

## Limits of the search

No paper text was read. The paper hosts are denied by this environment's
network policy (arxiv.org, nber.org, ssrn.com, bis.org, epfl.ch, tse-fr.eu,
eprints.gla.ac.uk, academic.oup.com, ideas.repec.org, core.ac.uk, econstor,
hal, web.archive.org among them), and WebFetch failed to resolve any host.
Three search agents ran about 70 WebSearch queries between them. Every lead
below is therefore [A] or [C] in the sense of `lit/README.md`, and every
phrase attributed to a paper is a search-tool snippet that may be a
paraphrase. Under the rules of `papers/MANUSCRIPT_SKELETON.md`, such a lead
can neither support nor defeat a positioning claim. It can only say what must
be read.

## 1. Internal audit

1. **Placement.** The parent puts C2 and C4 in its table of conclusions and
   leaves the pending table empty. Section 4.4 of the skeleton guideline keeps
   a claim that a result is new in the pending table "until every paper that
   could contain the result has been read in full". Both rows say their leads
   were "excluded at the level of their abstracts", and the cached bank
   models of `lit/CANDIDATES.md` (Hugonnier and Morellec, Bolton, Li, Wang and
   Yang, Klimenko et al., Glover et al., Lambertini and Uysal, De Nicolò et
   al.) are unread. By the guideline, C2 and C4 are pending.
2. **C4's mechanism in Peura and Keppo's Table 2.** Peura and Keppo is read in
   full (`lit/peura_keppo_2006`), and our replication reproduces its Table 2
   (`replication/peura_keppo_2006/results_stage1.md`). Their printed values,
   issue barrier $u_1$ and dividend barrier $u_2$ in percent:

   | Delay $\Delta$ | $K=0.10$ | $K=0.25$ | $K=0.50$ | $K=1.00$ |
   |---|---|---|---|---|
   | 0.08 | 0.9 / 2.4 | 0.8 / 2.7 | 0.7 / 2.9 | 0.6 / 3.1 |
   | 0.5 | 1.6 / 3.2 | 1.3 / 3.4 | 1.1 / 3.5 | 0.9 / 3.7 |
   | 1.0 | 1.7 / 3.5 | 1.4 / 3.7 | 1.1 / 3.7 | 0.6 / 3.8 |

   The fixed cost $K$ lowers the issue barrier and raises the dividend barrier
   in every row. The delay $\Delta$ raises the dividend barrier in every
   column, and raises the issue barrier at $K=0.10$ and $K=0.25$. At those
   points the two parameters move the trigger in opposite directions and the
   barrier in the same direction, the configuration that makes the Jacobian
   of M6 non-singular. So C4's mechanism is visible, as numbers, in a paper
   read in full, with the issue delay in place of the asymmetry parameter.
   Peura and Keppo state no identification and give no comparative statics in
   $\Delta$ as a result. What C4 can still claim is the preference parameter
   and the analytic proof, and the row should cite this contrast.

## 2. C2, leads ranked by threat

| Lead | Tag | What could overlap | What to read |
|---|---|---|---|
| Black and Perold (1992), "Theory of constant proportion portfolio insurance", JEDC 16(3):403-426 | [C] | Capped CPPI sets exposure to $\min\{m\,C,\ l\,W\}$, cushion $C=W-F$. Our derivation, not their text: the log cushion has volatility $m\sigma$ near the floor and tends to $l\sigma$ for large wealth, so the ratio $m^2/l^2$ is set by the cap alone. That is C2's structure without the liability shock | Their section on CPPI with a borrowing limit, whether they evaluate the two volatilities, and whether $m$ comes from a preference |
| Azcue and Muler (2010), "Optimal investment policy and dividend payment strategy in an insurance company", Ann. Appl. Probab. 20(4), doi 10.1214/09-AAP643, arXiv:1010.4988 | [C] | With a no-borrowing cap the whole surplus is in stocks for small surplus, so the volatility of log surplus near ruin is pinned by the cap, a one-branch analogue of the deficit limit | Section 6, Prop. 6.3(b) |
| Grossman and Vila (1992), "Optimal dynamic trading with leverage constraints", JFQA 27(2):151-168 | [C] | Once the leverage cap binds, the cap rather than risk aversion sets the position | Whether wealth volatility in the binding region is stated |
| Hilscher, Raviv and Wiener (2024), "Dynamic volatility regulation of financial institutions", Finance Research Letters 61:104968 | [C] | Asset volatility imposed as a function of the capital cushion | The volatility rule at small and large cushions |
| Décamps, Mariotti, Rochet and Villeneuve (2011), JF 66(5):1501-1544 | [C] | Return volatility as a function of the cash state, read as asymmetric volatility. No cap, risk-neutral firm | The volatility section |
| Bayraktar et al. (2026), read in full | [C] for the new point | A snippet paraphrases that in their parameter range a direct cap on the investment proportion is tighter than the liquidity constraint. `lit/bayraktar_2026` records no third cap. If the liquidity branch never binds in their numerics, L1 should say so | Their numerical section, against the cached PDF |

Nothing seen states the ratio $\nu_1^2/\nu_3^2$, or its independence from a
preference parameter, under a two-branch cap with a liability shock.

## 3. C4, leads ranked by threat

| Lead | Tag | What could overlap | What to read |
|---|---|---|---|
| Angelis (2025), "Price Setting and Price Stickiness: A Behavioral Foundation of Inaction Bands", JEEA 23(3):1095-1132, doi 10.1093/jeea/jvae049. Replication package zenodo.org/records/13785746 | [C] | Loss aversion with a menu cost gives an inaction band with two targets, calibrated to price microdata. Whether it gives the thresholds' comparative statics in the two parameters, or identifies them from the band, is not known | The model and calibration sections. The accepted manuscript (eprints.gla.ac.uk/342107) was embargoed to 15 October 2026 and could not be reached. Bibliography item B3 of the parent `TODO.md` |
| Alvarez, Le Bihan and Lippi (2014), "Small and large price changes and the propagation of monetary shocks", Banque de France WP 492 (published version not checked) | [C] | A possible counterpoint. The snippet says identification of the menu cost needs data on the cost firms bear, beyond the distribution of price changes, which would cut against identification from thresholds alone | The identification section of WP 492 |
| Bonaparte and Cooper (2009), "Costly portfolio adjustment", NBER w15227, and with Zhu (2011), NBER w16957 | [C] | Joint estimation of a preference (risk aversion) and an adjustment cost from inaction, by moments rather than a threshold Jacobian | The identification section of w16957 |
| Federico, Modena and Regis (2025), "Coordinating dividend taxes and capital regulation", CRC TR224 DP 622 | [C] | Payout threshold and costly issuance in a bank model, recapitalisation moved by regulatory parameters. No preference parameter seen | Whether it has a fixed issuance cost and trigger statics |
| Alvarez, Guiso and Lippi (2012), AER 102(5):2272-2300 | [C] | Two costs calibrated from two frequencies, the same counting logic, no preference parameter | Low priority |

No lead combined loss aversion or prospect theory with dividends or capital
injection.

## 4. Unread bank models

None of the seven cached bank models of `lit/CANDIDATES.md` could be reached
(Hugonnier and Morellec, Bolton, Li, Wang and Yang, Glover et al., Lambertini
and Uysal, Klimenko et al., Décamps et al. 2011, Bertsch and Mariathasan).
Their summaries show issuance costs in most of them and no penalty or
asymmetry parameter in any, which is [A] and decides nothing. They are the
natural place for comparative statics of the payout and issuance thresholds in
a fixed cost, which C4 does not claim.

## 5. What must be read before either claim is called new

For C2.
1. Black and Perold (1992), in full. If capped CPPI already gives cap-only
   limits, C2's novelty narrows to the liability-shock terms ($\sigma_L$, $c$)
   and to the reading as silence of $\lambda_S$, as the parent `TODO.md`
   anticipated on 10 October.
2. Azcue and Muler (2010), section 6.
3. The numerical section of Bayraktar et al. (2026), already cached.

For C4.
1. Angelis (2025), in full.
2. The identification section of Alvarez, Le Bihan and Lippi.
3. Peura and Keppo's Table 2, already read, cited in the row as the contrast
   of section 1.

For both, the cached bank models of section 4, in full.

## 6. Verdict

Neither claim can be called new yet. No lead found states either result, and
the closest leads (Black and Perold for C2, Angelis for C4) are unread. Under
the guideline both move to the pending table, each naming the readings of
section 5.
