# Literature search, the shortfall penalty and the fixed issuance cost, 25 October 2026

A planning note, not part of the paper. The author asked how the literature
treats the asymmetry parameter of a shortfall penalty and the fixed issuance
cost, to motivate the pairing better. A search (web, by a background agent)
returned the candidates below. **Nothing here is verified.** Apart from four
arXiv papers read in part, every entry rests on an abstract or a search
summary, and bibliographic details are as the search pages showed them.
Under the bundle's rules no candidate enters the manuscript until it is read
in full, with a `lit/` record and a Lean companion.

## Access

Only arxiv.org is reachable from the cloud environment. ecb.europa.eu,
norges-bank.no, papers.ssrn.com, carloalberto.org and opus.lib.uts.edu.au are
refused by the network policy. The author can upload the PDFs to the Drive
folder used before, or allow those hosts.

## Closest to the pair, to read first

| Paper | Why | Full text |
|---|---|---|
| Milne (2004), "The Inventory Perspective on Bank Capital", Cass Business School, SSRN 576062 | Regulatory minimum with random audit and fixed costs of issuing equity, by the author of Milne and Whalley (2001) | SSRN, blocked here |
| Lang and Menno (2026), "A structural model of capital buffer usability", ECB WP 3188 | A buffer requirement that can be breached at a stigma cost, with an occasionally binding equity-issuance constraint | ECB, blocked here |
| Behn, Daminato and Salleo (2019), "A Dynamic Model of Bank Behaviour under Multiple Regulatory Constraints", ECB WP 2233 | Buffers from the possibility of breaching requirements or having to raise fresh equity | ECB, blocked here |
| Constantinides and Richard (1978), "Existence of Optimal Simple Policies for Discounted-Cost Inventory and Cash Management in Continuous Time", Operations Research 26(4) | Linear holding and penalty costs with fixed and proportional transfer costs, the pair in cash management | paywalled |
| Vo (2009), "Optimality of Prompt Corrective Action in a Continuous-Time Model with Recapitalization Possibility", Norges Bank WP 2009/28 | Graduated intervention from an optimal contract, with recapitalisation | Norges Bank, blocked here |

## Others found

- Graduated shortfall cost. Kisin and Manela (2016, RFS), the shadow cost of
  capital requirements; Furfine (2001, JFSR), costs of proximity to the
  minimum; Benes and Kumhof (2011, IMF WP 11/130), penalties below the
  requirement; Shim (2011, JMCB), stochastic termination; Egami and Oryu
  (2013, arXiv 1311.3019), a regulator's running cost under prompt
  corrective action (downloaded); Federico, Modena and Regis (2025),
  dividend restrictions and costly equity; Subramanian and Yang (2020,
  Management Science); Albrecher, Gerber and Shiu (2011) and Mata (2025,
  arXiv 2508.21133), Omega models with a level-dependent bankruptcy rate.
- Fixed issuance cost. Paulsen (2008, SIAM J. Control Optim.); Conlon and
  Min (2006, arXiv math/0602397), the Peura and Keppo model; Keppo, Reppen
  and Soner (2018, arXiv 1805.05077); Hennessy and Whited (2007, JF),
  structural estimates of flotation costs; Bolton, Wang and Yang (2020,
  NBER WP 26802).
- Empirical, both pressures. Dinger and Vallascas (2016, JFQA), regulatory
  against market pressure on bank equity issues.
- Pending from the novelty check, link found. Glover, Johnson, Evatt and
  Cheng (2023, European Journal of Finance), open on the UTS repository.

## What the search suggests, from abstracts

The two frictions are each common. Graduated or stigma costs of a shortfall
appear in structural and DSGE bank models and in contract-theoretic
derivations of graduated corrective action; fixed issuance costs are standard
in impulse-control models. Together they appear formally in cash management
(Constantinides and Richard) and in a few bank models (Milne 2004, Lang and
Menno, Behn et al.). No abstract seen states that a shortfall-penalty
parameter and a fixed issuance cost move the recapitalisation trigger in
opposite directions, or that the two are told apart by the thresholds. The
five papers of the first table decide that.

## What the full readings show, 25 October 2026

Read in full from the author's Drive uploads, each with a record under
`lit/` (quotations verified by page, Lean companion): Constantinides and
Richard (1978), Vo (2009), Behn, Daminato and Salleo (2019), Lang and Menno
(2026), Glover, Johnson, Evatt and Cheng (2023, the December 2022 preprint),
Décamps, Mariotti, Rochet and Villeneuve (2011), Lambertini and Uysal (2013,
2014). Milne (2004) and Alvarez, Le Bihan and Lippi (2014, NBER WP 20155) were read
later the same day; see below. Still to read: Choulli, Taksar and Zhou (2003).

- The cost of falling below takes five forms in the bank papers:
  liquidation (Peura and Keppo), random audit (Milne and Whalley), a forced
  issue at the proportional issuance cost (Glover et al.), a charge that is the
  same at every shortfall (Behn et al., Lang and Menno), and a smooth log
  penalty that rewards a surplus (Lambertini and Uysal). None is a running
  cost charged only below the reference that grows with the shortfall. Vo's
  graduated intervention comes from an optimal contract, not from a cost.
- A fixed issuance cost is in Milne and Whalley, Peura and Keppo and Décamps
  et al. Behn et al., Glover et al., Vo and Lambertini and Uysal have none; Lang
  and Menno have issuance free or ruled out.
- The pairing of a linear running penalty below a level with a fixed and a
  proportional transfer cost is Constantinides and Richard's cash-management
  model. In their no-drift, vanishing-discount approximation the upper
  trigger rises in both the penalty and the fixed cost of raising cash; no
  direction is stated for the lower trigger. From their printed solution the
  lower trigger falls in the fixed cost (my algebra, CR-D4), and given the
  holding cost the four thresholds pin the penalty and the fixed cost
  (`Constantinides.thresholds_pin_pK`, CR-D6). They never state
  this. It is the nearest precedent to K1 and is cited in C4 (L52).
- Their printed equation for the upper gap, (h/2σ²)(u−U)³ = K⁻, disagrees by
  a factor of 3 with their printed solution; the area computation
  (`Constantinides.gap_area`) supports the solution.
- No paper read asks whether the policy separates the two frictions. Behn et
  al. separate a breach charge and an issuance cost by assigning moments.

## Milne (2004) and Alvarez, Le Bihan and Lippi (2014), 25 October 2026

- Milne (2004), SSRN 576062. Under continuous monitoring the minimum is a
  hard barrier, recapitalise at a fixed cost or be liquidated; under random
  audit the only deadweight loss on audit is the fixed cost, the top-up being
  paid at par, so no part of the cost grows with the shortfall
  (`Milne2004.audit_loss_is_fixed`). No threshold is shown to move in the audit
  intensity alone, and no two frictions move the trigger in opposite
  directions. By its own account the random-audit part summarises Milne and
  Whalley, so the theory paper does not cite it.
- Alvarez, Le Bihan and Lippi, NBER WP 20155 (May 2014), not the Banque de
  France WP 492 the notes named. Identification is global and analytic, from
  moments of the distribution of adjustments (frequency, variance,
  kurtosis) and outside cost data, never from the threshold. Two parameters
  that both raise kurtosis are separated only by adding the observed cost of
  adjustment; no rank or opposite-sign condition is stated. Cited in C4 (L58).

## Four more readings, 28 October 2026

- Federico, Modena and Regis (SAFE WP 487, July 2026). Dividend barrier and
  impulse issue at a fixed cost κ to an endogenous target. The capital
  requirement is a hard floor, liquidate or issue; the issue trigger is the
  floor. The dividend restriction is a payout tax that depends on the macro
  state, a ban in the limit. All threshold responses are numerical at one
  calibration; the baseline κ is never varied. Nothing estimated or
  identified. Cited in Section 3.1 (L59, L60).
- Bonaparte, Cooper and Zhu (NBER 16957). Preference parameters and a fixed
  income-scaled adjustment cost estimated together by simulated method of
  moments; an inaction band shown qualitatively; the only identification
  device is a table of elasticities of moments. Cited in C4 (L61).
- Bonaparte and Cooper (NBER 15227, August 2009 version). Weaker precedent,
  two moments and two parameters, just identified, no thresholds. Record only.
- Egami and Oryu (8 November 2014 version). The regulator's problem with a
  passive bank, a running cost per unit time of intervention, no fixed cost,
  trigger found numerically, never varied in a cost. Record only.
