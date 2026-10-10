# TODO

## Literature, by purpose (rule of 10 Oct 2026)

A paper needs its primary source, a `lit/` record and Lean only when a claim rests on it (verification) or a novelty check needs it. Exploration needs neither; exploratory rows stay at abstract level, marked unverified, and `check.py` rejects any claim that rests on a row not read in full.

### Read, recorded, with Lean

- [x] Borjas (1992) — L4, `lit/borjas_1992`
- [x] Cunha and Heckman (2007), NBER WP 12840 — L1, `lit/cunha_heckman_2007`
- [x] Gibbons and Waldman, NBER WP 6454 (1998) — L2, `lit/gibbons_waldman_1998`

### Needed for verification

None today: no model, map, headline or concluding row rests on a paper. The source manuscript (`inputs/firmworkers_model.pdf`) invokes these by name where a future claim would need them:

- [ ] Friedman and Savage (1948), JPE — the S-shaped utility behind M19 and M20 (Appendix). Needed if M19/M20 become claims
- [ ] Spence (1973), QJE — "labour-queue theory" (Section 2). Macro model, deferred
- [ ] Tullock lottery with r = 1 (footnote 2) — macro shared-pool abstraction, deferred; reference not given in the source

### Needed for novelty

To be fixed once definite claims exist (after the M14, M19, M23 decisions): every paper that could already contain a claimed result. Candidates on Drive with entry_contest records to adapt: Lazear and Rosen (1981), Hopkins and Kornienko (2004).

### Exploration only (no primary source needed)

L3 Loury (1977), L5 Bénabou (1996), L6 Bowles, Loury and Sethi (2014), L7 Calvó-Armengol and Jackson (2004), L8 Rosen (1986), L9 Ghiglino and Goyal (2010), L10 Immorlica et al. (2017), L11 Luttmer (2005), L12 Altonji and Pierret (2001), L13 Fershtman, Murphy and Weiss (1996); Alós-Ferrer and Prat (2012). None is cited by the source manuscript; they were added as candidates for the pending micro extensions. A row moves to verification, and needs its PDF, when a decision makes a claim rest on it.

## Decisions recorded

- 10 Oct 2026 (Q1): the micro state is rank plus human capital, (r, h).
- 10 Oct 2026 (Q2): categorical friction enters the rank kernel, not the accumulation of h, because the friction of interest arises from non-contractual relationships in Weber's framework.
- 10 Oct 2026 (Q4): u is not observed; preferences over income lotteries take expected-utility form, so u is a representation unique up to a positive affine transformation and its curvature is meaningful (M16, `premium_sign_affine_invariant`).
- 10 Oct 2026 (Q3): the swap pool is the lottery entrants only (M14 DECIDED). M11–M13 restated over the pool; M24 proves risk-neutral entry unravels when L > 0, so a pool survives only through the convex region of u.
- 10 Oct 2026 (Q5): choice set and equilibrium left to tractability (proposal: a one-shot comparison of the three routes for a displaced worker at each rank).
- 10 Oct 2026 (Q6): global rank on the ladder, with the source's three channels.
- 10 Oct 2026 (Q9): headlines and conclusions revisited once Q1–Q8 are settled.
- Open: Q7 (meaning of stakes), Q8 (repairs), law of motion for h, how friction enters the kernel (proposal: cross-group draws inside the entrant pool weighted by a closure parameter).
- 10 Oct 2026: primary sources (with record and Lean) are required for papers needed for verification or novelty; exploration does not need them.
- 10 Oct 2026: proofs stay Lean-only for now. Informal proofs under the skeleton's 30% prose limit (MS-6) are deferred, not dropped.
- 10 Oct 2026: no paper prose for now. Work is on claims; prose will be written from the claims later.
- 10 Oct 2026: the working papers of Cunha–Heckman (NBER WP 12840) and Gibbons–Waldman (NBER WP 6454) are cited as the versions read; the published AER and QJE versions are not needed for now.
- 10 Oct 2026: headlines rest on the measurement map (X rows) as the empirical key, not on model rows alone. Deliberate departure from skeleton MS-2.

## Measurement map

- [ ] Confirm or revise the PROPOSED observation entries (X1–X21, except X12), added 10 Oct 2026
- [ ] Add rows for the reference income r, displacement, the downgrade, the rank kernel and a mobility measure as the decisions below are made

## Model decisions pending (micro)

Each option is weighed by whether its objects are observable, since headlines rest on the measurement map.


- [ ] State space: rank plus human capital stock $(r, h)$
- [ ] Law of motion for $h$: does categorical friction enter $f$ (Borjas / Bowles–Loury–Sethi route), the rank kernel only, or both
- [ ] Rank kernel: global rank or reference-set rank
- [ ] Rank kernel: channels (own effort, propagation through contacts, observer learning)
- [ ] Rank kernel: where categorical friction enters (homophily, cross-group pass-through, observer weights)
- [ ] M14: is the swap partner drawn from lottery participants or from all workers
- [ ] M19 and M20: functional form of $u$ and the reference income $r$

## Lean

- [x] M15 for general distributions (Moments.lean, 10 Oct 2026)
- [x] Lean evidence for ILL_POSED rows M1, M6, M17
- [x] SymPy replaced by Lean throughout the micro ledger (M1, M4, M5, M6, M17)

## References

- [ ] Lean implementation for every cited paper (rule of 10 Oct 2026, empirical papers included). Done: Borjas (1992), Cunha and Heckman (2007), Gibbons and Waldman (1999), in `lean/Lit/`
- [ ] Confirm `math_content` for Loury (1977), Altonji and Pierret (2001)

## Deferred to macro sync

- [ ] M18: the risk-premium coefficient scales with $m^2$, so $p_L, p_H$ vary with $\mu$; the macro model treats them as constants

## Differences from displacement_upskilling_v4.tex (sanity check, 10 Oct 2026)

- Ladder: v4 uses $q=i/(N+1)$, the same repair as M2; M2–M4 apply to v4
- Lottery: v4 is a two-agent pool won with probability 1/2 and no entry cost; the micro model is a uniform swap among $N$ workers with cost $L$
- Human capital: v4 has $f(\kappa)=\kappa^\gamma$, depreciation and a subsistence floor; the micro model has an upward-move cost only
- Utility: v4 gives each group its own quadratic utility around subsistence ($\theta_L>0>\theta_H$); the micro appendix assumes identical S-shaped preferences
- v4 inconsistencies found: $\gamma\in(0,1)$ vs $\gamma=2$; $\lambda^*=\kappa^{\min}/M$ is the floor, not the optimum (argmax $\lambda=1$ at the test point); $\mu$ integral needs $\alpha>1$; $W\ge1$ is a barrier choice; maintenance cost treated as linear despite $\xi>1$
- v4 $\Phi(\mu)$ equals the firmworkers $\varphi(\mu)$ under v4's $\lambda^*$ (SymPy identity)
