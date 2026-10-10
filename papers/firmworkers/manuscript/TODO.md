# TODO

## Primary texts needed (to move literature claims to VERBATIM)

- [ ] Lazear and Rosen (1981), JPE 89(5)
- [ ] Spence (1973), QJE 87(3)
- [ ] Alós-Ferrer and Prat (2012), JET, or IZA DP 3285 (2008)
- [ ] Hopkins and Kornienko (2004), AER
- [ ] Friedman and Savage (1948), JPE (the manuscript's S-shaped utility)
- [ ] Bowles, Loury and Sethi (2014), JEEA 12(1) — L6
- [ ] Calvó-Armengol and Jackson (2004), AER 94(3) — L7, page range to confirm
- [ ] Cunha and Heckman (2007), AER 97(2) — L1
- [ ] Borjas (1992), QJE 107(1) — L4
- [ ] Loury (1977), book chapter — L3
- [ ] Bénabou (1996), REStud 63(2) — L5
- [ ] Immorlica, Kranton, Manea and Stoddard (2017), AEJ Micro 9(1) — L10, definition of status
- [ ] Ghiglino and Goyal (2010), JEEA 8(1) — L9
- [ ] Lower priority: Rosen (1986) L8, Gibbons and Waldman (1999) L2, Altonji and Pierret (2001) L12, Luttmer (2005) L11, Fershtman, Murphy and Weiss (1996) L13

## Model decisions pending (micro)

- [ ] State space: rank plus human capital stock $(r, h)$
- [ ] Law of motion for $h$: does categorical friction enter $f$ (Borjas / Bowles–Loury–Sethi route), the rank kernel only, or both
- [ ] Rank kernel: global rank or reference-set rank
- [ ] Rank kernel: channels (own effort, propagation through contacts, observer learning)
- [ ] Rank kernel: where categorical friction enters (homophily, cross-group pass-through, observer weights)
- [ ] M14: is the swap partner drawn from lottery participants or from all workers
- [ ] M19 and M20: functional form of $u$ and the reference income $r$

## Lean

- [ ] M15 for general distributions (currently finite support only)

## References

- [ ] Lean implementation of mathematical content in cited papers (all `NOT_STARTED`)
- [ ] Confirm `math_content` for Loury (1977), Borjas (1992), Altonji and Pierret (2001)

## Deferred to macro sync

- [ ] M18: the risk-premium coefficient scales with $m^2$, so $p_L, p_H$ vary with $\mu$; the macro model treats them as constants

## Differences from displacement_upskilling_v4.tex (sanity check, 10 Oct 2026)

- Ladder: v4 uses $q=i/(N+1)$, the same repair as M2; M2–M4 apply to v4
- Lottery: v4 is a two-agent pool won with probability 1/2 and no entry cost; the micro model is a uniform swap among $N$ workers with cost $L$
- Human capital: v4 has $f(\kappa)=\kappa^\gamma$, depreciation and a subsistence floor; the micro model has an upward-move cost only
- Utility: v4 gives each group its own quadratic utility around subsistence ($\theta_L>0>\theta_H$); the micro appendix assumes identical S-shaped preferences
- v4 inconsistencies found: $\gamma\in(0,1)$ vs $\gamma=2$; $\lambda^*=\kappa^{\min}/M$ is the floor, not the optimum (argmax $\lambda=1$ at the test point); $\mu$ integral needs $\alpha>1$; $W\ge1$ is a barrier choice; maintenance cost treated as linear despite $\xi>1$
- v4 $\Phi(\mu)$ equals the firmworkers $\varphi(\mu)$ under v4's $\lambda^*$ (SymPy identity)
