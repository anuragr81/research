# Validation of the GMM estimator used for Berger et al.

Run 14 October 2026, before any Berger et al. data were fetched. Berger et
al. estimate their partial-adjustment model by Blundell-Bond system GMM
with do-nothing capital, not a lagged dependent variable, as the
regressor. pydynpd 0.2.2 refuses a model without a lagged dependent
variable, so gmm.py implements one- and two-step system and difference
GMM for any regressors, with xtabond2's instrument layout, the first-step
weighting of pydynpd (xtabond2's h(2)) and Windmeijer-corrected two-step
standard errors.

## Findings

- pydynpd reproduces Roodman's printed difference-GMM estimate of the
  Arellano-Bond employment equation (Roodman 2008, CGD WP 103, PDF p. 29:
  0.6862261, standard error 0.1445943; p. 30: two-step 0.629).
- gmm.py reproduces the same numbers exactly, with 611 observations and 41
  instruments.
- gmm.py reproduces pydynpd's one- and two-step system GMM on the same
  data, all 24 coefficients and standard errors within 4.1e-08, and its
  observation count, instrument count and Hansen statistic.
- Roodman's printed one-step system-GMM example (p. 36, 0.9356053) uses
  xtabond2's default h(3) weighting; pydynpd's h(2) gives 0.9326197 on it.
  The two-step estimator, used for Berger et al., is not this example.

The data are Stata's `abdata` (stata-press.com/data/r13/abdata.dta).

Pinned: /home/anuragr/.cache/wealth_garch/roodman_2008_wp103.pdf sha256 396e8012389aaa14b651bbbc6bf3271fe27c3d8542e876c695e2323cfff2fb85
Pinned: /home/anuragr/.cache/wealth_garch/abdata_r13.dta sha256 e6c6a65b8a16921c021d5dc9a3b2d2dd1eb94e080f9498d054d50dc365507d15

Reproduce: python3 validate_gmm.py

```
roodman_2008_wp103.pdf sha256 396e8012389aaa14b651bbbc6bf3271fe27c3d8542e876c695e2323cfff2fb85
abdata_r13.dta sha256 e6c6a65b8a16921c021d5dc9a3b2d2dd1eb94e080f9498d054d50dc365507d15
Roodman p. 29 prints one-step difference GMM L1.n .6862261 (se .1445943): True
Roodman p. 30 prints 'the two-step one of 0.629': True
pydynpd one-step: L1.n 0.6862261 se 0.1445943, 611 obs, 41 instruments
pydynpd two-step: L1.n 0.6287089
one-step matches to 7 decimals: True
two-step matches to 3 decimals: True
gmm.py difference GMM: L1.n one-step 0.6862261 se 0.1445943, two-step 0.6287089, 611 obs, 41 instruments
gmm.py matches Roodman: True
gmm.py system GMM against pydynpd: 24 coefficients and standard errors, largest gap 4.1e-08, 751 obs, 48 instruments, Hansen 57.905
gmm.py matches pydynpd to 7 decimals: True
```
