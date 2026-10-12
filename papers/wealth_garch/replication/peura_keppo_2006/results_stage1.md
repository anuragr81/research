# Replication of Peura and Keppo (2006), stage 1: the model's printed numbers

Run 12 October 2026. Formulas (14)-(18), (A9) and (17) of the paper, solved with
scipy (fsolve from a grid of starting points; a solution is kept only if
0 < u1 < u2 < u0 and M <= f1 on [0, u2], the conditions of Prop. 3).

Reproduce: python3 stage1_barriers.py

```
u0 (17) at mu = sigma = 1%, rho = 4%: 3.80%   printed 3.80% (Fig. 2)
Fig. 2, Delta = 0.5, K = 1%: u1 0.90%, u2 3.66%, converged True   printed 0.90%, 3.66%

Table 2 (mu = sigma = 1%, rho = 4%): reproduced / printed
E[s]: conditional expectation u2 - E[X_Delta | survive]; gap: u2 - u1.
Years: expected time from u2 down to u1, with (+D) and without the delay.
Delta    K% |        u1% |        u2% | E[s]%  gap% /pr | cost(gap) | yrs+D   yrs /pr
 0.08  0.10 | 0.842/0.9  | 2.357/2.4  |  1.43  1.52 /1.5  |   6.2/6   |    8.4    8.3 /8   
 0.08  0.25 | 0.767/0.8  | 2.638/2.7  |  1.79  1.87 /1.8  |  11.8/12  |   18.8   18.7 /18  
 0.08  0.50 | 0.703/0.7  | 2.867/2.9  |  2.08  2.16 /2.2  |  18.8/19  |   35.3   35.2 /36  
 0.08  1.00 | 0.630/0.6  | 3.104/3.1  |  2.39  2.47 /2.5  |  28.8/29  |   67.6   67.5 /68  
  0.5  0.10 | 1.557/1.6  | 3.201/3.2  |  1.13  1.64 /1.6  |   5.7/6   |   11.7   11.2 /11  
  0.5  0.25 | 1.335/1.3  | 3.388/3.4  |  1.53  2.05 /2.0  |  10.9/11  |   28.3   27.8 /28  
  0.5  0.50 | 1.138/1.1  | 3.528/3.5  |  1.85  2.39 /2.4  |  17.3/17  |   57.3   56.8 /57  
  0.5  1.00 | 0.897/0.9  | 3.658/3.7  |  2.17  2.76 /2.8  |  26.6/27  |  122.2  121.7 /122 
  1.0  0.10 | 1.736/1.7  | 3.536/3.5  |  0.77  1.80 /1.8  |   5.3/5   |   17.0   16.0 /16  
  1.0  0.25 | 1.386/1.4  | 3.655/3.7  |  1.21  2.27 /2.3  |   9.9/10  |   45.0   44.0 /44  
  1.0  0.50 | 1.054/1.1  | 3.733/3.7  |  1.55  2.68 /2.7  |  15.7/16  |  104.0  103.0 /103 
  1.0  1.00 | 0.596/0.6  | 3.789/3.8  |  1.87  3.19 /3.2  |  23.8/24  |  294.0  293.0 /291 
rows matching the printed value at its precision, of 12: u1 11, u2 11, es 1, gap 10, cost 12, yrsD 5, yrs 9
```

## Reading

- Figure 2 reproduces exactly: u0 3.80%, u1 0.90%, u2 3.66%.
- Table 2, barriers: u1 and u2 each match 11 of 12 rows at the printed
  precision. The misses are at the shortest delay (0.08 years), consistent
  with the paper's solver (Excel Solver, n. 12) rather than a different
  model.
- Table 2, E[s]: the printed column equals u2 - u1 in 10 of 12 rows and
  the conditional expectation u2 - E[X_Delta | survive] in 1. The column
  is the paper's approximation (p. 2179), not the quantity its note
  describes (PK-D5).
- Table 2, cost: K/(K + (u2 - u1)) matches all 12 rows.
- Table 2, time between issues: the expected time from u2 down to u1
  matches 9 of 12 rows without the delay and 5 of 12 with it. The column
  omits the delay that its note includes (PK-D6).
