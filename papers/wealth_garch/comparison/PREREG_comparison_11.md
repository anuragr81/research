# Pre-registration: the testing thread's comparison, eleventh file

Written and committed 24 October 2026, after the first test-window result
(results_test_judged.md) was seen. It amends the facts scored in both
threads from here on. The author agreed (24 October 2026) to score F1 and
F2 only.

## What was seen before this file

Everything listed in PREREG_comparison_10.md, and results_test_judged.md:
on the 17 banks, 2015-2024, equity volatility misses F1 + F2 by 30.0
points against the baseline's 139.8; F3 and F4 were not scored for it,
and the baseline's were 93.4% and 72.2% against 33.3% and 55.6% observed,
from 9 observed recapitalisations.

## Amendment

1. F1 (dividends paid from a buffer below 1 sigma_i over R) and F2 (buffer
   above 4 sigma_i over R) are the scored facts, in both threads, for
   every comparison run after this file. Their definitions, the 10-point
   rule and the samples are unchanged.
2. F3 and F4 are still computed and reported, observed and for any model
   with issuance, and decide nothing.
3. Results recorded before this file stand as recorded, F3 and F4
   included; none is re-scored.

## Reasons

- The question is the size of the buffers banks hold and when they pay
  out; F1 and F2 measure that. F3 and F4 measure how banks recapitalise,
  which the paper no longer claims to explain.
- Recapitalisations are few. The binomial standard error of an observed
  share, at its observed value, is:

  | Banks | Window | Recapitalisations | F3 | F4 |
  |---|---|---|---|---|
  | sample B, 61 | 2005-2014 | 93 | 4.9 points | 5.0 points |
  | sample B, 61 | 2015-2024 | 46 | 5.3 points | 6.8 points |
  | the 17 banks | 2005-2014 | 31 | 8.9 points | 9.0 points |
  | the 17 banks | 2015-2024 | 9 | 15.7 points | 16.6 points |

  On subsets like the 17 banks the error is as large as the 10-point rule
  or larger, so agreement on F3 and F4 cannot be judged there.
- This amendment is written after a test-window result was seen. It
  changes no scored outcome of that result, since F3 and F4 were not scored
  for the enhanced model, but it does narrow what the comparison with the
  baseline covers: the baseline's dividend and buffer predictions, not its
  issuance rule. Results reports say so.
