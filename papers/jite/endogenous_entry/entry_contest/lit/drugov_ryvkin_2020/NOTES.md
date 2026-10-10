# Notes — Drugov and Ryvkin (2020), pass 1 (working paper, read in full)

Run `python3 lit/drugov_ryvkin_2020/verify_drugov_ryvkin.py` from the
repository root. It checks the quotations verbatim against the cached text,
runs a reversed-quote control, and measures
`lean/mathlib/DrugovRyvkinNoise.lean` (see `LEAN`).

## Verdict for C12

C12 waited on this paper to confirm that it has no entry margin. It has one,
in Section 5.3, and its sign is the opposite of M28's:

- **Intensive margin, the same sign as M28.** Effort falls as noise becomes
  more dispersed, for any prize schedule (Proposition 1); a scale change is a
  dispersive change (p.11). Raising μ scales the base score up against the
  bought component, and the number who pay falls (M28). Paying is the
  investment, so it moves like their effort.
- **Entry margin, the opposite sign.** With free entry and an outside option,
  the less dispersed noise has weakly fewer entrants (Claim 1, p.21;
  `less_noise_fewer_entrants`), because effort is higher and entry pays less.
  Morgan, Tumlinson and Vardy have the same sign for the same reason (L20).
- So C12 now claims M28's sign on the entry margin as new against both entry
  models read, and waits only on the search of binary pre-contest investment
  shared with C13.

## Findings from the formalisation

1. Claim 1 needs only one of the two payoff sequences to fall in the count
   (`entry_count_le_of_decX`); the paper assumes both.
2. Claim 3 needs a non-negative outside option
   (`control_claim3_needs_nonneg_outside`); the paper assumes ω > 0.
3. The sentence on p.21, "Y can be dominated by X in the dispersive order",
   must mean Y more dispersed for consistency with Proposition 1 (DR-D1).
