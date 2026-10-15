# Notes — Liu, Garg and Borgs (2022), pass 1 (read in full)

Run `python3 lit/liu_garg_borgs_2022/verify_liu_garg_borgs.py` from the repository root.

## Verdict for C1 to C14

No claim is overturned. The paper is a positioning neighbour for C13.

- **C13** (the entrants' share as a function of how many enter): unaffected. Access is the
  admission probability of a group fixed by environment, in a continuum with continuous effort.
  There is no choice to pay, so there is no share of those who paid and no count to vary. C13
  can cite the paper as the nearest construct (LGB-D4), adding that access shares the
  requirement of a fixed number of places (LGB-14).
- **C9, C12** (the sign in the weight on the base component): unaffected. The lever is the
  randomization of the reward, and the object is effort or access, not a count of entrants.
- **C3, C6** (the count pinned): unaffected; rank preservation pins who is admitted in a
  continuum with effort, which is a different object (LGB-D2).
- **C1, C5**: unaffected; there is no wealth.

## Findings from the formalisation

1. Two slips in the proof of Proposition 4.4 (LGB-D3); the proposition holds
   (`access_pure`, `access_antitone`).
2. The core of rank preservation is three short lemmas: the increment inequality for a concave
   (convex) function, Lemma C.4, and the closing step (`no_overtaking`).
