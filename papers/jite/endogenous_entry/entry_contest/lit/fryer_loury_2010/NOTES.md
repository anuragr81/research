# Notes — Fryer and Loury (2010), pass 1 (working paper, read in full)

Run `python3 lit/fryer_loury_2010/verify_fryer_loury.py` from the repository
root. It checks the quotations verbatim, on the page named, against the cached
text of the working paper (pinned by sha256), runs a reversed-quote control, and
measures `lean/mathlib/FryerLoury.lean` (see `LEAN`).

## Verdict for C10, C12 and C13

- **C13** (the payers' share as a function of how many pay is new): the claim
  stands only in a narrower form. Fryer and Loury have a binary investment in
  which those who do not invest still compete. They derive the gap between
  groups with different cost laws: the group with higher costs is lower in the
  hierarchy and holds fewer slots (FL-6, FL-9, FL-10). What they do not have:
  - a finite number of contestants with a fixed prize;
  - the share of the payers as a function of how many pay (M31's `ξ(k)`), and
    its comparative statics in the prize, the fee, the wealth profile and the
    weight on talent.

  C13 must cite them as the closest precedent and claim only that.
- **C10** (the share is set by the count and moves with the wealth profile):
  the direction, that the class with higher costs holds fewer positions, is
  theirs under laissez-faire, for groups rather than wealth ranks. C10 should
  say so.
- **C12** (the sign of M28): they have no weight on talent. In a draft passage
  they conjecture, without deriving it, that a blind policy which rations
  slots at random lowers investment in both groups (FL-D4). That is M28's
  direction. C12 stands, and the conjecture is worth citing.

## Findings from the formalisation

1. The group-gap formula on p.11 is an identity of the mixture, and its sign
   needs only the two orders (`group_gap`, `group_gap_pos`). That the
   likelihood-ratio Assumptions 1 and 2 give those orders is stated on
   pp.6-7 without proof and is not formalised.
2. In the two-job version the investors' share of slots is exactly their
   population share when effort does not change productivity at the
   threshold, and above it otherwise (`investor_share_eq`,
   `investor_share_gt`). This is the continuum counterpart of
   `ξ(k) = (k+1)/(Q+1)` at `F = G` in M31.
3. Three cross-references are wrong in the working paper (FL-D1 to FL-D3).

## Published-version pass, 13 October 2026 (JPE page proofs)

**Verdict.**
- **C13** stays as narrowed: the published version derives group slot rates,
  not the payers' share as a function of how many pay.
- **C10's** citation stands. The group gap is now an explicit statement
  (FLP-2, FLP-9; `jpe_slot_rates`).
- **C12's** sentence on the conjecture must name the working paper, since the
  published version drops it.
- **C6:** Proposition 1 is the continuum counterpart of count invariance. With
  heterogeneous costs and a binary investment the investment rate is unique
  (FLP-8, `jpe_unique_rate`), and the investors are the agents below a cost
  threshold (FLP-7), so identities are pinned too. With finitely many
  contestants the count is pinned while identities need not be (M6, M7). C6
  should say this.

