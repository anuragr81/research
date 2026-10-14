# Notes — Hassin and Haviv (1997), pass 1 (read in full from the preprint)

Run `python3 lit/hassin_haviv_1997/verify_hassin_haviv.py` from the repository root.

## Verdict for C1 to C14

No claim is overturned.

- **C3, C6** (count invariance): unaffected, and the paper is the clearest contrast found. Its
  priority purchase is follow-the-crowd (Proposition 3.1), so equilibria can be many. In ours
  the gain does not rise with the count, the avoid-the-crowd case, which its Section 2 says has a
  single fixed point (HH-D2). The customers are identical, so the paper says nothing about
  heterogeneous entry costs. C3 and C6 can cite it as the case with the opposite monotonicity.
- **C13** (the entrants' share): unaffected. Those who do not buy are still served, later
  (HH-3), which is our property that non-payers still compete. But there is no fixed prize and
  no share of positions, so no share is analysed.
- **C1, C5, C9, C12**: unaffected. There is no wealth, no spread and no weight on a base score.
- **Research question.** The paper is the formal case of priority bought at a fixed price with
  non-buyers still served. It differs from ours in having no ranking by merit, no wealth and a
  dynamic queue.

## Findings from the formalisation

1. The bound of Proposition 5.1 is off by one when `1/(1 − ρ)` is not an integer (HH-D3). The
   counting step gives ⌊1/(1 − ρ)⌋ + 1, which is attained at `ρ = 3/5` by the paper's own
   recursion, computed exactly in Lean by kernel evaluation.
2. The implementation of the recursion is checked against two of the paper's statements,
   `E(1) = B` and the increment bound of Proposition 5.1 for `n = 1, …, 10`.
