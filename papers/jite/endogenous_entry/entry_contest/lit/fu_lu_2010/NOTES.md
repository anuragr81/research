# Notes — Fu & Lu (2010), pass 1

Run: `python3 verify_fulu.py` — **4 checks, 0 failures**.

**Scope.** Our reading of the source is arithmetically consistent. Nothing
here reproves Theorem 1, whose substance is the existence of a feasible
`(V*, S*)` satisfying Lemmas 4 and 5; only the count arithmetic that eq. (7)
delivers is checked.

## Source confirmation

Read from MPRA Paper No. 945 (posted 28 Nov 2006). Equation (7), p.10:

> `E = Π₀ − N(V*,S*)·C`   (7)
>
> Note the importance of Equation (7). It states that in the optimally
> designed contest, the equilibrium total effort is given by the difference
> between the total budget of the contest organizer and the total entry costs
> incurred by participating contestants, regardless of the contest technology.
> In addition, the right-hand side of Equation (7) strictly decreases with
> `N(V*,S*)` [...] Hence, it can be deduced that the equilibrium efforts are
> bound from above by `E = Π₀ − 2C`.

> **Theorem 1** The unique optimal contest induces exactly two potential
> contestants to participate, and induces the total effort of `E = Π₀ − 2C`.

`LITERATURE.tex`'s description — "total effort equals budget minus `N` times
the entry cost [...] is what drives their Theorem 1 that the optimal contest
attracts exactly two entrants" — is faithful, including the causal direction:
the paper's own proof begins "Equation (7) shows that only a contest that
attracts two contestants to participate may induce the total effort of `E`."

## Results

| ID | Result |
|---|---|
| FL-1 | CONSISTENT. `dE/dN = −C`, matching the source's own "strictly decreases with `N`". |
| FL-2 | CONSISTENT. `N = 2` strictly beats `N ∈ {3,4,5,10}`, and `E(2) = Π₀ − 2C` matches Theorem 1's stated value exactly. |
| FL-3 | CONTROL passes. With `C = 0`, `dE/dN = 0` and `E = Π₀`: the count-dependence disappears, confirming FL-1/FL-2 test the entry-cost channel. |
| FL-4 | CONSISTENT. Eq. (7) with `E >= 0` rearranges to `N·C <= Π₀` — the same shape as P7's `(m+1)·kappa <= V`. |

## The P7 relation

The shared content is proved once, in
`../fu_jiao_lu_2015/Accounting.lean` (`accounting_bound`), with both bounds
as instances; it is not duplicated here. Fu–Lu's version is the sharper of
the two literature cases for the survey's purpose: eq. (7) is an **equality**,
not an inequality, so the bound `N·C <= Π₀` is exact rather than slack.

Note what this does *not* establish. Fu–Lu's `N` is the equilibrium count in
an **optimally designed** contest — the organiser chooses `(V, S)` to induce
it. P7 has no designer: the cap holds under fixed rules for whatever count the
wealth ordering produces. The survey's phrase "holds under fixed rules with no
designer" is doing real work and should be kept when the citation is added.

## Non-vacuity control

FL-2 was checked against its own negation: `N = 3` does **not** beat all other
candidates, so the maximiser test discriminates rather than accepting any
value.

## Discrepancy / outstanding

- **Title mismatch, already flagged by the survey.** The MPRA posting is
  "Contest design and optimal endogenous entry"; `LITERATURE.tex` records that
  Fu–Jiao–Lu cite a different title for the *Economic Inquiry* version. Not
  resolved here — the published version is not in the Drive folder. This
  remains on the survey's outstanding list and the reference should not be
  finalised until checked.
- Theorem numbering may differ between the MPRA working paper and the
  published article. Any citation of "Theorem 1" should be confirmed against
  *Economic Inquiry* before submission.

## Not attempted

- Lemmas 4 and 5, and the existence half of Theorem 1.
- FL-A, FL-E, FL-F: structural and textual readings.
