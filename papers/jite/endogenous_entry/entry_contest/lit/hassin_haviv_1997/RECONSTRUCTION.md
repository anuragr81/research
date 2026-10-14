# Reconstruction of Hassin and Haviv, "Equilibrium Threshold Strategies: The Case of Queues with Priorities"

**Source read.** The authors' preprint (17 pages, with a text layer), which the author uploaded
to Drive as `ay.pdf` on 14 October 2026, read in full [F, preprint version]. The published
version is *Operations Research* 45(6): 966-973 (1997), which is not used. The PDF is cached at
`~/.cache/entry_contest/hassin_haviv_1997.pdf`. The preprint's page numbers are its PDF pages.

**Why this paper.** It was a lead of the application search of 14 October 2026: a fixed toll
buys priority, and those who do not buy are still served.

## Model (pp.5-6)

- Poisson arrivals at rate `λ` to one exponential server at rate `μ`, with `ρ = λ/μ < 1`.
- Two queues. High priority preempts, and costs a toll `θ`; low priority is free.
- Customers are identical, with waiting cost one per unit of time. They see the queues and
  decide on arrival.
- `B = 1/(μ − λ)` is the expected busy period.
- Strategies are thresholds: buy priority if and only if the number in the system is at least
  the threshold `x`, mixing at `n` when `x = n + p`.

## Results

1. **The framework (Section 2).** If the best-response threshold `k(x)` is non-increasing
   (avoid the crowd, ATC), there is a single fixed point. If it is non-decreasing (follow the
   crowd, FTC), there may be many.
2. **Proposition 3.1.** The priority queue is FTC: the more others buy, the more one gains from
   buying, because the risk of being overtaken rises.
3. **Propositions 4.1-4.2.** An integer `n ≥ 1` is an equilibrium iff
   `1/μ + θ − B ≤ E(n) ≤ 1/μ + θ`, with `E(n)` the expected wait at the last low-priority place.
   A mixed `x` is an equilibrium iff `E(x) = θ + 1/μ`.
4. **Proposition 5.1.** `E(n + 1) − E(n) ≥ 1/μ`, with limit `1/μ`. The paper states that "the
   number of pure threshold Nash equilibria is at most ⌊1/(1 − ρ)⌋", and that the pure
   equilibria are consecutive integers (p.12).
5. **Section 6.** Balking, with `B_m` in place of `B`, and a non-threshold equilibrium.

## What is formalised

See `lean/mathlib/HassinHaviv.lean`:

- ATC uniqueness and an FTC control;
- Proposition 4.1 as a definition, and the counting step of Proposition 5.1;
- the appendix recursion for `E`, computed exactly at `ρ = 3/5`. It gives three pure equilibria
  where the stated bound is two, with controls (`E(1) = B`, Proposition 5.1's increments, and
  thresholds 4 and 8 failing).
