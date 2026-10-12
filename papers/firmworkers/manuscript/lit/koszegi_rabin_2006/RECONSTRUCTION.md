# Reconstruction of Kőszegi and Rabin, 2006

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| KR | "A Model of Reference-Dependent Preferences", *Quarterly Journal of Economics* 121(4), November 2006, pp.1133–1165 | publisher PDF with text layer (XPP), 33 pages, Drive `KoszegiRabin2006.pdf`, id `1U8tvEr95taSmkN84vqMwMgrhhVXiZL0v`, uploaded 12 Oct 2026; cached `~/.cache/firmworkers/koszegi_rabin_2006.pdf`, sha256 `c02790a4166994ca7e8a8b8eb675067c854227d280c02029413318b75964c581` | [F], published |

Text extracted with `pdftotext -layout`; form-feed block `i` is printed page
`1133 + i`; p.1133 ends with its number and every later block's running head
carries it. The text layer renders Greek letters and some operators as
private-use glyphs (`␩` for η, `␭` for λ, `␮` for μ, `兩` for the bar); the
quotations avoid them. Read in full on 12 Oct 2026, including the Appendix.

**Why this paper.** The contrast case for the reference-dependent sub-section
(decision of 12 Oct 2026): here the reference point is endogenous too, but it
is the person's own recent rational expectations, not the income norm of the
person's status group. The novelty check must place `R_h` against it. It is
also the source of the observation that, for large losses, concave consumption
utility counteracts the convexity of gain–loss utility in losses (KR-9), which
bears on displaced workers.

## Primitives (§II, pp.1137–1141)

- Utility `u(c|r) = m(c) + n(c|r)`, consumption utility `m` and gain–loss utility
  `n`, both separable across dimensions; `n_k(c_k|r_k) = μ(m_k(c_k) − m_k(r_k))`.
- A stochastic outcome `F` against a stochastic reference `G`:
  `U(F|G) = ∫∫ u(c|r) dG(r) dF(c)`.
- `μ` satisfies A0 (continuous, `μ(0) = 0`), A1 (strictly increasing), A2
  (`μ(y) + μ(−y) < μ(x) + μ(−x)` for `y > x > 0`, loss aversion for large
  stakes), A3 (`μ″ ≤ 0` for gains, `≥ 0` for losses, diminishing sensitivity),
  A4 (kink ratio `λ > 1`); A3′ replaces A3 by linearity away from zero.

## Derivation chain

1. **Proposition 1 (p.1140).** A lower reference makes a person happier; status
   quo bias for riskless bundles under A0–A4 (part 2), for lotteries under A3′
   (part 3).
2. **Proposition 2.** With `m` linear, `u(c|r) − u(r|r) = Σ v_k(c_k − r_k)`,
   with each `v_k` satisfying A0–A4: locally, prospect theory.
3. **Reference point (§III).** The reference point is recent rational
   expectations; personal equilibrium (PE, Definition 1) and preferred personal
   equilibrium (PPE, Definition 2).
4. **Proposition 3 (p.1145).** With a deterministic choice set and A3′, PPE
   maximises consumption utility: ex ante expected gain–loss utility is zero for
   a sure outcome and negative for a nondegenerate lottery.
5. **Shopping (§IV).** With `μ(x) = ηx` for gains and `ηλx` for losses:
   `p_min = (1 + η)/(1 + ηλ) < 1 < p_max = (1 + ηλ)/(1 + η)`; inequalities (6)
   (attachment effect, willingness to pay rises with the probability of buying)
   and (7) (comparison effect, it rises with the expected price).
6. **Driving (§V, Proposition 4).** Targets are expectations: unexpected morning
   income lowers afternoon work, expected wage increases raise participation.
7. **Conclusion (§VI).** An algorithm translating reference-independent models;
   judgment needed on dimensions and the moment of first focus.

## What was reconstructed

That A0 and A2 together give `μ(x) + μ(−x) < 0` for `x ≠ 0` (the step the
proof of Proposition 1.2 takes "by A2"), and Proposition 1.2 itself in one
dimension; the piecewise-linear `μ` of §§IV–V, its symmetric sum, and that it
satisfies A2 exactly when `λ > 1`; ex ante expected gain–loss utility of a
finite lottery under that `μ`, and its sign (the core of Proposition 3);
Proposition 2's identity in one dimension with affine `m`; the shopping
thresholds and inequalities (6) and (7) from utilities (4) and (5). See
`CLAIMS.md`.

## What was not reconstructed

Proposition 1.3, the multi-dimensional statements, A4's limit ratio in
Proposition 2, the PE/PPE existence argument (via Kőszegi 2005), the mixed
equilibrium of footnote 14, the driving model (8)–(9) and Proposition 4.
