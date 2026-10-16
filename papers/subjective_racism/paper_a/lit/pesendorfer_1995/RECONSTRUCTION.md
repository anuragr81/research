# Pesendorfer (1995), reconstruction

Wolfgang Pesendorfer, "Design Innovation and Fashion Cycles", *American
Economic Review* 85(4), September 1995, pp. 771–792.

## Source read

- The JSTOR scan of the published article, 22 PDF pages with an OCR text
  layer (JSTOR stable 2118231, as in the PDF metadata). Cached at
  `~/.cache/paper_a/sources/pesendorfer_1995.pdf` (455,673 bytes), sha256
  `d3519ab9…3f03`, and its extracted text `pesendorfer_1995.txt`, sha256
  `4299dd24…9b26`.
- The text of every page, PDF pages 1 to 22, was read on 16 October 2026, so
  the evidence tag is [F]. The extracted text sets the two columns side by
  side and garbles symbols, so PDF pages 5, 6, 7, 8, 10, 12 and 14 were also
  rendered at 110 dpi and read from the image. Every equation used below is
  taken from an image.
- Pages in this record are PDF pages. PDF page `n` is journal page
  `770 + n`.

## The model, from its own primitives

1. **Consumers and types** (PDF p. 5). A continuum `q ∈ [0, 1]`. Consumer `q`
   is a high type if `q ≤ α` and a low type if `q > α`. The type may be
   education, entertainment skills or human capital, and it is not observed
   by others.
2. **Match payoff** (PDF p. 5). `u(i, j)` for a type-`i` consumer matched
   with a type-`j`. Both types prefer a high partner (PF-Q6), and high types
   value a high partner more than low types do (PF-Q7), which is assumption
   (1), `u(h,h) − u(h,ℓ) > u(ℓ,h) − u(ℓ,ℓ)`, or `v^h > v^ℓ`. Footnote 6
   grounds (1) in production complementarities between the partners' human
   capital (PF-Q8). Utility is quasi-linear in money, and designs do not enter
   utility.
3. **Designs and matching** (PDF pp. 5–6). A designer creates designs at a
   fixed cost `c` and produces them at zero marginal cost. Everyone sees which
   design a person uses. Users of the same design are matched at random. The
   chance that a user of design `n` meets a high type is
   `μ_h(n)/[μ_h(n) + μ_ℓ(n)]`, the share of high types among its users. Pairs
   separate at the end of each period, and histories are not observed
   (PF-Q11).
4. **Static demand** (PDF pp. 6–7). If `[0, q]` use a new design, a high
   type `q ≤ α` meets a high type with probability 1 inside and
   `(α − q)/(1 − q)` outside, so she pays at most `v^h (1 − α)/(1 − q)`,
   eq. (2). A low type `q > α` meets a high type with probability `α/q` inside
   and 0 outside, so she pays at most `v^ℓ α/q`, eq. (3). Eq. (4) puts these
   together as the inverse demand `f(q)`, which rises on `[0, α]` (a
   bandwagon effect), peaks at `α`, and falls on `(α, 1]` (a snob effect),
   with `f(1) = α v^ℓ > 0`. Footnote 8 says the matching interpretation makes
   the payoff linear in the probability of being taken for a high type
   (PF-Q9), and that the matching game, unlike pure signalling, carries a
   screening role. Footnote 9 says that with a continuum of types a larger
   in-group lowers the average quality of both groups (PF-Q10), and the slope
   of `f` then depends on the curvature of `u(q, ·)`.
5. **The dynamic game** (PDF pp. 7–9). Each period the designer chooses
   whether to innovate and the prices, consumers buy and sell, then choose a
   design and are matched. Eq. (5) is the consumer's period payoff,
   `(1 − δ){ν_t u(i,h) + (1 − ν_t) u(i,ℓ)} − p_t·y_t`. The designer
   coordinates demand on his latest design, and old designs sell at a zero
   price. Weak Markov coordination (WMC) equilibria are defined by an
   acceptance function `P(·)`.
6. **Theorem 1** (PDF p. 9). A WMC equilibrium exists, and the designer
   innovates infinitely often if `c < M(δ)`, the one-innovation durable-goods
   monopoly value.
7. **Proposition 1** (PDF p. 10). The price of a design falls and the
   in-group grows until the next innovation. Just before it, either exactly
   the high types own the design (elitist) or everyone does (egalitarian). A
   lower bound on cycle length is `log(1 − c/αf(α))/log δ`. A large
   `v^h − v^ℓ` gives an elitist cycle. Examples 1 and 2 (PDF pp. 11–12) are
   numerical cycles of length three.
8. **Proposition 2** (PDF p. 12). With very short periods the designer's
   payoff vanishes, almost everyone owns the current design almost always,
   and fashion barely changes who meets whom.
9. **Competition** (PDF pp. 13–15). A "fashion czar" can be a monopolist
   under free entry. Proposition 3 gives subgame-perfect equilibria with
   `N ≥ 2` designers in which only high types buy, at a price that starts at
   `v^ℓ(1 − δ^{T+1})`, and low types never pay a positive price. High types
   meet high types and low types meet low types, so matching is efficient
   (PF-Q12). A price cut is read by the high types as the design going out of
   fashion. They leave, and the design loses its value to low types too.
10. **Conclusions** (PDF pp. 15–17). Fashion needs types and histories to be
    unknown (PF-Q13). Complementarity is less justified when the type is
    wealth and couples pool it (PF-Q14). A designer whose clients are mostly
    low types is less attractive (PF-Q15). Sumptuary laws are read as a way to
    avoid wasteful cycles where the lawmaker can identify efficient matches.
    Two types, and one design marketed at a time, are named as limitations.
11. **Appendix** (PDF pp. 17–20). Strategies, the outline proof of
    Theorem 1, and proofs of Propositions 1 to 3.

## Derivation chain, as formalised

- **Matching technology (i).** The chance of a high partner is the share of
  high types among the actual users of a design. An all-high pool gives 1, the
  non-users of `[0, q]` give `(α − q)/(1 − q)`, and the users of `[0, q]` with
  `q > α` give `α/q` (`in_pool_high`, `out_pool_high`, `in_pool_low`).
- **Static demand.** Eqs. (2) and (3) follow (`eq2`, `eq3`), and eq. (4)
  rises on `[0, α]`, falls above, peaks at `v^h` and ends at `α v^ℓ > 0`
  (`bandwagon`, `snob`, `peak`, `f_one`). Eq. (5) is linear in the chance of a
  high partner, so every type that prefers a high partner values a richer pool
  more (`pool_value_mono`).
- **Sorting and screening.** Complementarity makes a richer pool worth more to
  a high type than to a low type (`single_crossing`), and a price that admits
  only high types exists exactly under complementarity
  (`separating_price_iff`).
- **Proposition 1.** The step that `(q − q′) f(q)` peaks at `α` or 1
  (`prop1_static_step`), the lower bound on cycle length from the revenue
  premise (`prop1_ii`), and the unbounded-gain step of (iii)
  (`prop1_iii_step`).
- **Examples.** The prices that do and do not follow from (9) at the printed
  numbers (`example1_last`, `example1_second_by_eq9`, `example2_second`,
  `example2_last_by_eq9`).
- **Proposition 3 and competition.** A price `v^ℓ(1 − δ^n)` with `n` periods
  of use left equals a low type's value of an all-high pool for those periods
  (`prop3_price`). A covering cycle length exists when `c < α v^ℓ/N`
  (`prop3_T_exists`). The price is at least `2c/α` (`price_floor`).
  Footnotes 24 and 26 (`fn24`, `fn26`).
- **The necessary condition of PDF p. 15**, in our reading. With types
  observed a high type meets a high type without the design, so the design is
  worth nothing (`observed_type_no_value`).
- **The contrast with Paper A.** On two classes A1's quadratic utility
  satisfies (1) (`quadratic_complementarity`), but the low class prefers a low
  partner (`control_quadratic_low_prefers_low`), so avoidance runs both ways
  (`quadratic_avoidance_both_ways`). With two types a pool's spread is fixed
  by its mean (`two_type_gap`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` gives one row per result. In short:

- Not formalised, 7 rows. Theorem 1, Proposition 1 (iv), Proposition 2,
  Claims 1 and 2 with (A1) to (A4), eqs. (8) to (10) as an equilibrium,
  matching technology (ii) and (iii), and footnote 9's continuum example. They
  rest on the durable-goods monopoly results of Gul, Sonnenschein and Wilson
  (1986), on mixed pricing, or on limits in `δ`.
- Special cases, 7 rows. Proposition 1 (i) to (iii) are the steps of their
  proofs, with the revenue bound of (ii) as a hypothesis. Proposition 3 is its
  price path and cycle length without subgame perfection. The examples are
  checked only where their prices follow from (9). The necessary condition of
  PDF p. 15 is our reading.
- Footnote 9's example `u(q, q′) = (1 − q)^a (1 − q′)^a` is taken from the
  image without a check of the threshold `a < 1/3`.
