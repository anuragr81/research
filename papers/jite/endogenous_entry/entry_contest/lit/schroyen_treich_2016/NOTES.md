# Notes — Schroyen & Treich, pass 1

Run: `python3 verify_st.py` — **6 checks, 0 failures**.

**Scope.** These checks establish that our reading of the source is
arithmetically consistent. They do not reprove Theorem 3, whose derivation is
a quadratic-form argument on the Hessian of the best-response function that
none of our tooling reaches.

## Source confirmation

The claim `LITERATURE.tex` rests on was read directly from the PDF, not from
memory. Theorem 3 appears on p.35 of the WP:

> **Theorem 3** Let `A = -u''(.)/u'(.)` and `P = -u'''(.)/u''(.)`. In the
> symmetric privilege contest model, the sign of the quadratic form (A.9) is
> positive iff `2A(1 - m^2) > P`.  — eq. (A.16)

and the three reductions are in the paragraph immediately following: quadratic
`P = 0` so `m < 1`; CARA `A = P` so `m < 2^(-1/2) ~ .707`; CRRA
`gamma(1/2 - m^2) > 1/2`. `LITERATURE.tex` reproduces all three correctly.

## Results

| ID | Result |
|---|---|
| ST-2 | CONSISTENT. Arrow–Pratt definitions give `A = P = a` under CARA, which is what makes the CARA reduction work. |
| ST-3 | CONSISTENT. `P = 0` exactly; `2A(1-m^2)` factors as `4b(m-1)(m+1)/(2bw-1)`, boundary exactly `m = 1`. |
| ST-4 | CONSISTENT. Boundary solves to `sqrt(2)/2 = 0.707107`. |
| ST-5 | CONSISTENT, and *stronger than a boundary match*: `[2A(1-m^2) - P] / [gamma(1/2 - m^2) - 1/2] = 2/w`, a positive constant in `m`, so the two inequalities have the same truth value everywhere, not merely the same root. |
| ST-8 | CONSISTENT. The restatement multiplies by a positive factor, so it is sign-preserving; relative RA and relative prudence are the absolute measures scaled by `(w-x)`. |
| ST-12a | CONSISTENT, and this is the one that matters. See below. |

## ST-12a — the separation from Theorem 3 is real, not verbal

`LITERATURE.tex` claims P9-gen improves on Theorem 3 by needing "no third
derivative". That is only a contribution if the third derivative actually
changes Theorem 3's answer. It does:

- CARA with `a = 1`: `A = 1`, `P = 1`.
- Log utility at `w = 1`: `A = 1`, `P = 2` (derived in the suite from
  `u = log w`, not asserted).

At `m = 1/2` these give `2A(1-m^2) - P` equal to `+1/2` and `-1/2`
respectively. **Same `A`, same `m`, opposite conclusions, driven purely by
`P`.** So "no prudence condition" is a substantive difference between P9-gen
and Theorem 3, and the contribution sentence can rely on it.

## Non-vacuity controls

Every check was confirmed able to fail:

- ST-4 accepts `2^(-1/2)` and **rejects** a false boundary of `1/2`.
- ST-5's ratio test rejects a wrong right-hand side (`... > 1` instead of
  `> 1/2`) — the ratio then depends on `m` rather than being constant.
- ST-12a returns False when fed two utilities with the same `A` *and* the
  same `P`, confirming the witness is doing work.

## Discrepancies found

1. **Date, minor.** `LITERATURE.tex` describes the source as "TSE Working
   Paper 16-699 dated 11 April 2016". 11 April 2016 is the *manuscript* date
   on the title page; the WP series cover page is dated **September 2016**.
   Both are in the PDF. Worth stating as "WP 16-699 (September 2016),
   manuscript dated 11 April 2016" to avoid a referee query.
2. No substantive discrepancy found between `LITERATURE.tex` §sec:st and the
   source on any checked claim.

## Not attempted

- **ST-10**, that under CARA the two wealth effects in the rent-seeking
  contest "exactly cancel". This is the sub-claim most worth formalising next,
  because `PROOFS.tex` uses it to rule out a model variant (making `V` money
  added to the resource). It is currently an unverified structural reading.
- **ST-1, ST-9, ST-11** are readings of functional forms and prose; not
  arithmetic, deliberately unchecked.
- Theorem 2 (the Hessian quadratic form) — out of reach of SymPy and of
  Mathlib-free Lean alike.

## Lean

None. Nothing in this paper is discrete or order-theoretic; a Lean file here
would be decoration. Recorded per `lit/README.md` rule: absence is the
default.
