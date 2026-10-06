# Notes — Schroyen & Treich

Run `python3 lit/schroyen_treich_2016/verify_st.py` from the bundle root.
Result on 2026-10-06 is **17 checks, 0 failures**. The Lean audit inside the
suite reports 50 theorems declared, 50 audited, 14 axiom-free, 36 using only
`propext` and `Quot.sound`, 0 disallowed, 0 `sorry`. The file was compiled
with `lean` 4.34.1 (the `elan` default on this machine), core only, no
Mathlib and no lakefile.

**Scope.** The checks establish that our reading of the source is
arithmetically consistent with the source and that the conditions our
documents attribute to the source are the ones the source states. The
analytic inputs (derivatives of `u` and of the contest success function)
enter the Lean file as named hypotheses, and the suite checks in SymPy that
each hypothesis is the true derivative. Neither tool reproves Theorem 2.

## Source identity

The PDF read is the Drive file `stroyen_treich_weatlh_effects.pdf` (id
`1-V3T-xuauv6garL3e145yokjxOs0-Aw3`, 48 PDF pages), read in full on
2026-10-06 from its text layer, with the pages quoted below also read from
rendered page images. The text layer drops minus signs and Greek letters, so
every quotation below was taken from the page image.

- PDF page 1 is the TSE cover "WORKING PAPERS N° 16-699, September 2016",
  title “The Power of Money: Wealth Effects in Contest” (singular "Contest").
- PDF page 2 carries the stamp "*Manuscript / Click here to view linked
  References", the title "The Power of Money: Wealth Effects in Contests",
  the date "April 11, 2016", and a footnote opening "We would like to thank
  three anonymous reviewers". The last page ends with a URL on
  `ees.elsevier.com/ygame/` with `rev=1`.
- The document is therefore the first revision submitted to *Games and
  Economic Behavior* after review, repackaged as a TSE working paper. The
  typeset *GEB* article (vol. 100, pp. 46–68) is not on Drive. A search of
  the literature folder on 2026-10-06 found only this file for the paper.
- Printed page *n* of the manuscript is PDF page *n*+1. All locators below
  are printed pages.

## SymPy results

| ID | Result |
|---|---|
| ST-2 | CONSISTENT. Arrow–Pratt definitions give `A = P = a` under CARA, which is what makes the CARA reduction work. |
| ST-3 | CONSISTENT. `P = 0` exactly, and `2A(1-m^2)` factors as `4b(m-1)(m+1)/(2bw-1)` with its boundary exactly at `m = 1`. |
| ST-4 | CONSISTENT. The boundary solves to `sqrt(2)/2 = 0.707107`. |
| ST-5 | CONSISTENT and stronger than a boundary match. The ratio `[2A(1-m^2) - P] / [ρ(1/2 - m^2) - 1/2]` equals `2/w`, a positive constant in `m`, so the two inequalities have the same truth value everywhere rather than only the same root. |
| ST-8 | CONSISTENT. The restatement multiplies by a positive factor, so the sign is preserved. Relative risk aversion and relative prudence are the absolute measures scaled by `(w-x)`. |
| ST-12a | CONSISTENT. CARA with `a = 1` and log at `w = 1` share `A = 1` and have `P = 1` against `P = 2`, so `2A(1-m^2) - P` at `m = 1/2` is `+1/2` against `-1/2`. |
| ST-AP | The derivative triples given to Lean are the true derivatives. CARA `u = -exp(-αz)/α` has `(u', u'', u''') = s(1, -α, α²)` with `s = exp(-αz)`. CRRA has `z^(ρ+2)(u', u'', u''') = (z², -ρz, ρ(ρ+1))`. Log at `z = 1` has `(1, -1, 2)`. Quadratic utility has `u''' = 0`. |
| ST-2b | The source's Appendix A.2 values at the symmetric equilibrium (p.31) are correct, and so are the scaled forms `S = 8n³x^j p` used by `thm3_from_A2` and `thm3_end_to_end`, including `c1 c3 = c2²`. |
| ST-2c | Theorem 3 follows from (A.9). Recomputing the best response of the privilege contest by implicit differentiation, the (A.9) numerator divided by `2A(1-m²) - P` equals `u'² u'' x / (u'' x - u')³`. That ratio contains no `u'''` and is positive whenever `u' > 0 > u''`. The intermediate expression of the p.44 proof equals `(m/(4x³))[2A(1-m²) - P]`. The same computation locates two typos on p.43, recorded under Findings. |
| ST-2d | Equation (A.10) is correct. The willingness to pay `Π/u'` has slope `(Π/u')A` and curvature `(Π/u')A(2A - P)`. |
| ST-10 | Under CARA `u' = -αu` and `u'' = -αu'`, so `H'_RS = -α H_RS` identically, and `H'_RS = 0` wherever the symmetric first-order condition `H_RS = 0` holds. The control values from CRRA `ρ = 2` are the true scaled derivatives at `z = 1, 2`, and with `K = 5/4` they give `H_RS = 0` and `H'_RS = 3/16`. |

## Lean formalisation (`SchroyenTreich.lean`)

### Encoding

The real condition `2A(1 - m²) > P` is encoded over `Int` with scaled
rationals. With `A = a/d`, `P = p/d` (common denominator `d > 0`) and
`m = k/n` (`n > 0`), multiplying by `d n²` gives
`gap a p k n = 2a(n² - k²) - p n²` and `Thm3 a p k n` means `0 < gap a p k n`.
The predicate `Thm3U u1 u2 u3 k n` states the same condition directly in the
derivatives `u' = u1`, `u'' = u2`, `u''' = u3`, and `thm3U_iff_thm3` proves
the two forms agree whenever `A` and `P` are computed from the derivatives by
the source's definitions (`IsA`, `IsP`, both cross-multiplied).

The payoff definitions `privilege`, `ability` and `rentSeeking` are the
source's payoffs multiplied by `D`, with the win probability written `q/D`.
The function `uK` (slope 2 below zero, slope 1 above) is a concave increasing
stand-in on `Int`, used only to exhibit that a term inside `u` can carry a
wealth effect.

`hRS` and `dhRS` are `2e·H_RS` and `2e·H'_RS` with `K = m/(4x)` written `c/e`.

### What the Lean statements do and do not establish

Every statement is over `Int`. The proof scripts use only commutative-ring
rewriting and three ordered-ring sign facts. Each `poly` step proves a
polynomial identity, and a polynomial identity that holds at every integer
point is an identity of polynomials, so it holds over the reals. Each `rw`
step substitutes a hypothesis. The sign facts are that a product of two
positives is positive, that a product of a positive and a negative is
negative, and that multiplying by a positive preserves order, all of which
hold over the reals. The transfer of the Lean theorems to real-valued `u`
therefore follows by that argument, and the argument itself is not machine
checked.

### Faithfulness table

Each Lean theorem with its claim ID, its locator, and the source text it
formalises. Quotations are verbatim from the page images, with mathematics
transcribed in plain text. In the transcription `=[SE]` stands for the
source's "=" with "SE" set above it (evaluation at `x_a = x_b = x`), and `≝`
stands for "=" with "def" set above it.

| Lean theorem | Claim | Locator | Source text (verbatim) |
|---|---|---|---|
| `privilege_rent_margin`, `privilege_cost_margin_depends_on_wealth` | ST-1, ST-9 | eq. (4) p.9; p.8 | "U_i = u(w_i − x_i) + Π_i r. (4)" and "In this model, effort is monetary, but the rent —i.e., the privilege— is non-monetary and therefore its marginal value is independent of the level of wealth. Therefore, only the first effect on marginal cost described above is active." |
| `ability_cost_margin`, `ability_rent_margin_depends_on_wealth` | ST-1 | p.12; p.8 | "U_i = Π_i u(w_i + r) + (1 − Π_i)u(w_i) − c(x_i)," and "In this alternative model, rent is monetary but effort —which determines ability— is non-monetary and so the marginal cost of effort is independent of wealth." |
| `rentSeeking_rent_margin_depends_on_wealth`, `rentSeeking_cost_margin_depends_on_wealth` | ST-1, ST-10 | eq. (5) p.14; p.8 | "U_i = Π_i u(w_i + r − x_i) + (1 − Π_i)u(w_i − x_i). (5)" and "In Section 4, we study a model in which both the rent and the efforts are monetary, the so-called “rent-seeking contest” model" |
| `gap_alt`, `thm3_iff_alt` | ST-2 | Theorem 3, (A.16), p.35 | "Theorem 3 Let A = −u''(·)/u'(·) and P = −u'''(·)/u''(·). In the symmetric privilege contest model, the sign of the quadratic form (A.9) is positive iff 2A(1 − m²) > P. (A.16)" and "First, note that this inequality may also be written as 2A − P > 2Am²." |
| `thm3U_iff_thm3` | ST-2 | p.33 | "the coefficients of absolute risk aversion, A_i ≝ −u''(w_i−x_i)/u'(w_i−x_i), and absolute prudence, P_i ≝ −u'''(w_i−x_i)/u''(w_i−x_i)" |
| `wtp_concave_mps_negative` | ST-2 | (A.10) p.33; p.35 | "∂(−dw_i/dr\|dU_i=0)/∂w_i = (Π_i/u'_i)A_i, and ∂²(−dw_i/dr\|dU_i=0)/∂w_i² = (Π_i/u'_i)A_i(2A_i − P_i). (A.10)" and "Thus, if the marginal willingness to pay for rent is concave in final wealth (cf. (A.10)), a small MPS in wealth reduces total effort." |
| `fn8_hessian_form` | ST-6 | Theorem 2 (A.9) p.27; footnote 8 p.28 | "The second-order effect of a MPS in wealth dw_a = −dw_b on aggregate effort x_a + x_b is given by [(f_2)² f_11 − 2(1 + f_1)f_2 f_12 + (1 + f_1)² f_22] / [(1 + f_1)(1 − f_1²)]. (A.9)" and "It can be written as (−f_2, 1 + f_1)(f_11 f_12 ; f_12 f_22)(−f_2 ; 1+f_1)." |
| `a9_at_symmetric_privilege` | ST-6 | p.44 | "Given h_3 = 0, we obtain that f_1 = 0 and 1 + f_1 = 1." |
| `lemma1_first_order_cancels`, `lemma1_sign` | ST-6 | Lemma 1 p.28; p.27 | "Lemma 1 A small redistribution in wealth dw_a = −dw_b = t increases aggregate effort x_a + x_b iff F_11 − 2F_12 + F_22 evaluated at (w, w) is positive." and "x_a(w + t, w − t) + x_b(w + t, w − t) ≃ 2F(w, w) + (F_11 − 2F_12 + F_22)t²." and "In a symmetric equilibrium, a wealth transfer from b to a has no first-order effect on aggregate effort" |
| `thm3_proof_chain`, `thm3_proof_sign` | ST-2 | A.7 pp.43–44; FOC and SOC p.33; assumptions p.9 | "h = −u'(w_a − x_a) + p_1 r = 0", "h_1 = u''(w_a − x_a) + p_11 r < 0", "f_12 = (−r/h_1³) u''_a p_112 (u''_a + p_11 r)", and "Applying Theorem 2 then obtains that the sign of the quadratic form (A.9) is given by the sign of A(p_111 − 3p_112) − P p_11²/p_1". Positivity hypotheses come from "u(·) is thrice continuously differentiable with u' > 0 and u'' < 0" (p.9) and "p_1 = (m/x_a)Π_aΠ_b =[SE] (1/4)(m/x) > 0" (p.31). |
| `thm3_from_A2`, `thm3_from_A2_sign` | ST-2 | A.2 p.31; p.44 | "p_111 … =[SE] (1/2)(m/x³) − (1/8)(m³/x³)", "p_122 … =[SE] −(1/8)(m³/x³)", "p_112 … =[SE] (1/8)(m³/x³)", "3p_112 − p_111 =[SE] (1/2)(m/x³)(m² − 1)" and "Making use of the expressions for the p-derivatives gives A (1/2)(m/x_3)(1 − m²) − P (1/4)(m/x_3). This proves Theorem 3." |
| `thm3_end_to_end` | ST-2 | Theorem 3 p.35 with its proof pp.43–44 | Composition of the three rows above. Under the source's hypotheses the sign of the (A.9) numerator at `f_1 = 0` equals the sign of `2A(1 − m²) − P`. |
| `quadratic_P_zero`, `quadratic_reduction` | ST-3 | p.35; p.11 | "When u is quadratic, P = 0, and the inequality reduces to m < 1." and "When u is quadratic, the condition is m < 1." |
| `cara_AP`, `cara_reduction`, `cara_boundary_707` | ST-4 | p.35; p.11 | "When u is CARA, A = P and the inequality reduces to m < 2^(−1/2) ≃ .707." and "Under CARA this holds when m < 2^(−1/2) ≃ .707." |
| `crra_AP`, `crra_identity`, `crra_reduction` | ST-5 | p.36; p.11 | "When u(·) has constant relative-risk aversion (CRRA) denoted by ρ, the inequality reduces to ρ(1/2 − m²) > 1/2." and "When u has CRRA ρ, this holds iff ρ(1/2 − m²) > 1/2." |
| `gap_scale`, `relative_measures_same_sign` | ST-8 | p.36 | "If we multiply (A.16) by (w − x), we may replace A and P by −(u''(w−x)/u'(w−x))(w − x) and −(u'''(w−x)/u''(w−x))(w − x), the coefficients of relative risk aversion and relative prudence, respectively." |
| `m_dependence` | ST-7 | p.35–36 | "Thus the quadratic and CARA cases illustrate instances where the value of the decisiveness parameter of the CSF determines whether the effect of a MPS in wealth on total effort is positive or negative." |
| `separator_values`, `separator_AP`, `separator`, `separator_derivs` | ST-12a | (A.16) p.35; CARA p.35; log as CRRA ρ = 1 p.13 | The source states (A.16) and "When u is CARA, A = P". It identifies log utility as CRRA with "For this example u(w) = log(w) (i.e., CRRA ρ = 1)" (p.13). The separator itself is our instance of (A.16), not a statement in the source. |
| `separator_interval`, `separator_fails_above_boundary` | ST-12b | (A.16) p.35; p.36 | Our instance of (A.16) with `P = A` and `P = 2A`. At `ρ = 1` the source's own CRRA reduction "ρ(1/2 − m²) > 1/2" reads `1/2 − m² > 1/2`, false for every `m > 0`, which is the log half of `separator_interval`. |
| `rentSeeking_cara_identity`, `rentSeeking_cara_no_wealth_effect` | ST-10 | p.15; p.8; p.19 | "H_RS(w) ≝ (m/4x)[u(w + r − x) − u(w − x)] − (1/2)[u'(w + r − x) + u'(w − x)] = 0.", "H'_RS(w) = (m/4x)[u'(w + r − x) − u'(w − x)] − (1/2)[u''(w + r − x) + u''(w − x)]", "It is then immediately obtained that when u displays CARA then H'_RS(w) = 0." and "In this model, we show that under constant absolute risk aversion (CARA), the two opposing wealth effects exactly offset each other so that wealth has no effect on the efforts of agents." |
| `rentSeeking_crra2_no_cancel`, `rentSeeking_cancel_needs_cara` | ST-10 (control) | p.15 | Control, not a source statement. CRRA `ρ = 2` values satisfy `H_RS = 0` with `H'_RS ≠ 0`. |
| `thm3_from_A2_sign_needs_m_pos` | ST-2 (control) | eq. (2) p.4; p.5 | Control, not a source statement. The source's CSF has "x_i > 0 (i = a, b) and m > 0." (p.5). |
| `cara_reduction_needs_pos_A` | ST-4 (control) | p.9 | Control, not a source statement. The source assumes "u'' < 0", which makes `A > 0`. |
| `two_mul_int`, `three_mul_int`, `four_mul_int`, `pos_mul_iff`, `neg_mul_iff`, `sq_pos_of_ne`, `sq_nonneg_int`, `sq_lt_sq_iff` | support | none | Integer arithmetic used by the rows above. There is no source statement. |

ST-11 has no Lean theorem. The source text is "To conclude, let us mention
some natural extensions to our results. To start with, one may wish to
consider other CSFs, an arbitrary number of players and other dimensions of
heterogeneity (e.g., on the cost or value of rent)." (p.20). The dynamic
extension in `LITERATURE.tex` is "Finally, it could also be interesting to
explore dynamic effects: wealth affects conflict, which in turn affects
wealth, and so on." (p.21). The two-player restriction in ST-6 is "We
consider a strategic game with two players, i = a, b, in which the only
source of heterogeneity is wealth w_i." (p.25).

### Non-vacuity controls (rule 3)

Every Lean conclusion that depends on a hypothesis has a control showing
that the conclusion fails once the hypothesis is dropped.

- `rentSeeking_crra2_no_cancel` and `rentSeeking_cancel_needs_cara`. With
  CRRA `ρ = 2` at `z = 1, 2` (values `4u = (−4, −2)`, `4u' = (4, 1)`,
  `4u'' = (−8, −1)`) and `K = 5/4`, the first-order condition holds
  (`hRS = 0`) while `dhRS = 6`. No CARA coefficient fits those values. The
  exact cancellation is therefore a property of CARA, not of every concave `u`.
- `separator_fails_above_boundary`. At `m = 4/5` CARA and log utility, with
  the same `A`, both violate (A.16). Opposite signs need `m < 2^(−1/2)`, so
  the choice `m = 1/2` in ST-12a is doing work.
- `cara_reduction_needs_pos_A`. With `A = 0` the equivalence of
  `cara_reduction` fails at `m = 1/2`.
- `thm3_from_A2_sign_needs_m_pos`. With `k = 0` the A.2 expression is `0`
  while `Thm3` holds, so `thm3_from_A2_sign` needs `m ≠ 0`.
- The `uK` theorems are themselves the controls for the wealth-free margins.
  A term inside `u` can carry a wealth effect (`*_depends_on_wealth`), and a
  term outside `u` cannot (`privilege_rent_margin`, `ability_cost_margin`).

The SymPy controls reject `2^(−1/2)` replaced by `1/2` (ST-4), a CRRA
right-hand side of `1` in place of `1/2` (ST-5), two utilities with the same
`A` and the same `P` (ST-12a), the square root's derivatives in place of
log's (ST-AP), a wrong sign on the `m³` term of `p_111` (ST-2b),
`2A(1 + m²) − P` in place of `2A(1 − m²) − P` (ST-2c), and `A(2A + P)` in
place of `A(2A − P)` (ST-2d).

The audit itself has a control. A two-theorem file with one theorem built
on `Classical.em` is fed to the same audit, which must report
`Classical.choice` for that theorem and no axioms for the other. A string
containing `sorry` must be caught by the banned-token scan. The name regex
accepts `[A-Za-z0-9_']`, and all 14 digit-containing theorem names (for
example `cara_boundary_707`, `thm3_end_to_end`) are audited.

Two theorems first compiled with `Classical.choice`, `thm3_iff_alt` and
`crra_reduction`, because `omega` was applied to an `↔` goal. Splitting each
`↔` into two implications removed the dependency. No theorem in the final
file depends on `Classical.choice`, and `grind` (which brings it in) is not
used.

## Findings against the source and against our documents

1. **Version, minor.** `LITERATURE.tex` describes the source as "TSE
   Working Paper 16-699 dated 11 April 2016". April 11, 2016 is the
   manuscript date on PDF page 2, and the TSE cover is dated September 2016.
   The manuscript is the post-review revision submitted to *GEB* (reviewers
   thanked, Elsevier stamp, `rev=1`), so its numbering is likely close to the
   published article, although that has not been checked (TODO S1).
2. **Title, minor.** The TSE cover prints "Wealth Effects in Contest", and the
   manuscript title page prints "Wealth Effects in Contests". `refs.bib`
   follows the manuscript.
3. **Notation, minor.** The source writes the win probability as `Π_i` and
   the CRRA coefficient as `ρ`. `LITERATURE.tex` (Primitives; Consequence 3)
   writes `π_i` and `γ`. The displays in `LITERATURE.tex` are therefore
   paraphrases in our notation rather than quotations. The content matches.
4. **ST-10 names one case where the source names two.** The source gives
   exact cancellation in the rent-seeking contest under CARA and also under
   quadratic utility ("It is also easily observed that H'_RS(w) = 0 under a
   quadratic utility." p.15, and "there is no effect of wealth distribution
   across players under CARA or quadratic utility" p.18). `PROOFS.tex`
   (Payoffs paragraph) and `LITERATURE.tex` (Consequence 2) name only CARA.
   Both statements are true as written. The quadratic case is the second
   source-stated case and was not formalised, because our documents do not
   use it.
5. **Typos in the source, not in our documents.** On p.43 the printed
   `h_11 = −p_111 r − u'''_a` has the wrong sign on `p_111 r`, since
   differentiating `h_1 = u''(w_a − x_a) + p_11 r` in `x_a` gives
   `p_111 r − u'''_a`. On the same page the expressions labelled `f_11` and
   `f_22` are `∂²f/∂w_a²` and `∂²f/∂x_b²` respectively, which is the reverse
   of the convention `f(x_b, w_a)` that (A.9) uses. On p.44 `x³` is printed
   as `x_3`. The final expression on p.44 is consistent only with the correct
   sign and the correct labels (check ST-2c), so Theorem 3 is unaffected.
   The Lean hypotheses use the corrected labels, and anyone re-deriving
   Theorem 3 from p.43 should do the same.
6. **"iff" at the boundary.** `LITERATURE.tex` Consequence 3 states that a
   small spread "raises aggregate effort if and only if" (A.16) holds. The
   source's Theorem 3 signs the quadratic form (A.9), the coefficient of
   `t²`, and its Lemma 1 itself uses "iff" (p.28). Our wording follows the
   source. At `2A(1 − m²) = P` the `t²` term vanishes, and neither the source
   nor our sentence covers that case.
7. **No substantive discrepancy** was found between the source and
   `LITERATURE.tex` §sec:st, `LITERATURE.tex` §"Verdict on novelty", or the
   two `PROOFS.tex` passages that cite the key, on any claim checked here.
   The `PROOFS.tex` sentence "their Theorem~3 gives the effect as positive
   iff 2A(1-m^2) > P, with A = -u''/u' and P = -u'''/u''" matches p.35. The
   separator sentence that follows it is verified as ST-12a, and ST-12b
   shows it holds for every `m` in `(0, 2^(−1/2))` rather than only at
   `m = 1/2`.

## Not formalised, with reasons

- **ST-11** is a reading of prose. It is quoted above and has no check.
- **Theorem 2 itself** (Lemma 2, `F_11 − 2F_12 + F_22` equals (A.9)) is taken
  as given. Lean covers the algebra of Lemma 1's expansion, the Hessian form
  of footnote 8, and every step from (A.9) to (A.16). The derivation of
  Lemma 2 differentiates the composed equilibrium map, which none of our
  claims attributes beyond "a quadratic form in the Hessian".
- **The approximation "≃" in Lemma 1** is not formalised. Lean proves the
  algebra of the second-order Taylor polynomials (first-order terms cancel,
  and the sign of the change is the sign of the `t²` coefficient), not a
  remainder bound. "Local" in ST-6 rests on that algebra and on the source's
  own "small".
- **Analytic inputs** (the derivatives of `u` and of the power-logistic CSF)
  are hypotheses in Lean and are checked in SymPy (ST-AP, ST-2b, ST-2c,
  ST-2d, ST-10), not in Lean.
- **Equilibrium existence, uniqueness and stability** (Proposition 4,
  `|f_1 g_1| < 1`) are not formalised. The Lean sign theorems take the
  second-order condition `h_1 < 0` as a hypothesis.
- **Theorems 4, 5 and 6** (ability and rent-seeking spreads) and Example 1
  are not attributed by our documents and were not formalised.
