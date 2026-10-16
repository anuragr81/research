# Rothschild and White (1993), reconstruction

Michael Rothschild and Lawrence J. White, "The University in the Marketplace:
Some Insights and Some Puzzles", in Charles T. Clotfelter and Michael
Rothschild (eds.), *Studies of Supply and Demand in Higher Education*,
University of Chicago Press for the NBER, 1993, pp. 11–42 (conference of
17–19 May 1991). The pages include Martin Feldstein's comment, pp. 37–42.

## Source read

- The NBER chapter PDF (www.nber.org/chapters/c6096), 33 PDF pages, cached at
  `~/.cache/paper_a/sources/rothschild_white_1993_chapter.pdf` (410,934
  bytes), sha256 `7e00c894…21bcf6aa`, with its text layer at
  `~/.cache/paper_a/sources/rothschild_white_1993_chapter.txt`, sha256
  `f54969e6…dc88a0b`. Both are pinned in full in the suite. The text
  has 34 form-feed segments. Segment `n − 1` is PDF page `n`. PDF page 1 is the
  NBER cover sheet, PDF page `n` from 2 to 33 is book page `n + 9`, and
  segment 33 is empty (RW-D2).
- Every page, 1 to 33, was read from the text on 16 October 2026, so the
  evidence tag is [F]. PDF pages 6, 8, 9 and 10, where the extraction garbles
  the symbols of the stand-alone test, of `B(A)`, of eq. (1) and of the
  diversity example, were also read from 110 dpi images rendered with
  `pdftoppm`. Nothing beyond these pages is attributed.
- This is the chapter, not the 1995 *Journal of Political Economy* article on
  customers as inputs (RW-D1). PDF pp. 28–33 are Feldstein's comment (RW-D3).

## The chapter, from its own primitives

1. **Introduction** (PDF pp. 2–4). Universities compete for inputs, position
   themselves, set prices and decide entry, and the literature has ignored
   this market context. The summary of section 1.3 states that incoming
   students are an input to the university's output and that admission and
   pricing reflect this (RW-Q1, RW-Q4).
2. **Cross-subsidy** (PDF pp. 4–7). Faulhaber's stand-alone test says there is no
   cross-subsidy if for every subset `T` of customers `C(y^T) ≥ r(y^T)`. Since
   joint producers of undergraduate and graduate education survive alongside
   colleges that produce only undergraduate education, undergraduates do not
   subsidise graduate education. Economies of scope are named as the reason.
3. **The general problem** (PDF pp. 7–9). Students `S`, colleges `C`, an
   allocation `A : S → C`. The net benefit `W_A[A(s)]` depends on the whole
   allocation. `B(A)` is the sum of benefits and `A` is efficient if it
   maximises `B`. Allocations differ in efficiency only if students are inputs,
   which creates externalities (RW-Q2, RW-Q3). Matching theory (Gale–Shapley,
   Roth, Kelso–Crawford) gives stability only without compositional
   preferences (RW-Q12, RW-Q15).
4. **Eq. (1)** (PDF p. 9). `p_A(c, s) = x_s` if `A(s) = c` and `∞` otherwise
   implements any `A`. Denial of admission is an infinite price (RW-Q5). Such a
   price system is not competitive (RW-Q6).
5. **Homogeneity** (PDF pp. 9–10). Supposing the benefit rises with the mean
   ability of a college's students and falls with its variance, efficiency
   groups similar abilities (RW-Q7). A Spence-type price system can segregate
   abilities (RW-Q8). The authors name two problems, liquidity or wealth (RW-Q14), and possible
   non-existence of equilibrium when colleges compete for a mix, as in
   Rothschild and Stiglitz (1976) (RW-Q9).
6. **Diversity** (PDF pp. 10–11). Four students, two colleges of capacity two.
   `x₁, x₂` bring the random input `x̃`, `y₁, y₂` bring `ỹ`, independent and
   identically distributed. Students share their college's output equally and
   are risk averse. The optimum mixes the kinds, and an anonymous price system
   cannot achieve it (RW-Q10, RW-Q11). With many students the law of large
   numbers removes the problem.
7. **Markets and competition** (PDF pp. 11–25). Non-profit status and
   non-tuition funds weaken market discipline. The puzzles are the rewards to teaching,
   limited price discrimination, uniform tuition across fields, low prices at
   prestigious schools (Tables 1.1–1.3, with regressions of tuition on
   expected salary), entry and exit (Tables 1.4–1.6). On collusion over
   scholarships the authors find the case for a market failure weak, because
   the externality of desirable students is internalised in tuition (RW-Q13).
8. **Conclusion** (PDF pp. 25–26).

## Derivation chain of what A1 would use

- Eq. (1) gives every student a finite price at exactly one college
  (`pA_finite_iff`). An anonymous price cannot separate two students with the
  same benefits (`anonymous_same_ranking`), while a personalised one can
  (`control_personalised_separates`).
- With the linear special case `mean − k · variance`, `B(A)` is total ability
  less `k/2` times the within-college squared gaps (`totalBenefit_eq`), so
  sorting is efficient (`sorted_pairing_efficient`, `sorted_pairing_strict`)
  and with `k = 0` all allocations tie (`control_no_synergy`).
- In the diversity example a student's share has variance `σ²` in a
  homogeneous college and `σ²/2` in a mixed one (`homogeneous_share_var`,
  `mixed_share_var`, `mixed_better`). The gain disappears with perfectly
  correlated inputs (`control_perfect_correlation`).

## What could not be reconstructed

- The chapter's claims in section 1.3 are verbal. The general monotone benefit
  of section 1.3.2 is formalised only in the linear special case, and the
  claim that anonymous prices cannot achieve the diverse allocation is
  formalised only as the ranking lemma, without capacities or the dependence
  of benefits on the allocation.
- The Spence-type segregating price system (RW-Q8) and the non-existence
  result (RW-Q9) are cited, not derived, in the chapter.
- Sections 1.2 and 1.4 are not formalised. A1 does not rely on them.
