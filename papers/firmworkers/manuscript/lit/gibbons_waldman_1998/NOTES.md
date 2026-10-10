# Notes — Gibbons and Waldman (1999), pass 1 (working paper, read in full)

Run `python3 lit/gibbons_waldman_1998/verify_gibbons_waldman_1998.py` from the
bundle root. It checks the pinned sha256 of the scan and of its OCR text, the
title page, recovers the printed page numbers from the OCR (35 of 40), checks
each quotation verbatim in the OCR block that carries its page, runs two
controls, and measures `lean/Lit/GibbonsWaldman1998.lean` (build root, build,
no `sorry`, every cited name declared, every declared theorem audited, allowed
axioms only, controls present).

## Version and text

The source read is NBER WP 6454 (March 1998). The ledger cites the *QJE*
version (1999), under a different title; it was not supplied, so the row
carries `version_read` and is shown as unverified. The PDF is a scan with no
text layer and no OCR tool is installed locally, so the text is Drive's OCR of
the same file, pinned by its own sha256. Where a printed page number is missing
from the OCR (pp.1, 2, 4, 5, 9) the page block spans the neighbouring pages, so
the page check for quotations on those pages is coarser. Equations were read
from page images; the OCR garbles them. One OCR error met in a quotation
("Proposition I" for "Proposition 1", p.10) was avoided by quoting the clause
after it.

## What was verified

Our reading of Sections III–IV and of the appendix proofs of Propositions 1–2
and Corollaries 2.3–2.4 is consistent with the source:

- The wage is the maximum of the three job outputs, equal to the assigned
  job's.
- The wage is strictly increasing in effective ability, so there are no
  demotions or wage cuts under full information, and a demotion forces a wage
  cut.
- The raise at promotion splits as the paper says.
- The two lines of (A2) agree, the posterior is monotone and moves arbitrarily
  far, and beliefs are a martingale.
- The concavity step of Corollary 2.3 holds in discrete form, and wage
  decreases are impossible before `x*` and possible after it.

## What could not be verified

- Corollaries 2.1 and 2.2, and the stochastic-dominance half of Corollary 2.4.
- Every BGH statistic quoted in Sections II–V.

## Discrepancies

- GW-D1. Three different tie rules at a threshold (pp.8, 10, 31); harmless for
  wages (`tie_rule_reading`).
- GW-D2. `f'' ≤ 0` (p.8) against `f'' < 0` (p.29).

## Findings from the formalisation

1. The concavity step of Corollary 2.3 needs only discrete weak concavity,
   `f(x + 2) − f(x + 1) ≤ f(x + 1) − f(x)`, not differentiability
   (`ratio_increasing_of_concave`); with convex `f` it fails
   (`control_ratio_needs_concavity`).
2. The martingale property holds for any finite signal space with positive
   likelihood under `θ_H` (`beliefs_martingale`); normality is used only for the
   monotone likelihood ratio and the limits.

## Controls

| Check | Control |
|---|---|
| Quotations | GW-C1, GW-6 with "rises" replaced by "falls", is absent; GW-C2, GW-16 looked up in the p.10 block, is absent |
| Quotation suite | mutation-tested 10 Oct 2026; an altered word, a wrong page and an undeclared Lean name each fail; the first run itself caught a quotation placed on the wrong page (GW-14, p.24 not p.25) |
| Lean | four `control_*` theorems (thresholds, wage monotonicity, posterior, concavity) |

## Relevance to the firmworkers ledger

No model row rests on this paper yet. If the micro state is rank plus human
capital `(r, h)`, this is the closest precedent: effective ability is innate
ability times an increasing concave function of experience, jobs are a ladder
assigned by thresholds, and wage cuts arise only through learning. Whether the
firmworkers state takes this form is the author's decision (TODO.md).
