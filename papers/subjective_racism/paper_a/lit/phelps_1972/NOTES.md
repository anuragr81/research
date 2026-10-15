# Phelps (1972), notes

## What was verified

- Six quotations are found on the pages they cite in the OCR text, compared
  with whitespace removed, since OCR drops some spaces (LC-2). This is
  weaker than a check against a publisher's text layer.
- Eqs. (2), (5) and (5′) and Cases 1 and 2 are Lean theorems in the paper's
  notation (LC-4).
- Our reading is consistent with the source on these points.

## Page images read

| PDF page | Journal page | sha256 of the 150 dpi image |
|---|---|---|
| 1 | 659 | `11cf7bb7c75f8b581aa1d48d7a66e2128acc1b0c15e5b110d828dad345269318` |
| 2 | 660 | `3468848c581fe3af887bac81adbbc872265d024de6ee5f376d7822ef7dbcb423` |
| 3 | 661 | `bc84bad35317874ccad79681e9889d9d85eacf1dfa4ee94c7e3e0c530a57e59e` |
| 4 to 7 | blank | `7a0f2a24d31bc5d30938c9d83bcb0be50782a3342a9336740824c79c2424d3c9` |

## The parallel with Paper A, and where it ends

1. **Same.** The group is a proxy for what is not observed about the
   individual (PH-Q1), as Paper A's group prior is (M1 to M3).
2. **Different evidence.** Phelps's employer sees a test score that is
   evidence about the individual (PH-Q4). Paper A's marker is evidence about
   the group only (group sufficiency, M1). In Phelps the individual's own
   evidence moves the prediction of the individual. In Paper A nothing does.
3. **No dynamics.** The model is static. The group's parameters are given,
   and the paper does not model their revision. It names persistence in a
   parenthesis (PH-Q3) and does not derive it. The predecessor's claim that
   in this benchmark "counter-examples erode the stereotype" is not in the
   paper.

## Controls

- LC-2. A fabricated quotation, "the employer revises his belief about the
  group after each applicant", is not found. A true quotation is not found
  on a neighbouring page.
- Lean. `control_no_crossing_equal_variance` shows Case 2 needs the variance
  difference, and `control_weight_below_one` that the test never carries
  the whole weight.
