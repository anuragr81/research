# Jeffrey (1983), claims

Evidence tag [P]: page images of the preface and chapter 11 of the second
edition, read on 15 October 2026. The first edition (1965) is not read
(JF-D1). Pages are PDF pages of the scan. Quotations are checked against an
OCR of those pages with whitespace removed.

## Quotations

| ID | Page | Quotation | What we rely on |
|---|---|---|---|
| JF-Q1 | 8 | "The rest is as in the first edition, except for minor corrections" | The author's statement that chapter 11 carries over from the first edition, on which the attribution to Jeffrey (1965) rests |
| JF-Q2 | 176 | "but where the device of conditionalization cannot be applied because the change is not occasioned simply by learning of the truth of some proposition E." | The motivation for a rule for soft evidence, the role the predecessor gives it |
| JF-Q3 | 179 | "how should these changes be propagated over the rest of the structure of his beliefs?" | The problem the rule answers |
| JF-Q4 | 180 | "it did not change the conditional degrees of belief in any propositions on the evidence B or on the evidence" | Condition (11-1), the rigidity under which (11-2) holds, formalised as `kin_rigid` |
| JF-Q5 | 182 | "conditionalization is a limiting case of the present more general method" | Bayes' rule on learning a proposition is the limit of the rule. M2 and M3 show the two coincide for a single soft reading under group sufficiency |
| JF-Q6 | 183 | "Nor need it always be the case that the result of two applications of (11-2) is independent of the order" | Order effects across several readings, outside Paper A's single reading, formalised as `control_not_commutative` |
| JF-Q7 | 193 | "It is straightforward to verify that the present kinematical scheme is not generally commutative" | The same, from the notes |
| JF-Q8 | 192 | "This chapter stems from chapter 3" | The chapter's origin in the 1957 dissertation |

## Lean results

`lean/mathlib/Jeffrey1983.lean`, in the chapter's notation.

| Theorem | What it checks |
|---|---|
| `Jeffrey1983.kin_on_cell` | (11-7) at the level of outcomes rescales the old probabilities within each atom |
| `Jeffrey1983.kin_cellMass` | Each atom gets its new probability, `PROB A_i = q_i` |
| `Jeffrey1983.kin_rigid` | (11-8). Every conditional on an atom is unchanged |
| `Jeffrey1983.rigid_is_kin` | The converse. (11-8) at the level of outcomes with the new atom probabilities gives (11-7) |
| `Jeffrey1983.eq_11_5` | (11-5) from (11-2) and (11-3)(a) |
| `Jeffrey1983.mudrunner` | Examples 3 and 4 |
| `Jeffrey1983.order_BC` | Setting `PROB B = .9` then `PROB C = .2` gives `36/185` at the first outcome |
| `Jeffrey1983.order_CB` | The opposite order gives `9/20` |
| `Jeffrey1983.control_not_commutative` | Control. The two orders give different belief functions |
| `Jeffrey1983.control_rigidity_needed` | Control. A move of atom `B` to `.9` that does not rescale within the atom changes `PROB (C/B)` |

## Readings recorded

| ID | Reading | Reason |
|---|---|---|
| JF-D1 | The edition read is the second (1983), not the cited first (1965) | The preface says the chapter is as in the first edition except for minor corrections. Rows citing this record carry an unverified mark until the first edition is read |
| JF-D2 | The OCR sometimes reads the margin watermark into a line | Quotations stop short of it, for example JF-Q5 |
| JF-D3 | Pages are PDF pages | One page map covers the preface page and the chapter |
