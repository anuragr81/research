# Jeffrey (1983), chapter 11, reconstruction

Richard C. Jeffrey, *The Logic of Decision*, second edition, University of
Chicago Press, 1983. Chapter 11, "Probability Kinematics", pp. 164–183, and
the preface, p. xii.

## Source read

- The Drive file `the_logic_of_decision.pdf` (Drive id
  `11VXq9EGl6KjDP0Csy8e5trLX49KjM8Ou`, 17,333,214 bytes, folder `temp`), a
  242-page scan of the second-edition paperback (back cover, ISBN
  0-226-39582-0, "Richard Jeffrey added much new material for the second
  edition in 1983"). Cached at `~/.cache/paper_a/jeffrey_logic_of_decision.pdf`,
  sha256 `6fbdb1d1…b0e1`. No text layer.
- **Not the cited first edition** (1965). The preface, p. xii, says the
  second edition replaced chapter 9 and added section 1.7 and the Appendix,
  redid sections 7.5 and 7.6, and that "The rest is as in the first edition,
  except for minor corrections" and new notes. Chapter 11's own notes say it
  "stems from chapter 3" of Jeffrey's 1957 dissertation. On that statement of
  the author's, chapter 11 is taken to carry the first edition's kinematics,
  but the first edition is not read (JF-D1).
- Read from page images [P] on 15 October 2026, the contents (PDF pp. 4–6),
  the preface (PDF pp. 7–9) and every page of chapter 11 (PDF pp. 175–194,
  book pp. 164–183) at 110 dpi. Nothing beyond those pages is attributed.
  PDF page `n` is book page `n − 11` in chapter 11.
- OCR of PDF page 8 and PDF pages 175–194 by `lit/ocr_pages.py`, cached at
  `~/.cache/paper_a/jeffrey_1983_read.txt`, sha256 `b0ecda7d…ccb5`. The scan
  carries a watermark in the right margin, which the OCR sometimes reads
  into a line (JF-D2).

## The chapter, from its own primitives

1. **Conditionalization and its limits** (pp. 164–166). Learning that `E` is
   true moves `prob` to `prob_E`. Some changes, such as observation by
   candlelight, change the probabilities of `G`, `B`, `V` without any
   proposition `E` being learned.
2. **The problem** (pp. 166–168). Given new values `PROB B_1, …, PROB B_n`,
   how should the change propagate to every other proposition?
3. **`n = 2`** (pp. 168–170). If the change originates in `B`, so that
   `PROB(A/B) = prob(A/B)` and `PROB(A/B̄) = prob(A/B̄)` (11-1), then
   `PROB A = prob(A/B) PROB B + prob(A/B̄) PROB B̄` (11-2). The mudrunner
   examples give `.52` and `.31`.
4. **Relevance** (pp. 170–171). `PROB A − prob A = (PROB B − prob B) rel(A/B)`
   with `rel(A/B) = prob(A/B) − prob(A/B̄)` (11-4, 11-5).
5. **Comparison with conditionalization** (pp. 171–172). Conditionalization is
   the limit `PROB B → 1`. Unlike it, (11-2) is reversible.
6. **Finite `n`** (pp. 172–175). For the atoms `A_1, …, A_m` of the set
   (11-6), `PROB A = Σ_i prob(A/A_i) PROB A_i` (11-7), and the change
   originates in the set when `PROB(A/A_i) = prob(A/A_i)` for every `i`
   (11-8). The two are equivalent when no `prob A_i` is 0 or 1. Two
   applications of (11-2) need not commute.
7. **The continuous case, probabilistic acts, observation and meaning**
   (pp. 175–180).
8. **Notes** (pp. 180–183). The chapter stems from the 1957 dissertation.
   The scheme is not generally commutative, with commutation when the two
   partitions are independent under the initial function.

## Derivation chain of what Paper A uses

- (11-7) at the level of outcomes rescales the old probabilities within each
  atom (`kin`, `kin_on_cell`), so each atom gets its new probability
  (`kin_cellMass`) and every conditional on an atom is unchanged, (11-8)
  (`kin_rigid`). Conversely, a belief function with (11-8) at the level of
  outcomes and the new atom probabilities is the one of (11-7)
  (`rigid_is_kin`).
- (11-5) is (11-2) minus (11-3)(a) (`eq_11_5`), and the mudrunner numbers
  check (`mudrunner`).
- Non-commutativity is exhibited on four outcomes (`order_BC`, `order_CB`,
  `control_not_commutative`).

## What could not be reconstructed

- The continuous case of section 11.8 is described by approximation and is
  not formalised. Paper A's class space is finite.
- Sections 11.9 and 11.10 are philosophical and not formalised.
