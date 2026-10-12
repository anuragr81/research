# Notes — Robson (1992), pass 1 (read in full)

Run `python3 lit/robson_1992/verify_robson.py` from the bundle root. It checks
the pinned sha256 of the scan and of Drive's OCR text, the first page, that all
twenty printed page numbers 838–857 are recovered in order, each quotation
verbatim on its page, two controls, and measures `lean/Lit/Robson1992.lean`
(build root, build, no `sorry`, every cited name declared, every declared
theorem audited, allowed axioms only, controls present).

## What was verified

Our reading of §2, of Definition 5, Lemma 2, the Corollary to Theorem 1 and the
core step of Lemma 3, and of the remark in the conclusions, is consistent with
the source:

- Status is rank with ties at one half, and total status is one half whatever
  the distribution (finite version, `total_status`, `mean_status_half`).
- Example A's second derivative and its sign: convex exactly above
  `w̃ = [(1 − α)/(kβ(β − 1))]^{1/(β−α)}`, derived in Lean from `HasDerivAt`, not
  by hand. Example A′'s `√(2/3)` follows once the coefficient `¾` is read from
  the page image (ROB-D1).
- The scaling identity `w̃_γ/γ = w̃·γ^{α/(β−α)}`, its logarithmic case and its
  rising case; with a fixed inflection (Friedman–Savage) the share falls.
- At a mass point a fair bet won with high probability gains (Lemma 2, limit).
- A concave reduced-form utility rejects all fair finite gambles.
- A fair status lottery beats an equal shared status when utility is strictly
  convex in status (Lemma 3's core).

## What could not be verified

- Theorem 1's converse, Theorems 2–4, Examples B and C, the Dutch-book argument
  on p.842, and the generalisation of the scaling result on pp.840–841.
- The concavity of `V` for the two Example A′ distributions, whose proofs the
  paper omits. Checked by hand only: with `V = ln w + ¾F(w)²`,
  `w²V″ = 8w²/3 − 16w³/3 + 16w⁴/9 − 1 < 0` on `(0, 3/2]` for
  `F = 4w/3 − 4w²/9`, and `w²V″ = 81w⁴ − 180w⁵ + 90w⁶ − 1 < 0` on `(0, 1]` for
  `F = 3w² − 2w³` (largest value about `−0.14`, near `w = 0.53`), so both claims
  hold on the logarithmic range. Not formalised; it is illustration, not
  evidence.
- The falling case `α < 0` of the scaling result.

## Discrepancies and readings

See ROB-D1 to ROB-D3 in `CLAIMS.md`. The OCR also loses the tilde on `w̃`,
renders `γ` as `y`, prints `€`/`E` for `∈`, and garbles every display; the
quotations avoid these.

## Findings for the firmworkers ledger

1. **The precedent is close.** Robson already locates the convex region of
   reduced-form utility through the wealth distribution rather than a free
   parameter: with logarithmic utility the convex range sits at fixed quantiles
   as wealth scales (`log_case_proportion_constant`). The author's wish that the
   reference come from socio-economic position rather than a parameter is
   therefore not new in itself. What Robson does not have, on this reading:
   status inherited and outside the market (his status is global wealth rank,
   CMP92's wealth-is-status case, which the decision of 11 Oct 2026 rejects for
   `h`); a reference specific to the worker's own group; a finite pool whose
   prizes are the entrants' positions; a costly, negative-sum lottery; and
   displaced workers as entrants. These are the candidate points of novelty for
   the reference-dependent sub-section, still to be checked against Becker,
   Murphy and Werning (2005), Ray and Robson (2012) and Friedman and Savage
   (1948).
2. **Equilibrium concept differs.** Robson's stability is that free fair
   gambles are rejected by individuals of measure zero (ROB-6, ROB-14), and his
   equilibria have no gambling (ROB-15). The firmworkers lottery is not fair
   (M12), its pool is finite, and an entrant's entry changes the prize set.
   Robson names a market clearing with positive gambling as an open extension
   (ROB-24).
3. **An economy-wide reference is inert in a closed economy.** Robson notes
   that with exogenous total wealth a reference at average wealth leaves the
   externality without effect (ROB-26). The firmworkers economy is closed and
   swaps preserve total income (`Isc.swap_preserves_total`,
   `total_fixed_under_reassignment`), so an economy-wide mean reference would be
   a constant, a parameter in disguise. A group norm moves when workers swap
   across groups (`control_total_fixed_needs_whole_population`), which is what
   gives `R_h` content. This supports the group norm decided on 12 Oct 2026.
4. **Friedman and Savage's class reading** (ROB-10, quoted by Robson) ties the
   convex region to a change of social class. That is the link to status groups
   `h`, and it must be checked in Friedman and Savage (1948) itself before any
   claim rests on it.

## Controls

| Check | Control |
|---|---|
| Quotations | ROB-C1, ROB-6 with "rejected" replaced by "accepted", absent; ROB-C2, ROB-16 looked up on p.842, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (ROB-6), a wrong page (ROB-16 on p.844), an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | seven `control_*` theorems: ties at full weight break total status; no jump, no gain; a convex `V` accepts a fair gamble; no status, no convex region; a fixed inflection's share falls with scale; a sub-population's total moves under reassignment; a linear `g` gains nothing from the status lottery |

## Relevance to the firmworkers ledger

Row L16. No model row rests on Robson yet. It bears on the reference-dependent
sub-section (decision of 12 Oct 2026) as its closest precedent and on the
choice of a group norm over an economy-wide reference.
