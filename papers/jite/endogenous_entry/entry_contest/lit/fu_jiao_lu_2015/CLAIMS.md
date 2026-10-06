# Claims about Fu, Jiao & Lu (2015)

**Paper.** Qiang Fu, Qian Jiao, Jingfeng Lu, "Contests with endogenous entry",
*International Journal of Game Theory* 44 (2015), pp. 387–424.
DOI 10.1007/s00182-014-0435-9. Evidence level **[F]**, published version.

This is the directory that bears on the **P7 attribution question**.

## Claims as stated in `LITERATURE.tex` §sec:entry

| ID | Claim | Source locator |
|---|---|---|
| FJL-A | `M >= 2` identical risk-neutral potential bidders, common fixed entry cost, generalised nested lottery contest, prize purse capped at `V` and allowed to be contingent on the realised number of entrants. | Model section |
| FJL-B | **Equation (2)** is a rent-accounting inequality: `[1-(1-q)^M] V >= Mq(Delta + E[x^alpha])`. | p.397, eq. (2) |
| FJL-C | It **implies that the expected number of entrants `Mq` cannot exceed `V/Delta`**. | Survey's inference from (2) |
| FJL-D | "That is the same accounting logic as P7. P7's content is therefore not a new inequality but the deterministic, heterogeneous, utility-unit version of a known one: it bounds a pure-strategy count rather than an expected count, it uses `kappa` in utility units rather than a nominal cost, and it holds under fixed rules with no designer. **The paper should say exactly that.**" | Survey's positioning instruction |
| FJL-E | They take the mixed route and, because entrants do not observe the number of rivals, need Dasgupta–Maskin for two-dimensional discontinuous games (Theorem 1). | Theorem 1 |
| FJL-F | The design results built on top — uniform bound on expected overall bids (Theorem 2), single-peakedness in `q`, implementation by Tullock contests with entry fees (Theorem 7), optimal shortlisting (Lemma 6, Theorem 9, with `M-bar = min{N : V/N < alpha*Delta/(alpha-1)}`) — have no counterpart in the entry_contest model. | Theorems 2, 7, 9; Lemma 6 |
| FJL-G | Their Theorem 11 (optimal entry probability, bid bound and shortlist size all fall in the entry cost) is the design-side cousin of P8. | Theorem 11 |

## What is checkable here

- **FJL-B** confirmed verbatim against p.397.
- **FJL-C** is the load-bearing inference and is checked in two steps: the
  bracket is a probability (FJL-1) and the effort term is signed (FJL-2).
  Whether FJL *state* this bound themselves is examined in `NOTES.md` — the
  answer changes how the attribution should be worded.
- **FJL-D** is the positioning claim. Its formal content — "the same
  accounting logic" — is made precise in `Accounting.lean`: one abstract
  lemma, with both FJL's expected-count bound and P7's cap derived from it as
  instances, plus a theorem isolating what P7 has that the shared lemma does
  not.
- **FJL-F**'s shortlist cutoff is checked for well-definedness (FJL-4).
- **FJL-A, FJL-E, FJL-G** are structural readings; not arithmetic, unchecked.
