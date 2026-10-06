# Claims about Cole, Mailath & Postlewaite

**Papers.** CMP92: *JPE* 100(6), Dec. 1992, pp. 1092–1125 **[F, published]**,
read from the JSTOR copy (`cole_status_value.pdf`). CMP95: CARESS Working Paper
#95-14, 1 August 1995, "Forthcoming, Quarterly Review, Federal Reserve Bank of
Minneapolis" **[F, WP]** (`colemaliath_incorporating_concern.pdf`). CMP95 page
numbers below are the working paper's printed pages, not the *QR* pages.

## Claims as stated in `LITERATURE.tex` §sec:foundations, `PROOFS.tex`, and the handover

| ID | Claim | Where we make it | Source locator | How checked |
|---|---|---|---|---|
| CMP-A | They justify a **rank-allocated non-market prize**; cite them where `V` is introduced. | `LITERATURE.tex` l.1130; `PROOFS.tex` l.361–371 | CMP92 abstract p.1092, §II p.1096; CMP95 §2.2 p.8, §4 pp.14–15 | Quotation; the rank half is formalised under CMP-G1, CMP-G2 |
| CMP-B | Status is "a ranking device that determines how well he or she fares in the nonmarket sector". | `PROOFS.tex` l.362–364; `LITERATURE.tex` l.851–852 | CMP92 abstract p.1092 | Quotation only |
| CMP-C | When desirable goods "are allocated as prizes rather than sold, the standard welfare theorems regarding the Pareto optimality of the outcomes no longer apply"; a remark, not a theorem. | `PROOFS.tex` l.364–368; `LITERATURE.tex` l.882–883 | CMP95 §4 p.15 | Quotation only (a remark in Concluding Comments) |
| CMP-D | CMP92 yields multiple equilibria, so growth differences need no differences in preferences, technology or endowments. | `LITERATURE.tex` (context) | CMP92 abstract p.1092 | Unchecked, context only |
| CMP-E1 | The §IV.A two-point economy, `K − ε` on `[0, ½)` and `K + ε` on `[½, 1]`, keeps the average capital and makes the distribution unequal. | Set-up behind `PROOFS.tex` l.1620–1621 | CMP92 §IV.A p.1102 | Lean |
| CMP-E2 | Within one economy, matching raises savings above the undistorted level, `∂λ/∂j < 0`, and a lower `V(½)` forces a lower `λ(j)`. | `LITERATURE.tex` l.856–857 (Property 2 analogue) | CMP92 §IV.A pp.1101–1103 | Lean, analytic steps as hypotheses |
| CMP-E3 | CMP92's "§IV.A example has the more compact income distribution producing the higher savings rate", the origin of the received result that "inequality dampens status competition". | `PROOFS.tex` l.1172–1173, l.1618–1621; `LITERATURE.tex` l.861–864 | CMP92 §IV.A p.1103 | Lean, analytic steps as hypotheses, two controls |
| CMP-E4 | The same comparison at `γ = 2`, exact rationals: compact economy saves 3/4, dispersed economy 1/2, at the same initial income and the same match. | Numerical witness for CMP-E3 | CMP92 §IV.A eqs. (2), p.1101–1103 displays | Lean (`decide`), SymPy CMP-5 |
| CMP-F | "that finding is recovered as the mass-competition (below-pivot) case"; "mass competitions, in which the marginal entrant sits low in the distribution". | `PROOFS.tex` l.1175–1176, l.1624–1625 | CMP92 §IV.A p.1103 (structure of the comparison) | Lean for the displacement facts; the mapping itself is a finding, see `NOTES.md` |
| CMP-G1 | Matching is by rank: the wealthiest man gets the woman of highest endowment, a man's match depends only on his relative position, and higher capital yields weakly better status. | `PROOFS.tex` l.361 ("allocated by rank rather than purchased") | CMP92 §IV.A p.1101 | Lean |
| CMP-G2 | `m(y)` is the distribution function of output, so `m′` is large when the distribution is tight and competition is intense. | `LITERATURE.tex` l.875–877 | CMP95 §2 p.4, p.5; eq. (4) p.6; §2.1 (2.5)–(2.8) p.6, p.7–8 | Lean |
| CMP-H | The bottom agent is undistorted; it is competition from below that distorts. | `LITERATURE.tex` l.856–857, l.879–881 | CMP92 p.1101 and n.11 p.1102; CMP95 n.8 pp.5–6 and §2.1 p.7 | Lean for the rank fact; quotation for the rest |
| CMP-I | Property 1: "no family's relative rank ever changes". | `LITERATURE.tex` l.855–856, l.893–894 | CMP92 Property 1 p.1107; proof Appendix B p.1122 | Lean (exchange argument), one control |
| CMP-K | Other paraphrases in `LITERATURE.tex` §sec:foundations (Proposition 1, Proposition 2, n.20, §V.D, CMP95 §3, instrumental concern). | `LITERATURE.tex` l.849–883 | listed in `NOTES.md` | Quotation only |

## What is checkable here, and how

- **Lean** (`ColeMailathPostlewaite.lean`, 41 theorems): CMP-E1 to CMP-E4,
  CMP-F (displacement facts only), CMP-G1, CMP-G2, CMP-H (rank fact), CMP-I.
  Each theorem's page and quoted text are tabulated in `NOTES.md`.
- **Quotation only**: CMP-B, CMP-C, CMP-K, the non-rank half of CMP-A and of
  CMP-H. These are prose in the sources, so a Lean statement would add nothing
  that the quotation does not already fix.
- **SymPy / numeric** in `verify_cmp.py`: CMP-1 to CMP-4 check the *use* we
  make of the papers (Levin–Smith eq. (8), the private/social wedge, `V` as a
  scale factor). CMP-5 re-derives the CMP-E4 instance. CMP-6 and CMP-7 test,
  across a parameter grid, the step the paper leaves unstated and the framing
  under which CMP-E3 holds.

## Caveat on CMP-C

CMP95 §4 is **Concluding Comments**, a discussion section. The welfare
sentence is an authoritative conceptual claim by the authors and **not a
theorem**. `PROOFS.tex` l.367 already calls it "a remark in their concluding
discussion rather than a theorem".
