# Claims about Cole, Mailath & Postlewaite

**Papers.** CMP92: *JPE* 100(6), Dec. 1992, pp. 1092–1125 **[F, published]**.
CMP95: CARESS Working Paper #95-14 **[F, WP]**.

## Claims as stated in `LITERATURE.tex` §sec:foundations, the Verdict, and the handover

| ID | Claim | Source locator | Relevance claimed to `PROOFS.tex` |
|---|---|---|---|
| CMP-A | They justify a **rank-allocated non-market prize**; cite them where `V` is introduced. | CMP92 abstract, §II; CMP95 §4 | `PROOFS.tex` §Primitives, at the introduction of `V` |
| CMP-B | Status is "a ranking device that determines how well he or she fares in the nonmarket sector"; the existence of a nonmarket sector **endogenously generates** a concern for relative position. | CMP92 abstract | Foundation for `V` being rank-allocated rather than purchased |
| CMP-C | **CMP95 §4**: "welfare theorems do not apply to prizes allocated by rank". | CMP95 §4, *Concluding Comments* | **Welfare section** (deferred) — handover §5 item 1 |
| CMP-D | CMP92 yields multiple equilibria, so growth differences need no differences in preferences, technology or endowments. | CMP92 abstract, §IV | Context only |

## What is checkable here

Very little, and that is the honest position. CMP-A, CMP-B and CMP-D are
textual and were confirmed **by reading**, not by computation; the quotations
are in `RECONSTRUCTION.md` §1–2.

What the suite does check is the *use* we make of them:

- **CMP-1/CMP-2** — the rank-allocation premise (one fixed prize, delivered to
  the top-ranked competitor) reproduces **Levin–Smith equation (8)** exactly.
  This turns CMP-C from a conceptual assertion into the welfare function the
  deferred section will actually use.
- **CMP-3** — the private/social wedge CMP95 §4 points at, stated
  arithmetically; it is the same wedge as LS-7's business stealing.
- **CMP-4** — `V` is a pure scale factor in our model (`Δ = V·E[φ]`,
  homogeneous of degree one), which is *why* their justification can be
  imported without their matching machinery.

## Caveat on CMP-C

CMP95 §4 is **Concluding Comments** — a discussion section. The statement is an
authoritative conceptual claim by the authors, **not a theorem**. It should be
cited as such. See `NOTES.md`.
