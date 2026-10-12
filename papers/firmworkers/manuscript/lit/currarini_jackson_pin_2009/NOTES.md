# Notes — Currarini, Jackson and Pin (2009), pass 1 (working paper read in full)

Run `python3 lit/currarini_jackson_pin_2009/verify_cjp.py` from the bundle
root. It checks the pinned sha256 of the PDF, the first page, that every page
ends with its number, each quotation verbatim on its page, two controls, and
measures `lean/Lit/CurrariniJacksonPin2009.lean` (build root, build, no
`sorry`, every cited name declared, every declared theorem audited, allowed
axioms only, controls present).

## What was verified

Our reading of the homophily indices, identity (6), Proposition 2, the §4.5
example and the welfare proportionality of Proposition 7 is consistent with the
source.

## What could not be verified

Propositions 3–6 and 8–10, the matching foundation in Appendix A, the
estimation and the welfare figures. The Econometrica version was not read.

## Discrepancies and readings

CJP-D1 (a garbled display on p.18) and CJP-D2 (the closure link is ours) in
`CLAIMS.md`.

## Findings for the firmworkers ledger

1. **The closure parameter is a known index in disguise.** Under the
   firmworkers closure weighting (own group 1, other group `1 − σ`), the share of
   own-group partners is `H = n/(n + (1 − σ)m)` and the baseline is
   `w = n/(n + m)`, and `σ = IH/H` exactly, where `IH` is Coleman's inbreeding
   homophily as used by CJP (`closure_is_inbreeding_over_homophily`). The
   identification of `σ` in X25 (`Isc.closure_from_crossShare`) is therefore a
   transformation of an established measure, not a new one; the measurement map
   should cite it so.
2. **Closure as a bias in meetings, not in preferences.** CJP separate two
   channels: type-sensitive preferences, which explain friend counts, and a bias
   in meetings toward one's own type, which is needed for inbreeding in all
   groups (CJP-5, CJP-16, CJP-19). The firmworkers `σ` acts on partner draws
   (M29), that is, on meetings; the reference `R_h` of the sub-section acts on
   preferences. The firmworkers split of a feasibility channel and a preference
   channel is the same split CJP make for friendships. That supports the
   decision of 11 Oct 2026 and narrows the novelty claim: the separation itself is
   CJP's; what is new is its use for a costly lottery over job positions.
3. **Identification needs meeting data.** CJP say that separating channels
   requires information about the meeting process (CJP-20, CJP-23). For X25 that
   means the observation must be of partners met through the lottery route, not
   of all ties, which matches the observation already proposed for X25.
4. **Types include inherited identities** (CJP-12), and segregation arises
   without anyone being rejected (CJP-13), which is how the firmworkers closure
   works: nobody refuses a cross-group swap; cross-group draws are just rarer.

## Controls

| Check | Control |
|---|---|
| Quotations | CJP-C1, CJP-15 with "heterophilous" replaced by "homophilous", absent; CJP-C2, CJP-9 looked up on p.6, absent |
| Quotation suite | mutation-tested 12 Oct 2026: an altered word (CJP-13), a wrong page (CJP-9 on p.6), an undeclared Lean name and a wrong sha256 pin each fail |
| Lean | two `control_*` theorems: with three types two groups can inbreed at once while the shares still balance; with no other-group partners the index is zero whatever `σ` |

## Relevance to the firmworkers ledger

Row L24. No model row rests on CJP yet. It bears on M29 and X25, and on the
novelty of separating the feasibility and preference channels.
