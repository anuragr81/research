# Traceability pass — artifacts against the survey's relevance claims and `PROOFS.tex`

Date: 1 September 2026. Covers all twelve paper directories (70 checks, all
green at the time of writing; run `./verify_lit.sh` to confirm).

**What this pass is.** Each artifact establishes something specific. The
survey (`LITERATURE.tex`) makes a *relevance claim* about each paper — "this
bears on P7", "this is the precedent for P9-gen" — and `PROOFS.tex` states the
corresponding claim at some strength (or does not state it). This document
walks the three layers against each other and issues one verdict per thread.

**Verdict vocabulary.**

| Verdict | Meaning |
|---|---|
| ALIGNED | Artifact, survey and `PROOFS.tex` agree; nothing to do. |
| ALIGNED-TIGHTEN | Substantively aligned; one phrase or qualification should change. |
| SURVEY-ERROR | The artifact contradicts the survey's wording; the survey (and/or handover) needs correction before the claim propagates. |
| UNSTATED | The artifact supports a positioning claim that `PROOFS.tex` does not yet make. The evidence is ready; the sentence is missing. |
| LEAD-NOT-RESULT | The artifact sharpens an open research lead; nothing may be claimed in any document. |

This pass **audits**; it does not edit `PROOFS.tex` or `LITERATURE.tex`. The
consolidated action lists are at the end.

Note on `CLAIMS.md` backfill: the first five paper directories (Schroyen–
Treich, Hopkins–Kornienko, Fu–Jiao–Lu, Fu–Lu, Levin–Smith) predate the
relevance-to-`PROOFS.tex` column added from Costrell–Loury onward. Rather than
edit five files, this document is the canonical relevance mapping for all
twelve; the per-paper `CLAIMS.md` files remain the canonical claim inventories.

---

## Verdict table

| # | Thread | Artifact evidence | Survey relevance claim | `PROOFS.tex` state | Verdict |
|---|---|---|---|---|---|
| 1 | P7 attribution (FJL eq. 2 / Def. 1; Fu–Lu eq. 7) | `Accounting.lean` 4 thms; FJL-1..4; FL-1..4 | "Same accounting logic; the paper should say exactly that" | §P7 carries **no citation** | **UNSTATED** |
| 2 | FJL citation target | FJL never display `Mq ≤ V/Δ`; it is Definition 2's feasibility set | Survey cites eq. (2) alone for the count bound | n/a (survey-level) | **ALIGNED-TIGHTEN** |
| 3 | P9-gen ↔ dispersive order (HK) | `Dispersive.lean` 7 thms: HK-L1 (MonotoneT free), HK-L2 (bridge), HK-L4 (crossing necessary) | "Should be checked formally and then stated" | Checked ✔; **stated nowhere** | **UNSTATED** |
| 4 | P9-gen ↔ Costrell–Loury pivot | CL-1..3 wage algebra; pivot rule confirmed verbatim | "P9-gen should be presented as the entry-count analogue" | §P9gen carries **no citation** | **UNSTATED** (with the §7 disanalogy attached) |
| 5 | Open item 2 route (CL Prop 5 + Suen Prop 2) | CL-4/5 (monotonicity, no log-concavity); SU-3 (log-concavity ⇔ ρ′≤0); SU-5 (rescaling trap) | "Same technique … log-concavity of 1−Λ"; "two independent sources" | Open item 2 text predates findings | **SURVEY-ERROR** ×2 + **LEAD-NOT-RESULT** |
| 6 | P-MU ↔ Ryvkin–Drugov | RD-4 (kernels identical), RD-5 (log-supermodularity transfers), RD-6 (crossing reversed) | Verdict: "discrete analogue of the **hazard-rate** result" | §PMU carries **no citation** | **SURVEY-ERROR** (label) + **UNSTATED** (kernel identity) + **LEAD-NOT-RESULT** (unimodality) |
| 7 | Contribution vs Schroyen–Treich | ST-12a: same `A`, different `P`, opposite signs | "No third derivative … global not local" — contribution sentence | Sentence **not in `PROOFS.tex`** (open item 4 stale) | **UNSTATED** |
| 8 | Contribution vs Lazear–Rosen | LR-1..3: channels decisively separated; "example, not theorem"; choice-object difference | "1981 precedent; must acknowledge; u″ here, u‴ there" | No acknowledgement anywhere in `PROOFS.tex` | **UNSTATED** + **ALIGNED-TIGHTEN** (precedent is an example the authors decline to generalise; add the choice-object difference) |
| 9 | P5 ↔ Moreno–Wooders | MW-1..4: binomial vs deterministic; flat vs rank-dependent threshold | "Complete information separates P5 from a private-cost threshold" | §Primitives states it, cites MW ✔ | **ALIGNED-TIGHTEN** ("identities are not determined" → "the entering set is random") |
| 10 | Identity problem, three devices | LS (fn. 2 area), FL (sequential), MOS Prop 1 + MOS-6 (C(6,k)=15 vs 1) | "P5 is the fourth device, identity a property of the agents" | §Primitives states it with all three citations ✔ | **ALIGNED** (optionally add MOS-6's count and the fn.2-is-main-text locator fix) |
| 11 | Welfare frame (LS Prop 3/6/8/9, eq. 8/9/18) | LS-1..7 incl. LS-7 branch criterion; CMP-1..3; MW-5 | "Prop 3 is the frame; expected verdict excessive entry" | Welfare section deferred — nothing written | **SURVEY-ERROR** (eq. 9 role; Props 8–9 attribution) + inputs staged |
| 12 | `V` foundation (CMP 92/95) | CMP-1..4; quotes confirmed verbatim; §4 is prose, not a theorem | "Cite CMP where V is introduced" | `V` introduced **without citation** | **UNSTATED** + **ALIGNED-TIGHTEN** (cite §4 as motivation, not result) |
| 13 | MOS experiment figures | MOS-3/4 table reproduced; MOS-5 opposite-direction gaps | "Observed was 2.5 and 3.7" | n/a (survey-level) | **ALIGNED-TIGHTEN** (second-half figures; directions differ) |
| 14 | Suen cross-side vs same-side | SU-A/SU-E confirmed | "Map class identical, object compared differs" | n/a | **ALIGNED** |
| 15 | S11 provenance | RD-4: S11's hump at (Q−1)/Q **is** RD's kernel at (k−1)/k | Survey names `b_k` as the comparator | §PMU derives the hump with no provenance note | **UNSTATED** (one sentence) |

**Status 6 Oct 2026.** The table records the pass-1 state, and four rows have
moved since then.

- Row 5 is resolved in `LITERATURE.tex`. The survey says the two sources are
  not independent, and the route was not needed, since `PROOFS.tex` closes
  open item 2 through the tail condition.
- Row 6 changed direction. RD-6's "crossing reversed" dropped the leading minus
  sign of the P-MU identity, and the orientation matches RD's (pass-2 finding
  F1 in `ryvkin_drugov_2020/NOTES.md`). `LITERATURE.tex` is corrected, and
  `PROOFS.tex` is listed for the author in `TODO.md` item L1.
- Row 9's tightening is applied in `PROOFS.tex`.
- Row 11's welfare inputs gained two conditions. The Moreno–Wooders optimum is
  constrained to symmetric independent entry rules, and Levin–Smith's
  excessive entry needs entrants beyond the first to keep positive expected
  rent. Both are in `LITERATURE.tex` §sec:entry.

---

## Detail on the non-trivial verdicts

### 1–2. P7 (threads 1–2)

What the artifacts establish: one abstract lemma (`accounting_bound`) with
FJL's expected-count bound and P7's cap as instances; the residual term is the
formal statement of what P7 drops (no bidding stage); `p7_count_is_integral`
is the formal residue of "deterministic version of a known one". Fu–Lu's
eq. (7) is the sharper case (an equality, so the cap is exact) but their `N`
is an optimally *designed* count — "fixed rules with no designer" is doing
real work.

`PROOFS.tex` §P7 currently: no citation, and the prose "Stronger than the
original numerical claim" reads as a novelty claim. The survey's instruction —
"present it as the deterministic, heterogeneous, utility-unit version of a
known inequality" — is fully evidenced and simply not executed.

Tighten at the survey level: the count bound is FJL **Definition 2's
feasibility set**, not a displayed corollary of eq. (2); cite Definition 2
alongside eq. (2) or a referee will find only the bid chain (3)–(6).

### 5. Open item 2 (thread 5)

Two survey errors, both load-bearing for the proposed route:

- **"using the same technique … a hazard-rate or log-concavity condition"** —
  false for Costrell–Loury. Their Prop 5 needs only `β` **non-decreasing**;
  the words "log-concav" and "hazard" do not occur in their paper. The
  three-part technique is Suen's alone.
- **"Two independent published sources"** — false. Suen's proof imports the
  quantile-reversal step from Costrell–Loury by citation ("see, for example,
  [2]"). One lineage, not two.

And one obstacle the survey does not record: **Suen's footnote 1** declines
rank-rescaling because concavity does not survive it unless the density is
increasing (SU-5 exhibits `φ̂″ = +1.12` from a concave `φ`). The proposed
composition `κ∘Λ⁻¹` is exactly such a rescaling. Together with the
aggregate/count gap and cross-side/same-side, three obstacles stand between
the plan and a result. The survey's closing line ("should not be claimed until
tried") remains exactly right — the corrected plan is: **try Costrell–Loury's
weaker hypothesis (monotone weight) first**, not log-concavity.

### 6. P-MU (thread 6)

Three distinct layers, one per verdict:

- **SURVEY-ERROR.** The Verdict calls P-MU "the discrete analogue of the
  Ryvkin–Drugov hazard-rate result". The hazard rate governs their *aggregate*
  object `E(h(X_{(k−1:k)}))`; the comparator for `Δ(0)` is the *density*
  object `b_k = E[f(X_{(k−1:k−1)})]` (the survey's own body text says so; the
  Verdict contradicts it; RD-7 separates the two by the factor `k(1−F)`).
- **UNSTATED.** S11's hump-shaped weight peaking at `(Q−1)/Q` **is** RD's
  log-supermodular kernel peaking at `(k−1)/k` (RD-4, exact under `G↔F, Q↔k`).
  One provenance sentence in §PMU converts an apparent artefact of our
  construction into the standard object of this literature, with a citation.
- **LEAD-NOT-RESULT.** Log-supermodularity transfers (RD-5); the
  single-crossing orientation is reversed (RD-6: ours crosses `−+`, Karlin
  needs `+−`). Unimodality of `Δ(0,Q)` in `Q` is NOT established; the
  plausible target is an interior *minimum* (consistent with R2's refutation),
  and it has not been derived.

### 7–8. The contribution sentence (threads 7–8)

The survey's drafted sentence — *moving the choice from the intensive to the
extensive margin makes the MPS comparative static sign-determinate without
higher-order restrictions on utility* — is now backed at both ends:

- ST-12a: within Schroyen–Treich's own condition, same `A`, different `P`,
  opposite signs at the same `m`. The third derivative is load-bearing there;
  P9-gen needs none. The separation is substantive, not verbal.
- LR-3: quadratic utility separates the channels — `κ′ = −c/4 < 0` (ours
  works) while `A′ > 0` (theirs fails).

Two strengthenings the survey misses, both artifact-backed:

1. Lazear–Rosen's precedent opens *"While it is not possible to make a general
   argument based on an example…"* — the authors decline to generalise. The
   acknowledgement should say they **suggest** the sorting on an example under
   DARA; the present model **derives** it under concavity alone,
   distribution-free.
2. The **object of choice** differs: theirs is a choice between two
   compensation schemes that both pay (risk-tolerance for a binomial spread);
   ours is whether to pay a fixed fee at all (affordability). Different
   economics, not merely different derivatives.

None of this is currently in `PROOFS.tex`; open item 4 still says the
literature position "should be settled before further proof effort", which the
survey settled and twelve artifact directories have now audited.

### 11. The welfare frame (thread 11)

The deferred section's inputs are now staged and mutually consistent:

- **Branch criterion** (LS-7): Prop 6 applies iff eq. (18) holds; a fixed
  prize gives `V_n ≡ V`, so eq. (18) fails by exactly `V − W_n` and the model
  is in the CV branch **by construction**, citable to LS p.596 ("social gains
  are zero … for all n ≥ 2") rather than by analogy.
- **Conceptual authority** (CMP-2): CMP95 §4's premise *generates* LS eq. (8)
  in one line — but §4 is Concluding Comments, prose, not a theorem; cite as
  motivation.
- **Heterogeneity objection pre-empted** (MW-5): Moreno–Wooders Prop 3 shows
  heterogeneous private costs leave free entry optimal *in the IPV branch* —
  the branch is set by `V_n`, not by the cost distribution. The optimum is the
  constrained one, over symmetric independent entry rules (p.320), and the
  paper's own Table 1 beats it with an entry cap (added 6 Oct 2026).

Two survey errors must not leak into the drafting:

- **Eq. (9) is the social planner's FOC** (`∂S/∂q = 0` from eq. 8),
  characterising `q^s` — not a "win only if alone" reservation condition. The
  same algebra *is* FJL's Definition 1, reached from the opposite direction;
  identifying them as one condition inverts the excessive-entry wedge that
  Prop 3 is about. Replacement text is in
  `levin_smith_1994/NOTES.md`.
- The monotone welfare decline is **Proposition 9 alone**; Prop 8 is the
  no-entry probability. And the "identities not explained" sentence is main
  text p.586, with fn. 2 attached to the preceding sentence.

### 9, 12. Small `PROOFS.tex` items (threads 9, 12)

- MW phrase: "the identities are not determined" → "the number of entrants
  and the identity of the entering set are both random, being determined by
  unobserved cost draws" (in MW the *rule* is deterministic given draws; the
  indeterminacy-without-any-rule problem belongs to the identical-agent
  papers). Replacement in `moreno_wooders_2011/NOTES.md`.
- `V` is introduced with no citation. CMP92 abstract/§II + CMP95 §4 are the
  foundation; CMP-4 (Δ homogeneous of degree 1 in `V`) is the one-sentence
  licence for importing the foundation without the matching machinery.

---

## Consolidated action lists

### A. Corrections to `LITERATURE.tex` (survey errors; fix before anything propagates)

1. Eq. (9) paragraph (Levin–Smith): planner's FOC, not a reservation
   condition; same equation as FJL Def. 1 from the opposite direction.
2. "Propositions 8–9" → welfare decline is Prop 9; Prop 8 is P(no entry).
   Fn. 2 locator: main text p.586.
3. Verdict item 3: "hazard-rate result" → "density result
   `b_k = E[f(X_{(k−1:k−1)})]`".
4. "Consequence for open item 2": drop "same technique" and "independent";
   CL Prop 5 = quantile decomposition + non-decreasing weight, no
   log-concavity anywhere; Suen adds concavity + log-concavity and cites CL.
   Record the three obstacles (count vs aggregate; rescaling trap, fn. 1;
   cross-side vs same-side). Revised candidate: monotone-weight route first.
5. FJL count bound: point at Definition 2 alongside eq. (2).
6. MOS figures: label 2.5/3.7 as rounds 26–50 and attach the directions
   (+0.5 excess vs −0.3 shortfall, opposite signs).
7. Lazear–Rosen: add the "example, not general" qualification and the
   choice-object difference.
8. Handover next-step 3 (log-concavity of 1−Λ) and next-step 4 (P-MU/RD):
   update to match items 4 and the RD findings.

### B. Additions to `PROOFS.tex` (all evidence in hand; one pass)

1. §P7: cite FJL eq. (2) + Definition 2 and Fu–Lu eq. (7); present as the
   deterministic, heterogeneous, utility-unit version under fixed rules with
   no designer; drop the unqualified "stronger" framing.
2. §Primitives at `V`: cite CMP 1992/1995 with the scale-factor sentence.
3. §P9gen: cite Costrell–Loury (pivot rule stated by them; fixed-θ vs
   endogenous-k* disanalogy) and Hopkins–Kornienko 2009 + Shaked 1982 for the
   dispersive order, stating the `Dispersive.lean` results: dispersive +
   crossing ⇒ both P9-gen hypotheses, with MonotoneT free (HK-L1) and the
   crossing necessary (HK-L4).
4. §PMU: one provenance sentence — the weight is RD's kernel — citing RD.
5. Contribution paragraph (discharges open item 4): the drafted sentence +
   Lazear–Rosen acknowledgement (example/theorem + choice-object) +
   Schroyen–Treich naming (`u″` vs `u‴`, global vs local).
6. §Primitives MW phrase tightening (thread 9).
7. Rewrite open items 2 and 4 to current state.

### C. Welfare section (deferred; inputs ready)

Frame: LS Prop 3 via the LS-7 branch criterion (cite p.596 directly); CMP95
§4 as motivation; MW Prop 3 to pre-empt the heterogeneity objection; LS
eq. (9)/`q^s` correctly as the planner's benchmark; corners `μ = 1` and
prohibitive `c` per the handover plan.

### D. Research leads (nothing claimable)

1. **Open item 2**: try the Costrell–Loury monotone-weight route on `k*`
   first; the count/aggregate gap is the real obstacle; avoid rank-rescaling
   or accept the increasing-density restriction (Suen fn. 1).
2. **P-MU unimodality**: work out the `−+` orientation (plausible target: an
   interior minimum of `Δ(0,Q)` in `Q`); or check RD Appendix A.2's TP_r
   machinery (unread) for the multimodal/reversed case.
3. **Two-pivot generalisation** (HK 2004's ULR, two thresholds): natural Lean
   extension of `Dispersive.lean`; untouched.

### E. Follow-ups on sources

- Published-version checks unchanged from the survey's list (Costrell–Loury
  JPE numbering; Fu–Lu *EI* title/numbering; Schroyen–Treich *GEB* Theorem 3;
  CMP95 published form).
- Levin–Smith quotations were transcribed by eye from a scan; re-verify before
  quoting in print.
