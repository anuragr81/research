# Literature verification artifacts

One directory per paper. Each verifies **our reading of the paper**, not the
paper. Nothing here reproves a published result; the suites check that the
conditions, reductions and special cases attributed to a paper in
`LITERATURE.tex` are arithmetically correct and mutually consistent, and that
the separation claimed from our own results is real rather than verbal.

This is worth doing because the survey has already had to correct one
paraphrase error of exactly this kind (`LITERATURE.tex`, "Correction to the
previous draft": an earlier version stated Schroyen--Treich produce no
comparative statics on the wealth distribution, when their Theorem 3 signs a
mean-preserving spread).

## What a paper directory contains

| File | Role |
|---|---|
| `RECONSTRUCTION.md` | **Written first, before reading what the survey says.** The paper rebuilt from its own primitives in our notation: objects, the derivation chain, and what each result is derived *from*. Ends with an explicit list of what could NOT be reconstructed. |
| `CLAIMS.md` | Every claim `LITERATURE.tex` makes about the paper, given an ID, quoted or tightly paraphrased, each tagged with the source location in the paper. |
| `verify_<slug>.py` | SymPy suite checking the claims that are checkable. One check per claim ID where possible. |
| `<Slug>.lean` | Only where the paper's content is genuinely discrete or order-theoretic. Most papers will not have one; absence is the default, not a gap. |
| `NOTES.md` | What was verified, what could not be, and every discrepancy found against the source. |

## Why reconstruction comes first

Arithmetic checks cannot catch a role-error. In the Levin--Smith pass, check
LS-6 correctly verified that their equation (9) is algebraically identical to
Fu--Jiao--Lu's Definition 1 -- a true fact -- while the survey's claim about
what equation (9) *is* was wrong: it is a social planner's first-order
condition, not an entry-equilibrium condition. The equation is the same; the
role is not, and role is invisible to a check on the equation.

Reconstructing the derivation chain makes role explicit by construction. It is
slower, and it is the step that turns catching such errors from luck into
method.

## Rules

1. **Never write "the paper is verified."** The permitted conclusion is "our
   reading is arithmetically consistent with the source."
2. A claim tagged `[A]` or `[C]` in `LITERATURE.tex` must not be given a
   verification artifact that implies a full read. Verify what the abstract
   supports and say so.
3. Every check must be able to fail. A check that passes for the intended
   claim and for its negation is worthless -- see the P7 end-to-end episode
   recorded in `HANDOVER_20260901.md`. State the non-vacuity control in
   `NOTES.md`.
4. Quote the source with a locator (theorem number, equation number, page)
   so a reader can confirm without re-deriving.
5. Where the source read was a working paper, say so; proposition numbers may
   not survive to the published version.

## Running

`./verify_lit.sh` runs every paper suite and fails if any suite fails or if a
paper directory is missing its `verify_*.py`. It is separate from the main
`verify.sh`, which covers the model's own claims.
