# Building a verified manuscript skeleton

These are instructions for a session that builds or extends a manuscript
skeleton like the one in `papers/jite/endogenous_entry/entry_contest/`. They
assume no knowledge of the conversations that produced it. The entry-contest
bundle is the reference implementation. Copy from it and change only what the
new paper needs.

Before starting, read this file, then these four files in the reference bundle.

1. `notes/writing_discipline.md`, the rules every new sentence follows. Copy
   it into the new bundle.
2. The header comment of `MANUSCRIPT.tex`, which gives the row macros and the
   rules.
3. `checks/verify_manuscript.py`, the checker that enforces them.
4. `lit/README.md`, which describes the per-paper literature records.

## 1. What the skeleton is

The skeleton is a manuscript whose body is four tables (introduction, model,
literature, conclusions) and whose appendices carry every proof. Every
statement in it rests on one of four grounds.

- A theorem of Lean 4.
- A counterexample checked in Lean.
- A model assumption, stated in the primitives appendix.
- A verbatim quotation, with its page, from a source that was read.

Numbers may appear as illustrations. SymPy and numerical sampling are never
evidence for a claim. They stay in the bundle only as illustrations and
regression tests.

A script checks four things on every build. Every row link resolves, every
Lean name exists and passes the axiom audit, every quotation appears in its
source record, and no proof exceeds the prose limit. The prose submitted to a
journal is written from the skeleton later. The skeleton is what makes that
prose checkable.

## 2. Order of work

1. **Novelty.** Read the closest papers before calling anything new.
2. **Verification.** Prove the model's claims in Lean, and record what each
   source says.
3. **Manuscript.** Fill the tables from what was verified.

A sentence is written from a verified row and never ahead of it. A claim not
yet verified goes into the pending conclusions table or stays out.

## 3. Bundle layout

| Path | Role |
|---|---|
| `MANUSCRIPT.tex` | The skeleton. The only canonical statement of the results. |
| `refs.bib` | Bibliography. Every key cited in an L row must be here. |
| `lit/<author_year>/` | One record per paper read (section 6). |
| `lit/verify_lit.sh` | Runs every paper suite, and fails if a record has no `verify_*.py`. |
| `lean/<Paper>.lean` | Optional core-Lean file, no Mathlib. |
| `lean/mathlib/` | A Lake project pinned to a toolchain and a Mathlib commit. Each file is a build root in `lakefile.toml`. |
| `checks/verify_manuscript.py` | The skeleton checker (section 4.3). |
| `checks/verify_mathlib.py` | Builds and axiom-audits the Mathlib files. |
| `checks/verify_docs.py` | Fails if a count stated in the manuscript differs from the generated audit. |
| `verify.sh` | Runs everything and writes `VERIFICATION.md`. That file is generated, so never edit it by hand. |
| `TODO.md` | The pass plan (item PLAN), the author's decisions with dates, and the novelty ledger. |
| `LITERATURE.tex` | The explored literature. It holds everything read that is not an L row, and may be amended freely. |
| `PROOFS_ADDENDUM.tex` | Results proved in Lean that go beyond what the manuscript states, such as weaker hypotheses or more general statements. |
| `MEASUREMENT_MAP.tex` | Optional. For every input and output of the model, it records what the object is and how an observer could obtain it. |
| `RETIREMENT.md` | Only when the skeleton replaces an earlier document. It maps every part of the earlier document to its new home, and a script checks the map against git. |
| `notes/writing_discipline.md` | The writing rules. |

Source PDFs and their extracted text are cached outside the repository, in
`~/.cache/<bundle>/`. The author supplies them, usually on Google Drive.

## 4. The manuscript

### 4.1 Macros

Copy these verbatim. The checker parses them by name and argument count.

```latex
\newcommand{\lean}[1]{\path{#1}}
\newcommand{\rowref}[1]{#1}
\newcommand{\unv}[1]{\textit{Unverified:} #1}
\newcommand{\pend}[1]{\textit{Pending:} #1}

\newcommand{\krow}[4]{#1 & #2 & \rowref{#3} & #4 \\ \addlinespace}
\newcommand{\mrow}[4]{#1 & #2 & #3 & \lean{#4} & p.~\pageref{proof:#1} \\ \addlinespace}
\newcommand{\lrow}[6]{#1 & \citet{#2} & #3 & ``#4'' & #5 & \rowref{#6} \\ \addlinespace}
\newcommand{\crow}[3]{#1 & #2 & \rowref{#3} \\ \addlinespace}

\newenvironment{mproof}[2]
  {\subsection*{Proof of #1}\label{proof:#1}%
   {\raggedright\noindent\textit{Lean:} \lean{#2}.\par}\smallskip}
  {\par\hfill$\square$\par\medskip}
```

The arguments, in order, are these.

- `\krow{ID}{headline}{model rows}{scope note}`
- `\mrow{ID}{claim in words}{statement in symbols}{Lean theorems}`
- `\lrow{ID}{bib key}{what we rely on}{verbatim quote}{page}{model or conclusion rows}`
- `\crow{ID}{statement}{model or literature rows}`
- `\begin{mproof}{model row}{Lean theorems} ... \end{mproof}`

Lists are comma-separated. A Lean name is fully qualified,
`Namespace.theorem`. Tables are `longtable` in `\footnotesize`, with
`booktabs` rules and the `P{width}` column type defined as
`>{\raggedright\arraybackslash}p{#1}`.

### 4.2 Sections, in order

1. **Introduction.**
   - It opens with a paragraph of two or three sentences. The paragraph states
     the paper's question in one sentence, in the author's words if the author
     gave them, and adds at most one line of framing.
   - Table 1 follows, with columns ID, Headline, Model rows and Scope note.
   - Each scope note says what its headline does not claim. That covers the
     hypotheses the headline needs, where it fails (citing the counterexample
     row), and what is already in print.
2. **Model.** Table 2 has the columns ID, Claim, Statement, Lean and Proof.
   - The claim is in words. The statement is in symbols and gives both the
     hypotheses and the conclusion.
   - The Proof column holds the page of the proof and is generated by the
     macro.
3. **Literature.** Table 3 has the columns ID, Paper, What we rely on, Quote,
   Page and Rows.
   - It holds only papers that were read.
   - Below the table, one sentence names every paper that is cited elsewhere
     but not read, and says that every claim made about them is unverified.
4. **Conclusions.** A short paragraph explains the two tables that follow.
   - **Table 4** holds the conclusions that rest on model rows and on papers
     read in full.
   - **Table 5** holds every claim that something is new, and anything else
     still waiting. Each of its rows ends with `\pend{...}`, naming exactly
     what it waits on (a paper, a published version, or a search).
   - Keep C IDs stable when a row moves between the tables.

The appendices follow the conclusions.

- **A. Proofs.** It opens with a Notation paragraph that defines every symbol
  once. Then comes one `mproof` per model row.
- **B. Evidence and verification.**
  - The toolchain and the Mathlib commit.
  - The checks that `verify.sh` runs.
  - "What Lean does not carry", which lists the assumptions that enter the
    theorems only as hypotheses.
  - "Illustrations", which says that no row rests on SymPy or sampling.
- **C. Terminology.** A table of Term, Symbol and Meaning. It binds every
  load-bearing English term to a symbol already defined, so it is a key and
  not a second set of definitions.
- **D. Primitives and scope.**
  - One paragraph per primitive or boundary.
  - Each paragraph says what lies outside the model and which rows depend on
    it.
  - Model choices made by the author are recorded here, with their reason.
- **E. Refuted conjectures.** Each conjecture comes with its Lean
  counterexample.
- **F. Responses to the referee reports.** Only for a resubmission.
- **G. Coverage of the referee comments.**
  - A table of Comment, Substance, Status and Where.
  - Status is one of answered, partly answered, open or moot.
  - Moot means the comment concerned an object the rebuilt model no longer
    has.

### 4.3 What the checker enforces

`checks/verify_manuscript.py` applies six rules.

| Rule | Requirement |
|---|---|
| MS-1 | Row IDs are well formed (`K1`, `M1`, `L1`, `C1`, ...) and unique. |
| MS-2 | Introduction rows point to model rows only. |
| MS-3 | Conclusion rows point to model or literature rows only. |
| MS-4 | Every model row names Lean theorems that are declared in an audited file. Every model row has exactly one proof, and the proof names the same theorems. |
| MS-5 | Every literature row's bib key is in `refs.bib` and has a `lit/` record, the record's `KEYS` file lists that key, and the quote appears verbatim in the record's `CLAIMS.md` or `NOTES.md`, after normalising LaTeX and whitespace. |
| MS-6 | Plain English is at most 30% of every proof, counted as prose words over prose words plus mathematical tokens. |

Each rule also runs on a well-formed sample, which it must accept, and on a
faulty sample, which it must reject. A rule that cannot fail measures nothing.

In a new bundle, adapt only `lean_declarations()` to the bundle's Lean files,
and keep the rest. Wire the checker into `verify.sh`.

### 4.4 Rules no script enforces

- **Headlines.** A headline that claims more than its rows prove is a defect,
  even when every link passes. The scope note gives the limits.
- **What we rely on.** In an L row, this column names the model row that the
  paper supports or contrasts with, and says where the parallel ends.
- **Versions.** Mark an L row `\unv{...}` when the version read is not the
  version cited, such as a working paper or a draft, and say which was read.
- **Novelty.** A claim that a result is new stays in Table 5 until every paper
  that could contain the result has been read in full.
- **New rows.** When a new row contradicts text already in an appendix, fix
  that text in the same commit. Search the appendices for every object the new
  row touches. A scope paragraph that still denies what a new row proves is
  the usual case.
- **Beyond the manuscript.** A proof that goes beyond the manuscript's stated
  assumptions goes into `PROOFS_ADDENDUM.tex`. Restricting a result to a
  family of functions needs an economic reason.
- **Lean names.** They are for internal verification. They leave the printed
  manuscript before submission.

## 5. Lean

- **Two rules for axioms.** A core-Lean file may depend on `propext` and
  `Quot.sound` only. A Mathlib file may also depend on `Classical.choice`.
- **The audit.**
  - Search every source for `sorry`.
  - Run `#print axioms` on every declared theorem and parse the output.
  - Fail on any axiom outside the allowed set.
  - Fail if fewer theorems were audited than were declared.
  - When extracting theorem names, match `[A-Za-z0-9_']*`. A narrower class
    once truncated names containing digits and left three theorems unaudited
    while the run reported full coverage.
- **Namespaces.** One namespace per file, and model rows cite
  `Namespace.theorem`.
- **Controls.** Every cluster of theorems carries at least one control, named
  `control_*`. A control is a theorem showing that the conclusion fails when a
  hypothesis is dropped, usually through an explicit witness in exact
  rationals.
- **Counterexamples.** A counterexample is a Lean theorem about a concrete
  witness, never a sampling result.
- **Analytic steps.** Analytic steps are proved, not assumed. Whatever enters a
  theorem only as a hypothesis is listed in Appendix B under "What Lean does
  not carry".
- **Structural claims.** Before asserting a structural claim, such as
  uniqueness or "every equilibrium has ...", name the theorem that quantifies
  over the objects the claim is about. If no theorem does, the claim is prose
  and does not enter.

## 6. Literature records

Each paper read gets a directory `lit/<author_year>/` with these files.

| File | Contents |
|---|---|
| `RECONSTRUCTION.md` | Written first. It rebuilds the paper from its own primitives, gives the derivation chain, says what each result is derived from, and ends with what could not be reconstructed. It also records the source read (version, pages, upload date, cache path). |
| `CLAIMS.md` | A quotation table with the columns ID, Page, Quotation and What we rely on. A table of the Lean results. A table of "Readings recorded", which gives an ID such as `XX-D1` to every discrepancy or typo found in the source, together with the reading adopted and its reason. |
| `NOTES.md` | What was verified, what could not be, every discrepancy, and the control of each check. |
| `KEYS` | The paper's bib keys, separated by whitespace. The manuscript checker uses this file to find the record. |
| `verify_<slug>.py` | The paper's suite (see below). |
| Lean | Either `<Slug>.lean` in the directory (core Lean), or a file `LEAN` that names a file in `lean/mathlib/`. Use the paper's own notation. |

Each claim about a source carries an evidence tag.

| Tag | Meaning |
|---|---|
| [F] | The full text was read. |
| [P] | Page images from the author's own copy were read. The images are pinned by sha256, and nothing beyond the pages read is attributed. |
| [A] | Only the abstract or a publisher summary was read. Such a claim cannot support a positioning claim. |
| [C] | The paper is known only through a citing paper's description. Such a claim cannot support a positioning claim either. |

Each paper is handled in these steps.

1. Ask the author for the paper by full reference, with its DOI or a link.
   Cache the file and its extracted text under `~/.cache/<bundle>/`. If the PDF
   has no text layer, read the page images and pin the PDF by sha256.
2. Read the paper in full and write `RECONSTRUCTION.md`.
3. Write `CLAIMS.md`. A quotation is verbatim, and an elision is recorded.
4. Formalise in Lean the results we use, with controls. An interpretive
   source, such as a sociology text, still gets Lean for whatever structure we
   attribute to it, unless the author exempts the paper. Record any exemption
   in `lit/README.md`.
5. Write the suite. It checks five things.
   - The quotation rows parse, and their pages lie in the paper's range.
   - Each quotation is found in the cached text by exact normalised
     containment, not by a fuzzy ratio. A fabricated quotation is the
     control, and it must fail.
   - The cached file matches its pinned sha256.
   - The Lean file is a build root, it builds, it has no `sorry`, every Lean
     name cited in `CLAIMS.md` is declared, and every theorem passes the axiom
     audit.
   - The named controls are present.
6. Then update everything that depends on the paper.
   - Add the paper to `refs.bib` and `KEYS`.
   - Add its L row.
   - Revise every C row that waited on the paper, moving it between Tables 4
     and 5 if it now qualifies.
   - Remove the paper from the unread list.
   - Update `LITERATURE.tex` and the novelty ledger in `TODO.md`.

A record checks our reading of a source and does not reprove the source.
Never write "the paper is verified". Write that our reading is consistent with
the source. Arithmetic checks cannot catch a role error. In the reference
bundle, an equation was correctly matched to another paper's definition, but
the equation was a planner's first-order condition and not an equilibrium
condition. Reconstructing the derivation chain is what exposes that kind of
error.

## 7. Passes

Each pass ends with every suite green and a commit. The author may reorder
passes. In the reference bundle, the literature table and the headlines were
done before the predecessor document was retired.

| Pass | Work | Done when |
|---|---|---|
| 1 | Scaffold the skeleton: four empty tables, the ID scheme, the appendix stubs, and the checker with its controls, wired into `verify.sh`. | The empty skeleton builds, and every rule passes its good sample and fails its bad one. |
| 2 | Set up the Lean project, with the toolchain and Mathlib pinned, the build roots listed, and the audit in place. | `verify.sh` builds and audits it. |
| 3 | Move the analytic steps into Lean, one cluster per pass, with controls and counterexamples. | Each step is a theorem that passes the audit. |
| 4 | Add model rows and their Appendix A proofs, one family of results per pass. | MS-4 and MS-6 pass. |
| 5 | Fill Appendices B to G. Open items go to `TODO.md`. | Every stub is filled. |
| 6 | Retire any predecessor document. A completeness check confirms that every claim, number and Lean name in it has a home. | `verify.sh` is green without it. |
| 7 | Do the novelty reading. Every paper that could hold a claimed result gets a `lit/` record and Lean, and then the ledger is revised. | Every Table 5 row names only what is genuinely outstanding. |
| 8 | Write the conclusions, then the introduction. Each headline is assessed for overreach. | MS-2 and MS-3 pass, and every headline has a scope note. |
| 9 | Fill the literature table, with only the papers a model or conclusion row depends on. | MS-5 passes. |
| 10 | Readability rounds on the text added since the previous round, under the writing discipline. | Record what changed in `TODO.md`. |
| 11 | Re-read every referee comment against the verified results and update Appendix G. | Every comment has a status and a place. |

## 8. Writing

`notes/writing_discipline.md` governs every sentence. These points recur in a
skeleton.

- **One name per object.** Use one name per object across the manuscript, the
  terminology table and the measurement map. Check a new symbol against every
  symbol already in use. In the reference bundle, a share written `s(k)`
  collided with the component `s` and had to be renamed `\xi(k)`.
- **Punctuation.** No colons in manuscript prose, and no dashes doing the work
  of a sentence.
- **Say it once.** Each point is stated once. Other places point to it by row
  ID or by appendix paragraph.
- **Pronouns.** Every pronoun has its noun on the page.
- **Contrasts.** Every contrast names both sides.
- **Reasons.** Every choice the model makes comes with a reason a reader can
  check, never a preference.
- **Mechanism first.** If a mechanism cannot be stated in one line, the
  paragraph is not ready to write.

## 9. Working with the author

- **Decisions.** Model choices, definitions, interpretations and placements
  belong to the author.
  - Present the options with their economic consequences and give a
    recommendation.
  - Record the decision with its date in `TODO.md`, and in Appendix D where it
    affects the model.
- **What not to ask.** Do not ask what the code, a source or a checker can
  settle.
- **Missing papers.** When a paper is not available, list it as unread and
  mark the claims that depend on it pending. Never cite a paper from memory.
- **Pace.** Verify each step before the next.
- **Reporting.** Report failures with their output. Say when a step was
  skipped.

## 10. Build, check and commit

Before every commit, run these checks.

1. `./verify.sh` exits 0, and `VERIFICATION.md` is regenerated.
2. `lit/verify_lit.sh` exits 0.
3. `python3 checks/verify_manuscript.py` reports no failures.
4. `pdflatex` and `bibtex` build the PDF, and the log shows no overfull boxes
   and no undefined references or citations.

Every count the manuscript states, such as the number of audited theorems, is
written in digits and is checked against `VERIFICATION.md` by
`checks/verify_docs.py`. A count spelled out in words once escaped a numeric
check.

Commit and push after each batch. Stage files by name, because other sessions
may have uncommitted work in the same repository. Never commit any of these.

- LaTeX build files.
- `__pycache__/`.
- Session manifests and tarballs.
- `HANDOVER*` continuity notes.

End each commit message with the attribution line the session specifies.

## 11. Errors already made once

| Error | Guard |
|---|---|
| A check whose two sides are the same expression rearranged | Before adding a check, name the input that would make it fail. |
| A suite printed "sorry: 0" without measuring it | Every printed figure is compared by a check. |
| The prose generalised beyond what the Lean quantified over (identities of entrants claimed pinned, only the count was) | Name the theorem that would fail if the claim were false. |
| An environment fact was asserted in prose ("Mathlib is unavailable") | Measure it on every run and let the generated file carry it. |
| A sampling loop delivered fewer cases than the prose cited | Read the printed count, not the loop bound. |
| A scope paragraph was left stating what a new row refutes | Search the appendices for the objects of every new row. |
| A fabricated control quotation scored 0.97 on a fuzzy match, just under the pass threshold | Use exact containment after normalisation. |
| A source printed a sign error in a display | Record it as a reading (`XX-D1`), use the correct form, and say why. |
| The expected paper turned out to be a different one | Check the title page against the reference before building a record. |
