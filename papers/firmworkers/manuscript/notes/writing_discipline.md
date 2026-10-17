# Writing discipline (firmworkers)

Copied on 12 Oct 2026 from `papers/wealth_garch/notes/writing_discipline.md`, the newest version. Rules 1 to 5 and 7 to 10 are unchanged apart from the Lean path in rule 1; rule 6 holds this paper's terminology.

What produced the prose that matched the author's understanding in the
2026-09 sessions, written down so it transfers to sessions that do not have
this conversation's history. Rules marked (A) were stated by the author, (F)
came from the falsifier-first document the author uploaded on 2026-09-27, (S)
are what I found myself doing once the others were enforced.

## 1. Nothing enters the text before it is verified (A)

- A mathematical claim is proved in Lean first, under `lean/IscLean/` (model) or
  `lean/Lit/` (literature), and
  the prose is written *from the theorem*, not the other way round. SymPy
  and numerical runs are illustrations, never the ground of a claim. The check
  fixes the exact statement, its conditions and its closed form, and the
  sentence carries those.
- A claim about a source is written after reading the source, with the page.
  A quotation is verbatim. An elision is recorded.
- A claim that is inferred rather than checked is marked as such in the note,
  and is left out of manuscript text.

Why it improves the writing and not only the correctness: a sentence written
from a verified row cannot be vague, because the row says exactly what is
true and under what condition. "First order generically" becomes
"$(1-\omega)(\alpha-q_0)$, zero only at $\omega=1$ or when the cue delivers
the prior marginal".

## 2. Falsifier first (F)

For every claim, name the world in which it is false, and say what rules
that world out. A claim that cannot name its falsifier does not enter.

- The falsifier is instantiated as its own object and compared at the scale
  where the claimed effect lives. For the sequence effect the rival object is
  the Bayes-factor benchmark; for "second order" it is the interior weight.
- Naming the rival closes the referee's easy move. "I decline your
  assumption" has to become "I read impressions as Bayes factors", a position
  with documented costs.
- It also exposes when a claim is already a proposition in the paper (the
  forcing claim at line 277 was Proposition DIV all along), so no new result
  gets invented.

## 3. Every contrast names both sides (A)

- "X rather than Y", never "X". Test each sentence with the counterfactual:
  *contrasts with what?* If the other side is missing, write it in.
- No "this", "it", "the effect", "both readings" without an unmistakable
  referent already on the page. Name the noun.
- No ordinals for things that could be confused with the paper's own ordered
  objects. "Under the first, ... under the second" collided with first and
  second *cue*; it became "read as a level ... read as a Bayes factor".
- Plain declarative sentences. No "the question is sharp", "this is not a
  modelling preference", "reveals it". If a sentence asserts that something
  is important instead of saying what it is, cut it.

## 4. One job per paragraph (A, after the "patchwork" correction)

- Before adding anything, state in one line what the target paragraph is
  for. If the new material does not serve that job, name where it goes
  instead, or say it should not go in.
- Propose the cut alongside the addition. If nothing can be named to come
  out, the paragraph is full and the material goes elsewhere.
- Append nothing. Replace. Appending never requires arguing that something
  should go, which is how five approved sentences became one paragraph with
  no direction.
- Say no, including to my own earlier suggestions.

## 5. Do not repeat what the document already says (A)

- Check the existing text before drafting. The abstract restates the
  introduction by design; a section opener restates a result by design;
  everything else appears once.
- When the author supplies a sentence, the plan's version of the same
  content is superseded, not kept alongside.

## 6. Manuscript terminology only, fixed and agreed (A)

- One name per object, from the Terminology section of the measurement map
  (`measurement_map.yaml`: each row's `term` and the list `terms`), proposed
  on 12 Oct 2026 at the author's request and awaiting the author's agreement
  (status PROPOSED until then). The names in its column Not to be called do
  not enter manuscript prose. `check.py` rejects them in the model summary and
  in the map's prose, and lists the ledger rows that still use them.
- Points that need care:
  - $h$ is the **status group**, never "human capital" or "status label". The
    ledger used "human capital" both for $h$ (M28) and for skills (X14).
  - $a$ is the **tail index**, never "stakes" (M5 is refuted). **Inequality**
    is the plain word for a low tail index.
  - $b$ is the **cost exponent**. A higher cost exponent raises the
    **upskilling cost** only for moves larger than one unit of income, so
    "higher $b$" is never written as "higher cost".
  - The **lottery** is the model's route; **competing through connections**
    is its real-world counterpart. "Social contest", "social lottery" and
    "social navigation" do not enter.
  - A displaced worker has three routes, the **downgrade**, the **return** and
    the **lottery**. An **entrant** is a displaced worker in the lottery, the
    **pool** is the set of entrants, and the **partner** is the entrant whose
    former job an entrant receives.
  - **Job**, not "position"; **closure**, not "categorical friction".
- No new terms. If a term is not in the table, use the manuscript's words for
  the thing, or add the term to the table first.
- The plain-English model summary (`claims.yaml`, `summary`) rests sentence by
  sentence on model rows, Lean theorems or a verbatim source premise, and
  `check.py` keeps it within 270 to 310 words, free of colons, dashes and
  symbols.
- Idea summaries (`claims.yaml`, `idea_summaries`; section Idea summaries of
  the skeleton) follow the same rules, each with its own word range. They stay
  in step with the claims at all times (author, 13 Oct 2026).
  `summary_stamps.yaml` holds a fingerprint of every row and theorem statement
  each summary (and the model summary) cites; `check.py` fails when one
  changes or a new one is cited. Review the sentences resting on it, revise
  them, and only then run `check.py --restamp`. Restamping without the review
  defeats the check.
- The Players sub-section (`claims.yaml`, `players`, `choices`) follows the
  same grounding, terms and stamps. Each player is a term of the table, and
  each choice row carries a status (DECIDED, PROPOSED, DEFERRED) and names the
  issue that would decide it.
- The base case (`claims.yaml`, `base_case`) follows the same rules: its
  summary like an idea summary, its payoff table like the players table,
  and only LEAN_PROVED rows.
- The questions and answers (`claims.yaml`, `faq`) follow the same rules;
  each answer has its own word range and is stamped, and each question ends
  with a question mark and uses the manuscript's terms.

## 7. Mechanism before prose (S)

The paragraph on the second channel was good because the mechanism had been
forced into plain words first, by the author asking "why is $c=0$ so
important" and "is Bayesian inference enough". Only after "full adoption is
the setting in which the paper's channel is the only one present" could be
said in one sentence was the paragraph writable. The one-sentence test is the
gate: if the mechanism cannot be stated in one line, the paragraph is not
ready.

## 8. Compression tests the author applies, and I now apply first (S)

- "What is this paragraph trying to convey, in one line?" If the answer
  lists more than one thing, the paragraph is patchwork.
- "Give me one sentence." The one sentence is the paragraph's job; the
  paragraph is that sentence with its evidence.
- "Without symbols" and "with symbols" are two different readers. Write for
  the one asked for, and keep the content identical between them.

## 9. Surface rules (A)

- No colons in manuscript prose.
- No dashes doing the work of a sentence.
- Rewrites stay within 80--120% of the author's draft.
- Line numbers go stale; anchor every edit by its BEFORE text.

## 10. Placement and bridging (S, from the intro-prelude exchange, 2026-09-27)

- Material goes where the reader first asks its question. The two-channel
  sentence belongs where the intro first says sequence dependence is
  unclear, because that is where a reader asks "which mechanism, and why".
- State the bridge. When a new sentence sits next to one on a different
  subject (source of the effect next to how it registers), the sentence that
  joins them must be written, not assumed. Here: the registering question
  has a hard answer for only one of the channels.
- A choice the paper makes is justified by reasons a reader can check, never
  by preference. "The channel for which the question is non-trivial", "the
  only one present under the premise", "the one that survives a procedure"
  are reasons. "The one we study" is not.
- Name what the paper does *not* do in the same breath as what it does
  ("measures it rather than studies it"). The scope is a contrast, so it
  obeys rule 3.
- Both sides of a contrast in parallel grammar ("an interaction between the
  cues ... a temporal bias of the observer"), so the reader sees they are
  the two answers to one question.
- Count how many times a theme is touched in a section and name each
  touch's distinct job. If two touches share a job, one loses a clause.
  Flag it; the author decides.
- When asked "what do you think", answer in that order: the verdict, the
  conditions under which it holds, then the draft. Never the draft first.

## Pre-send checklist

1. Is every claim in the sentence a verified row, a read page, or the
   author's own premise? Which one?
2. What is the world in which the sentence is false, and what rules it out?
3. Does every contrast name both sides? Does every pronoun have a noun on
   the page?
4. What is the paragraph's one job, and does this sentence serve it?
5. Is this already said elsewhere in the document?
6. Is every term the manuscript's own?
7. Can the point be said in one line? If not, stop and find the mechanism.
8. Is it placed where the reader first asks its question, and is the bridge
   to the neighbouring sentence written?
9. Is every choice given as a reason the reader can check?
10. How many times does this section touch the theme, and does each touch
    have its own job?
