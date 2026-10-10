# Writing discipline

What produced the prose that matched the author's understanding in the
2026-09 sessions, written down so it transfers to sessions that do not have
this conversation's history. Rules marked (A) were stated by the author, (F)
came from the falsifier-first document the author uploaded on 2026-09-27, (S)
are what I found myself doing once the others were enforced.

## 1. Nothing enters the text before it is verified (A)

- A mathematical claim is proved in Lean first, under `lean/mathlib/`, and
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

- One name per object. Agreed 2026-10-10, taken from
  `00_reader/TERMINOLOGY.md` and the binding paragraph of
  `00_document/PROOFS_v2.tex`:
  - $\lambda_S$ is the **asymmetry parameter**, never "loss aversion".
    Loss-aversion language would need elicited-choice evidence that
    observational data cannot supply.
  - $\lambda_V$ is the **induced variance asymmetry**, read off the
    **variance ratio** $\lambda_V^4$. $\lambda_S$ and $\lambda_V$ are
    distinct objects, and no relation between them is asserted.
  - $Q$ (level) and $q$ (ratio) are **required capital**. $K$ is only the
    **fixed issuance cost**, and $\kappa$ the **proportional issuance
    cost**.
  - $c$ is only the correlation between the risky return and the liability
    shock. $\rho$ is only the discount rate, $\rho_L$ the liability-net
    discount rate, and $r_E$ the return on equity.
  - The three controls are named by type, **classical** (risky exposure
    $\pi$), **singular** (dividends $D$ at the **dividend barrier** $y^*$)
    and **impulse** (recapitalisation $I$ at the **recapitalisation
    trigger** $x_L$, up to the **injection target** $y_{\text{post}}$).
  - **Cap geometry** ($a_1,a_2,a_3$) is never called preference.
  - $\nu_1^2,\nu_3^2$ are the **saturation limits** of the $z$-volatility
    $\zeta$. Renamed 2026-10-10 from PROOFS_v2's $\kappa_1^2,\kappa_3^2$ so
    that $\kappa$ means only the proportional issuance cost.
- The "also called" synonyms in `TERMINOLOGY.md` (payout barrier, injection
  trigger, recapitalisation boundary, volatility ratio) do not enter the
  manuscript. One name each.
- No new terms. If a term is not defined in the manuscript, use the
  manuscript's words for the thing.

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
