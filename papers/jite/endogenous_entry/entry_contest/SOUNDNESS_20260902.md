# Soundness report — the central chain of entry_contest

Date: 2 September 2026. Follows the literature-verification programme
(`lit/`, twelve papers, all suites green) and the traceability pass
(`lit/TRACEABILITY.md`). Prompted by the author's question: *do the proofs
make sense, and does the model add anything new to the literature?*

**Method.** Adversarial pass on the central chain — primitives → P1 → P3 →
P5/anonymity → P9-gen — treating the prose with the same suspicion the
literature passes applied to the survey. Every claim below is verified by
computation where computation applies; the one theorem-candidate is clearly
marked as unproved.

**Summary of verdicts.**

| # | Finding | Severity | Status of the model after the finding |
|---|---|---|---|
| 1 | The identity-pinning claim is **false**; only the *count* is pinned | Serious (prose), repairable | P9-gen survives; P5 can be *strengthened* |
| 2 | The agent's utility function is never displayed | Moderate, one paragraph | A strength once stated (privilege-contest form) |
| 3 | Wealth-independence of score draws is implicit but load-bearing | Moderate, one sentence | `prop:anon` sound once stated |
| — | Novelty verdict | — | P9-gen carries the paper; P7, P-MU are support |

---

## Finding 1 — identities are not pinned; the count is

### The claim

`PROOFS.tex` §Primitives (*Information* paragraph) asserts that common
knowledge of the wealth profile "makes $k^*$ a *deterministic* count **with
the identities of the entrants pinned down by the wealth ordering**." The
survey's positioning repeats it more strongly: P5 is "the only [device] that
makes the identity of entrants a property of the agents rather than of the
timing protocol or of a randomisation."

### The counterexample

Take three challengers with wealths $w_1 > w_2 > w_3$ and any primitives
inducing the reduced form

$$\Delta(0)=12,\quad \Delta(1)=10,\quad \Delta(2)=1;\qquad
\kappa_1=2,\quad \kappa_2=5,\quad \kappa_3=8,$$

which is consistent with everything proved ($\Delta$ strictly antitone by
P3-cor; $\kappa$ decreasing in wealth by concavity). Enumerating all
pure-strategy entrant sets and checking Nash conditions (member $i$ of a
$k$-set stays iff $\kappa_i \le \Delta(k{-}1)$; outsider stays iff
$\kappa_i > \Delta(k)$):

**Three equilibria: $\{1,2\}$, $\{1,3\}$, and $\{2,3\}$.**

The last deserves emphasis: there is an equilibrium in which **the richest
challenger abstains** — joining as a third entrant is unprofitable even for
her ($\kappa_1 = 2 > \Delta(2) = 1$), and the two poorer entrants are each
content facing one rival ($\kappa_3 = 8 \le \Delta(1) = 10$). The wealth
ordering pins identities only in the assortative equilibrium that P5
constructs, not across equilibria.

### How common this is

Randomised experiment, 20,000 instances ($Q \in \{3,\dots,6\}$, $\Delta$
strictly decreasing and $\kappa$ strictly increasing, i.i.d. uniform draws,
seed 20260902; full enumeration of entrant sets):

| Property | Result |
|---|---|
| A pure-strategy equilibrium exists | 20,000 / 20,000 |
| The assortative prefix is an equilibrium | 20,000 / 20,000 |
| Every equilibrium has count $k^*$ | **20,000 / 20,000** |
| Multiple equilibria (identity multiplicity) | 3,455 (**17.3%**) |
| … including one with the richest absent | 959 (≈ 4.8%) |
| Sufficient condition below holds | 11,141 |
| … and the equilibrium is then unique | **11,141 / 11,141** |

Identity multiplicity is not a knife-edge curiosity; under this sampling it
occurs in a sixth of instances.

### What is salvaged, and it is substantial

**The count is unique across all pure-strategy equilibria** — not merely
within the assortative construction. Proof sketch (not yet formalised): let
$S$ be any equilibrium with $|S| = k$. Every member has
$\kappa_i \le \Delta(k{-}1)$, every outsider $\kappa_j > \Delta(k)$. If
$k > k^*$: the $k$-th smallest cost satisfies
$\kappa_{(k)} \le \Delta(k{-}1)$, contradicting the maximality of $k^*$ under
single crossing ($\kappa_{(j)} - \Delta(j{-}1)$ increasing in $j$). If
$k < k^*$: single crossing gives $\kappa_{(k+1)} \le \Delta(k)$, and since
only $k$ agents are in $S$, some outsider has cost at most
$\kappa_{(k+1)} \le \Delta(k)$ and profitably deviates in. Hence $k = k^*$.

This is a genuine **strengthening of P5**: the equilibrium count is an
invariant of the game, not an artefact of the constructed equilibrium. It is
discrete, order-theoretic, and Lean-able in the existing style
(hypotheses: $\Delta$ antitone, $\kappa$ sorted, single crossing).

**Identity uniqueness under one extra condition.** If
$\kappa_{(k^*+1)} > \Delta(k^*{-}1)$ — the first excluded agent would not
enter even at the most favourable slot — then every count-$k^*$ equilibrium
must draw its members from $\{1,\dots,k^*\}$, so the assortative prefix is
the **unique** equilibrium. The experiment confirms: in all 11,141 sampled
instances satisfying the condition, exactly one equilibrium exists. Without
it, the assortative equilibrium can still be defended as a *selection* (it
minimises total realised entry cost among count-$k^*$ equilibria, since it
fills the slots with the lowest-$\kappa$ agents), but that is an argument to
be made, not a fact to be asserted.

### Why the formal layer did not catch this

The Lean development's `Enters`/`EntersAt` machinery takes the assortative
assignment as given: `cost j` *is* $\kappa(w_{(j)})$, i.e. the $j$-th slot is
occupied by the $j$-th richest by construction. Prefix structure and cutoff
uniqueness are proved *within that schedule*. No theorem quantifies over
arbitrary entrant sets, so no theorem could have failed. This is the same
failure class as the earlier false-green episodes (the silent Lean skip, the
P7 end-to-end check): **the prose generalised beyond the layer that was
verified.**

### Consequences

- `PROOFS.tex` §Primitives: "identities … pinned down by the wealth
  ordering" must be corrected to: the **count** is deterministic and
  invariant across equilibria; the assortative equilibrium always exists and
  pins identities within it; identity uniqueness holds under the stated
  condition, and otherwise assortativity is a selection.
- `LITERATURE.tex` positioning: "the only device that makes identity a
  property of the agents" must be weakened to the count/assortative-selection
  form — still a genuine advance over mixing, arrival order, and random
  delays, since those leave even the *assortative* outcome undefined.
- **P9, P9-gen, P9-str, P7, P8, P-MU are unaffected in substance**: every
  comparative static in the paper is a statement about $k^*$, the count, and
  the count is exactly what survives. P9-gen's marginal-entrant condition
  $w_{(k^*)}$ vs $x_0$ references the sorted profile, not a particular
  equilibrium's membership.
- New work item: formalise count-uniqueness-across-equilibria in Lean; state
  the identity-uniqueness condition; re-run suites.

---

## Finding 2 — the utility function is never displayed

The entry condition compares $\kappa(w) = u(w) - u(w-c)$, a **utility**
increment, against $\Delta(m) = V\,\mathbb{E}[\varphi(M_m)]$, a **prize value
times probability**. Nowhere in §Primitives is the payoff function written
down. The comparison is licensed only by the implicit form

$$U_i \;=\; u\big(w_i - c\,e_i\big) \;+\; V\cdot\mathbf{1}\{i \text{ wins}\},$$

additively separable, with the prize entering *linearly in utility units* and
valued identically by rich and poor.

This is not a defect of the model — it is exactly Schroyen–Treich's
*privilege contest* $U = u(w-x) + \pi r$, which the survey verified is the
right taxonomy cell, and the rank-allocated non-market prize of
Cole–Mailath–Postlewaite is precisely the justification for $V$ not entering
$u$: the prize is not fungible with wealth, so there is no wealth effect on
the benefit side and the *only* channel for inequality is $\kappa$. Stated,
this is a strength (it is what makes P8's separation and P9's
sign-determinacy clean). Unstated, it is the first question a referee asks.

**Fix:** one displayed equation and three sentences in §Primitives: the form
above; the remark that risk aversion operates on wealth but not on the prize,
with the CMP justification; and the observation (already proved as P1) that
$V$ then enters all results only as a scale factor.

---

## Finding 3 — wealth-independence of the score draws is implicit

Proposition `prop:anon` (anonymity) — $\Delta(j-1)$ depends on the number of
entrants only — is the hinge of P9: it is why a wealth spread moves only the
cost side of the entry condition, so the partial effect is the full effect.

It requires that the draws $r_i, s_i$ are **independent of wealth**.
§Primitives says the draws are "independent from arbitrary distributions",
which reads as independence across players; independence from $w_i$ is never
stated. If ability correlated with wealth ($s_i \sim S(\cdot \mid w_i)$),
then $F$ and $G$ would depend on *which* wealths enter, anonymity would fail,
and the no-feedback argument behind P9 would collapse — a spread would move
$\Delta$ as well as $\kappa$.

This is one sentence, and it is a substantive modelling commitment worth
owning explicitly: the model isolates the pure *affordability* channel by
construction. (It also marks a genuine boundary of the results: economies
where wealth buys ability are outside the theorems' scope, and honestly so.)

**Fix:** add "with $(r_i, s_i)$ drawn independently of the wealth profile" to
§Primitives, and a sentence at `prop:anon` noting this is where the
assumption bites.

---

## The novelty verdict

With twelve papers read, reconstructed, and checked (`lit/`), the honest
hierarchy of the model's claims:

**P9-gen carries the paper.** The unoccupied cell, stated precisely: *the
sign of a mean-preserving wealth spread's effect on the equilibrium entry
count — global, distribution-free, requiring only $u'' < 0$, with the flip
located at the pivot versus the marginal entrant's wealth.* The neighbours,
each verified to occupy a different cell:

| Neighbour | Their cell | Verified separation |
|---|---|---|
| Schroyen–Treich 2016 | Intensive margin; local second-order; needs $u'''$ and CSF decisiveness | ST-12a: same $A$, different $P$, opposite signs |
| Hopkins–Kornienko 2004/09/10 | No extensive margin; dispersive order for wages/welfare | HK passes; their order *implies* our hypotheses (Dispersive.lean) |
| Costrell–Loury 2004 | Pivot logic, but fixed margin $\theta$, wage schedule | CL pass; disanalogy stated in `PROOFS.tex` |
| Entry literature (LS, FJL, FL, MOS, MW) | Identical or private-cost agents; no wealth sorting | MOS-6: $\binom{6}{2}=15$ entrant sets vs one; MW-4: flat vs rank-dependent threshold |
| Lazear–Rosen 1981 | Scheme choice; DARA; example, not theorem | LR-3: quadratic utility separates the channels |
| Ryvkin–Drugov 2020 | Fixed contest, no entry margin | RD-4/6: same kernel, reversed crossing |

**Crucially, P9-gen is robust to Finding 1**, because it is a theorem about
the count and the count is the cross-equilibrium invariant.

**P7 is corollary-grade support.** The bundle now says so itself: the
accounting is FJL/Fu–Lu's; what is P7's own is integrality, utility units,
and no designer — real, but thin. It should never again be presented as a
headline result.

**P-MU is a clean characterisation** on unoccupied territory (the
$Q$-dimension, which Schroyen–Treich explicitly did not undertake), whose
weight is now known to be Ryvkin–Drugov's kernel. Analogue-grade; honest as
support.

So the answer to "does the model add anything new" is: **yes, if and only if
P9-gen's chain is sound** — and this report's three findings are the current
known gaps in that chain. All three are repairable; none touches the theorem
itself; two of the three, once repaired, make the paper *stronger* (the
displayed privilege-contest form, and count-invariance as a new result).

---

## Repair plan, in order

1. **Primitives paragraph** (Findings 2 + 3): display $U_i$; state
   wealth-independence of draws; add the CMP sentence for why $V$ is outside
   $u$. Half a page.
2. **Correct the identity prose** (Finding 1) in `PROOFS.tex` §Primitives and
   the survey's positioning paragraph; state the count-invariance result and
   the identity-uniqueness condition; present assortativity as existence +
   selection otherwise.
3. **Formalise count uniqueness across equilibria** in Lean (new section in
   `EntryContest.lean`; hypotheses: sorted $\kappa$, antitone $\Delta$,
   single crossing; conclusion: any Nash entrant set has size $k^*$). Add a
   Python cross-equilibrium enumeration check to the suites so the claim is
   never again prose-only.
4. **Re-run both suites**, update `VERIFICATION.md`, repackage.
5. Then — and only then — the deferred items in their previous order
   (welfare on the corrected LS reading; the open-item-2 monotone-weight
   attempt; P-MU shape).

---

## A note on method

All suites were green while a central prose claim was false. The lesson is
the recurring one of this project: **verification protects exactly the layer
that is verified, and not one sentence more.** The count-uniqueness theorem
and the enumeration check in the repair plan exist precisely to move the
injured claim from prose into the checked layer.
