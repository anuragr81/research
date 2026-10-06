# Notes — Cole, Mailath & Postlewaite, pass 1

Run: `python3 verify_cmp.py` — **4 checks, 0 failures**.

**Scope.** This is the thinnest verification directory in the set, and
deliberately so: almost everything we take from these two papers is
**conceptual**, and conceptual claims are confirmed by reading, not by
computation. The four checks cover the *use* we make of them, not the papers.
Nothing here reproves any CMP result.

---

## The citation instruction is sound

The survey's next-step 2 — cite CMP92/95 where `V` is introduced, "since they
justify a rank-allocated non-market prize" — holds up. Both halves are in the
sources:

- **A prize allocated by rank rather than sold.** CMP95 §4: *"To make the land
  example analogous to our models, we should have the land simply given away,
  with the best given to the wealthiest, and so on."*
- **Status as the ranking device.** CMP92 abstract: *"We interpret an agent's
  status as a ranking device that determines how well he or she fares in the
  nonmarket sector."*

Their examples are close to the paper's motivating cases: *"Country club
memberships, charity board invitations, university trusteeships, invitations to
chic parties, and assigned seats in churches and synagogues."*

And the generalisation gives the licence directly (CMP95 §4): *"Whenever an
increase in an individual's position in the wealth distribution by itself
increases the likelihood of obtaining desirable outcomes, optimal individual
behavior will exhibit some of the qualitative features exhibited in the models
analyzed above."*

---

## CMP-C confirmed verbatim — with one qualification

The handover cites "Cole–Mailath–Postlewaite 1995 §4 (welfare theorems do not
apply to prizes allocated by rank)". The source says exactly that:

> "when the desirable goods or decisions are allocated as prizes rather than
> sold, the standard welfare theorems regarding the Pareto optimality of the
> outcomes no longer apply."

**Qualification: §4 is *Concluding Comments*.** This is a discussion section,
not a formal result — there is no theorem, no proof, no stated conditions. It
is an authoritative conceptual claim by three authors who have just built the
models, and it should be cited that way: as motivation for why a welfare
analysis is needed, not as a result that does any work. A referee who follows
the citation expecting a proposition will not find one.

This matters because the handover's welfare plan lists CMP95 §4 alongside
Levin–Smith Prop 3 and Becker–Murphy–Werning §7 as though they were the same
kind of object. They are not: LS Prop 3 is a proposition with a proof; CMP95 §4
is a paragraph. Both are usable, for different purposes.

---

## The finding: CMP's conceptual premise generates LS's welfare function

This is what makes the directory worth having.

CMP95's point is conceptual — prizes given by rank are not sold, so the welfare
theorems lapse. Levin–Smith's is algebraic — with `V_n ≡ V` the marginal
entrant's social gain is zero while her private gain is positive. **They are
the same point, and the bridge is one line.**

Take CMP's premise literally: one prize of value `V`, delivered to the
top-ranked competitor, delivered iff at least one competitor enters (CMP-1: the
delivered prize mass is `V` regardless of the count, so `∂/∂n = 0`). With
independent entry at probability `q` among `N`, each entrant paying `c`:

> `S = V·Pr(at least one entrant) − (expected total entry cost)`
> `  = [1 − (1−q)^N]V − qNc`

which is **Levin–Smith equation (8) exactly** (CMP-2).

So the deferred welfare section has a conceptual authority (CMP95 §4) and a
formal apparatus (LS §I.A) that agree, and deriving one from the other takes a
sentence. CMP-3 states the resulting wedge arithmetically: social marginal
value of an entrant beyond the first is `0`, her private gain is `Δ(m) ≥ κ > 0`.
That is the same wedge as LS-7's business stealing.

---

## Why we may import the justification without the machinery

`V` enters our model **only as a multiplicative scale factor**. P1 gives
`Δ(m) = V·E[φ(M_m)]`, so `∂Δ/∂V = E[φ]` is free of `V`, `∂²Δ/∂V² = 0`, and `Δ`
is exactly homogeneous of degree one in `V` (CMP-4).

Nothing in P1, P3, P5, P7, P8 or P9 depends on *where* `V` comes from — only on
its being fixed and rank-allocated. That is the licence to cite CMP for the
foundation without inheriting their matching model, their CRRA specification,
or their multiple-equilibria machinery. Worth stating in one sentence at the
point of citation, because it pre-empts "why are you citing a marriage-matching
model?"

---

## Results

| ID | Result |
|---|---|
| CMP-1 | CONSISTENT. Delivered prize mass is `V` independent of the count; `∂/∂n = 0`. |
| CMP-2 | CONSISTENT, and the useful one. The rank-allocation premise reproduces LS eq. (8) exactly. |
| CMP-3 | CONSISTENT. Social marginal value `0`, private gain `Δ > 0`; wedge equals the private gain. |
| CMP-4 | CONSISTENT. `Δ` homogeneous of degree one in `V`; `∂²Δ/∂V² = 0`. |

## Not attempted — which is most of both papers

1. **CMP92 §III's** equilibrium existence and characterisation, §III.D's
   aristocratic equilibrium, and all of **§IV** (capital accumulation, growth
   trajectories, multiple equilibria). None of the formal results were checked.
2. **CMP95 §2** (effort model, complete information) and **§3** (incomplete
   information and signaling) — the models that *generate* the concern for
   relative rank — were not reconstructed.
3. CMP95's Appendix §5.

**CMP-D** (multiple equilibria explaining growth differences) is context only
and is unchecked.

Everything we actually use sits in CMP92's abstract and §II and CMP95's §4, all
of which is prose. Anything else drawn from these papers is **[A]-grade**
despite the [F] tags, and the artifacts here do not change that.
