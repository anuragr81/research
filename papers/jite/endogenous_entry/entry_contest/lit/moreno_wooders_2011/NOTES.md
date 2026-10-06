# Notes — Moreno & Wooders (2011), pass 1

Run: `python3 verify_mw.py` — **5 checks, 0 failures**.

**Scope.** Our reading is arithmetically consistent. One phrase in
`PROOFS.tex` is imprecise and should be tightened; two findings improve the
text rather than correct it. Nothing here reproves any result of the paper.

---

## The `PROOFS.tex` citation is accurate

This matters more than usual: Moreno–Wooders is the only literature paper
`PROOFS.tex` cites for a substantive modelling contrast. §Primitives says the
alternative of privately known entry costs

> "yields a symmetric Bayesian threshold in cost space and a *binomial*
> entrant count, as in \citet{MorenoWooders2011}; there the number of entrants
> is random and the identities are not determined."

Against p.320:

> "an entry strategy for a buyer can be described by a **threshold `t ∈ [c,c̄]`**
> indicating the maximum entry cost for which the buyer enters [...] If all
> buyers employ the same threshold `t`, then **the number of bidders follows a
> binomial distribution `B(N, H(t))`**."

Threshold ✓. Cost space ✓. Binomial ✓. Random count ✓ (MW-1: variance
`N·H(1−H) > 0`).

---

## One imprecision: "the identities are not determined"

Strictly, in Moreno–Wooders the identities **are** determined once cost draws
are realised — buyer `i` enters iff `z_i < t`, which is a deterministic rule
given `z_i`. What is undetermined is *ex ante*: since costs are private and
random, **which** buyers turn out to have low draws is a random variable.

So the accurate phrasing is that the entrant **set is random**, not that
identities are indeterminate. This matters because "identities not determined"
is the phrase Levin–Smith and Morgan–Orzen–Sefton use for a genuinely different
problem — with *identical* agents there is no rule at all that picks who
enters, which is why those papers need mixing or arrival order. Moreno–Wooders
does not have that problem; it has heterogeneity, and a perfectly good rule.

**Suggested tightening** for `PROOFS.tex` §Primitives:

> ... yields a symmetric Bayesian threshold in cost space and a *binomial*
> entrant count, as in \citet{MorenoWooders2011}; there the number of entrants
> and the identity of the entering set are both random, being determined by
> unobserved cost draws.

This keeps the contrast and avoids conflating two distinct difficulties. The
surrounding paragraph already distinguishes them correctly for the
identical-agent case, so the fix is local.

---

## A sharper statement of the separation than the survey gives

The survey and `PROOFS.tex` locate the difference in the *information
assumption*. Correct, but it leaves the mechanism implicit. **MW-4** makes it
mechanical:

| | Moreno–Wooders | P5 |
|---|---|---|
| threshold lives in | cost space | the wealth **order** |
| threshold shape | **flat** — one number `t` for everyone | **rank-dependent** — `κ(w_(j)) ≤ Δ(j−1)` |
| entrant count | `Binomial(N,H(t))`, variance `> 0` | deterministic, variance `0` |

Under private costs every buyer faces the same expected utility `U(v,H(t))`, so
the comparison value **cannot** depend on rank. Under complete information the
`j`-th richest knows exactly how many richer rivals enter ahead of her, so her
comparison value is `Δ(j−1)`, which falls in `j` by P3-cor. A rank-dependent
rule collapses to a flat one exactly when `Δ` is constant — which, by strict
antitonicity, never happens.

That is a one-line, checkable statement of why the two models are not
relabellings of each other, and it is stronger than "we assume complete
information and they do not".

---

## A welfare finding for the deferred section

Their **Proposition 3** — *"A screening value and an admission fee both equal
to zero maximize social surplus"* — supplies a third benchmark:

| source | environment | verdict on free entry |
|---|---|---|
| Levin–Smith Prop 3 | CV, homogeneous costs | **excessive** |
| Levin–Smith Prop 6 | IPV, homogeneous costs | optimal |
| Moreno–Wooders Prop 3 | IPV, **heterogeneous private** costs | optimal (constrained) |

**Cost heterogeneity does not flip the verdict** (MW-5). What flips it is
whether `V_n` varies with `n` — LS-7's criterion. Our model has heterogeneous
costs *and* a fixed prize; it is the fixed prize that puts it in the
excessive-entry branch, not the heterogeneity.

This is worth stating in the welfare section, because *"your entry costs are
heterogeneous, so Levin–Smith's homogeneous-cost result may not transfer"* is
the obvious referee objection, and Moreno–Wooders answers it directly.

Note also their `W*` is a **constrained** maximum — constrained to symmetric
independent entry. Under complete information our `k*` is not so constrained,
which is a further difference to keep in view when the welfare objective is
written down.

---

## Results

| ID | Result |
|---|---|
| MW-1 | CONSISTENT. Count is `B(N,H(t))`; at `N=10, H=1/3` variance is `20/9 > 0`. |
| MW-2 | CONSISTENT. Totally differentiating `(3)` gives `dt*/dφ = 1/(U_H h − 1)` and `dt*/dv = −U_v/(U_H h − 1)`; with `U_H < 0`, `h > 0`, `U_v < 0` both are negative, as Prop 2 states. |
| MW-3 | CONSISTENT. MW variance `20/9 > 0` against P5 variance `0` on an explicit profile. |
| MW-4 | CONSISTENT. `Δ` strictly decreasing across ranks, so P5's rule never collapses to a flat threshold. |
| MW-5 | CONSISTENT. With a fixed prize `V_n − V_(n−1) = 0`; the branch is set by `V_n`, not the cost distribution. |

## Not attempted

- Proposition 1 and §2's preliminaries — the `U(v,p)`, `S(v,p)` representation
  everything rests on.
- Proposition 2's uniqueness argument (only the comparative statics were
  checked, and those from the displayed equation `(3)`).
- Propositions 3 and 4 beyond the displayed `(6)`–`(7)`.
- Propositions 5–10, the design results, and §5's example.
- Proposition MM-LS as they state it.

Anything leaning on these is **[A]-grade** despite the [F] tag.
