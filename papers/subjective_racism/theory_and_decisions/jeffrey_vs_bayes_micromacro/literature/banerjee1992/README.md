# Banerjee, A.V. (1992), "A Simple Model of Herd Behavior"

*Quarterly Journal of Economics* 107(3), 797-817. (Drive copy: JSTOR scan,
stable URL 2118364. All 22 PDF pages read.)

## Claims formalized

### The basic model (Sec. II, pp.802-803)

* Options are indexed by `i ∈ [0,1]`, and exactly one option, `i*`, pays `z`. The
  prior on `i*` is uniform.
* Each agent receives a signal with probability `α`. A signal is true with
  probability `β`; otherwise it is uniform on `[0,1]`.
* Agents move in a fixed order. Each sees her predecessors' choices, but not
  whether they had a signal.
* Tie-breaks A, B and C (p.803).

### The claims

* **Lemma 1** (p.805): if agents 1 and 2 both chose `ī ≠ 0`, agent 3 should follow
  them even if her own signal says otherwise. The paper displays both posterior
  weights.
* **Proposition 1** (p.806), decision rule `D`: once an option other than `0` has
  been chosen by more than one person, an informed agent joins it unless her
  signal matches an already-chosen option.
  * Sec. IV.A (p.807): "If the first person chooses `i ≠ 0` and the second person
    follows her, the third person will always follow them. All subsequent decision
    makers will also choose the same option."
* **The herd probability** (p.808, repeated p.810): the probability that no one
  chooses the right option, "however large the population", is
  `Π = [1 - α(1-β)]⁻¹(1-α)(1-β)`.
  * `Π` is decreasing in `α` and `β`, and it is close to 1 for small `β` (p.800
    item 2, p.808).
  * This contrasts with independent choices, where someone is right with
    probability tending to 1.
* **The `D*` expression** (p.810): `1 - (1-αβ)^{n-1} - (n-1)(1-αβ)^{n-2}αβ`.
* **The herd externality** (p.799): the joiner's "choice therefore provides no new
  information to the next person in line".
* **Sufficient statistics** (p.809): "in our model the choices made by agents are
  not always sufficient statistics for the information they have … this lack of
  invertibility is what causes the sufficient property to fail."

## Result

`lean/Banerjee.lean` is a symlink to `lean/Literature/Banerjee.lean`. It was
checked with `lake env lean Literature/Banerjee.lean`. It has no `sorry`, and the
headline theorems use only `[propext, Classical.choice, Quot.sound]`.

### Lemma 1

* `wBar` and `wPrime` build the two posterior weights factor by factor from the
  model.
* `lemma1_weights`: the first weight equals the printed
  `α³β²(1-β) + α²β(1-β)(1-α)`. The second is `α²β(1-β)(1-α)`.
* `lemma1`: `wPrime < wBar`. The difference is `α³β²(1-β)`.
* `printed_second_weight_lt`: the second weight as printed carries an extra
  factor `β`, so it is strictly smaller than the model's weight. The lemma is true
  a fortiori.

### The herd externality

* `followProb` gives the probability that an agent facing a herd on `ī` chooses
  `ī` under rule `D`, for three cases:
  * `i* = ī`;
  * `i*` is another already-chosen option;
  * `i*` has not yet been chosen.
* `herd_follow_uninformative`: this probability is exactly 1 both when the herd is
  right and when `i*` has not been chosen. Joining the herd therefore leaves the
  posterior between these hypotheses unchanged. It is informative (`1 - αβ`) only
  against `i*` being another chosen option.

### The equilibrium path and `Π`

* `chain` models the equilibrium path under `D` on four states: only `0` chosen;
  distinct wrong options; a wrong herd (absorbing); `i*` found (absorbing).
* `pi_hasSum`: `Π` is the sum of the herding-path series
  `(1-β) Σ_k (α(1-β))^k (1-α)`.
* `no_one_correct_ge_Pi`: for **every** `N`, `Pr(no one chooses i*) ≥ Π`. The
  proof conserves the eventual-success value along the chain.
* `tendsto_no_one_correct`: `Pr(no one chooses i*) → Π` as `N → ∞`. The transient
  mass is at most `(1-αβ)^N`.
* `Pi_antitone_alpha`, `Pi_antitone_beta` and `Pi_beta_zero`: `Π` decreases in
  each parameter, and `Π = 1` at `β = 0`.
* `independent_tendsto_zero`: without observation, the probability that no one is
  right is `(1-αβ)^N → 0`.
* `dstar_formula`: the p.810 expression equals `Σ_{j≥2} C(n-1,j)(αβ)^j(1-αβ)^{n-1-j}`.

### The sympy check

`sympy/check_banerjee.py` runs 30 checks; all pass, and it exits 0.

* The restaurant example (prior .51, one A and one B signal cancel).
* Lemma 1's weights derived symbolically from the generative model.
* `Π` as a series.
* `Π` as the limit and lower bound of the exact finite-`N` chain, for three
  `(α, β)` pairs up to `N = 200`.
* The derivatives of `Π`.
* The `D*` expression against the binomial tail for `n ≤ 8`.
* The welfare normalisation.

## What formalizing revealed

**Banerjee's action is not coarse.** The choice set is the continuum `[0,1]`, the
same space as the signals. The sufficient-statistic failure comes from the
equilibrium **decision rule**, which maps a wide range of signals to one option.
In the herd state, every signal that does not match an already-chosen option maps
to the herd option. `herd_follow_uninformative` makes this exact: the follower's
choice has likelihood 1 under both "herd right" and "`i*` unchosen".

Banerjee attributes this non-invertibility to "how we specify the payoffs" (p.809).
He conjectures invertibility when "the space of choices and the space of signals
are of comparable dimension and the payoff function is continuous". In his model
the dimensions are comparable, and only the payoff is discontinuous (see also
p.816). BHW make the same diagnosis on p.1002: Banerjee's incorrect cascades
"derive from a degenerate payoff function". Banerjee's own "coarse" example
("machines, for example, come in only a small number of sizes", p.809) is offered
for *other* settings.

**The herd is absorbing because joining is uninformative.** In `chain`, a wrong
herd can be left only if some later signal matches an already-chosen option. That
happens with probability 0 unless `i*` was chosen. This is why the probability
that no one is correct stays above `Π` for every `N`: the herd, once formed,
persists.

**Slips in the paper (none affect its conclusions):**

* Lemma 1 (p.805): the second posterior weight is printed with an extra factor `β`.
  The model gives `α²β(1-β)(1-α)`.
* p.810: the displayed expression is described as "the probability that at least
  two people have *not* received the true signal". It is the probability that at
  least two people *have* received it (`dstar_formula`), which is what the argument
  needs.
* pp.810-811: the lower bound `z[N - n(ε)](1-ε)/N` is per capita and tends to
  `z(1-ε)`, not to `N(1-ε)` as printed. The comparison bound `zN[1-Π]` is a total.
  The argument goes through once both are per capita: `z(1-ε)` against `z(1-Π)`.

## Bearing on Paper B

`PAPER_B_MANUSCRIPT.tex` lines 254-267 use Banerjee in three places:

* "consider herd-behaviour and informational cascades in which sequence matters";
* "In \citet{Banerjee1992}, the sequence persists because a coarse public action
  fails to be a sufficient statistic for private information";
* "the sequence effect in \citep{BHW1992, Banerjee1992} lives on the analogue of the
  marginals".

1. **"the sequence persists because … fails to be a sufficient statistic".** The
   core is supported: p.809 names the failure of sufficient statistics as the
   source of the herd externality.
   * The formalization also supports a *persistence* reading. The wrong herd is
     absorbing because joining it is uninformative (`herd_follow_uninformative`).
     That is why `Pr(no one correct) ≥ Π` for every `N` (`no_one_correct_ge_Pi`).
   * "The sequence persists" is still loose. What persists is the herd, whose
     location is set by the first few signals (p.800 item 3).
2. **"coarse public action".** This is inaccurate for Banerjee: his action space is
   a continuum matching the signal space. The accurate term is his own: the
   decision rule lacks invertibility, because of the discontinuous payoff.
   * "Coarse" is accurate for BHW, whose action is binary. The manuscript's
     contrast ("on the other hand") therefore puts the coarseness on the wrong
     paper.
   * The two mechanisms are the same. In both, the herd or cascade action has
     likelihood 1 under the relevant states (compare BHW `cascade_uninformative`).
3. **"on the analogue of the marginals".** This has no referent. The state is the
   single `i* ∈ [0,1]`, and no joint or association structure exists in the model.
4. **Proposed corrected wording** (text only, not applied). It is the same joint
   sentence as in `literature/bhw1992/README.md`:

   > \citet{BHW1992} and \citet{Banerjee1992} study informational cascades and herd
   > behaviour, in which the order of moves matters: agents observe predecessors'
   > actions but not their signals. In both, the equilibrium action is not a
   > sufficient statistic for the agent's private information (a binary action in
   > \citet{BHW1992}; in \citet{Banerjee1992}, a decision rule that maps a wide range
   > of signals to the same option), and a cascade---an action that does not depend
   > on the private signal \citep[p.~1000]{BHW1992}---therefore conveys no
   > information, so later signals are no longer aggregated.

   If a Banerjee-specific sentence is wanted:

   > In \citet{Banerjee1992}, a herd once formed persists because joining it reveals
   > nothing: choices are not sufficient statistics for the information behind them
   > (p.~809), and the probability that no one finds the right option stays bounded
   > away from zero however large the population (p.~808).

## Audit findings (2026-09-29)

These findings are checked against `verify_updating_theory.md` §4 (N1-N5).

* **N1 (VERIFIED):** agreed. The §V.B nuance also agrees: rule `D` uses only the
  distribution of choices, which is why the four-state `chain` suffices.
* **N2 (VERIFIED-WITH-CAVEAT):** agreed that p.809 supports the sufficient-statistic
  core. Two qualifications:
  * The audit treats "coarse" as a paraphrase of "lack of invertibility". For
    Banerjee's own model it is not a paraphrase, because the action space is not
    coarse. The non-invertibility is in the equilibrium rule, driven by the
    discontinuous payoff (p.809, p.816; BHW p.1002).
  * The audit says Banerjee does not present the sufficient-statistic failure as
    the reason something "persists". The formalization shows the persistence of
    the herd *is* a direct consequence: joining is uninformative, so the wrong herd
    is absorbing and `Pr(no one correct) ≥ Π` for all `N`. The manuscript's
    "persists" is defensible if it refers to the herd rather than "the sequence".
* **N3 (VERIFIED):** agreed.
* **N4 (UNSUPPORTED):** agreed. The single scalar state `i*` has no marginal and
  association structure.
* **N5:** not re-checked (bibliographic).
* **New, not in the audit:** the three slips listed above: the Lemma 1 extra `β`,
  "have not received" on p.810, and the welfare normalisation on pp.810-811.

## Not formalized

* The full equilibrium strategy of Proposition 1 over arbitrary histories. This
  includes the off-path specification left open in fn 16, and the uniqueness
  argument (forward induction).
* The four-state `chain` is an abstraction of rule `D`. It is exact for the event
  "someone has chosen `i*`" because a false signal matches a chosen option with
  probability zero. That measure-zero step is argued, not formalized.
* The implementation of `D*` by incentive schemes.
* Proposition 2 (rewards for being first; `k*`).
* Sections V.B-C and VI.
