# Wagner, C.G. (2002), "Probability Kinematics and Commutativity"

*Philosophy of Science* 69(2), 266-278. The copy used is the author's preprint (14 pages,
its own pagination, no journal page numbers); page references below are to the preprint.

The manuscript cites this paper (`\citep{Wagner2002}`) at MS:117 (footnote), MS:171,
MS:216-217 and MS:277, for the claim that the Bayes-factor benchmark $\PB$ is a
sequence-free reference and that Bayes-factor updating predicts no sequence effect. The
paper extends Field (1978) from finite to countable families. It also proves a partial
converse: under conditions (4.3)-(4.4), the Bayes-factor identities are **necessary** for
commutativity, not only sufficient.

## What the paper proves

Schema (3.1) (p. 4) has two routes. Route 1 is $p\xrightarrow{E}q\xrightarrow{F}r$ and
route 2 is $p\xrightarrow{F}q'\xrightarrow{E}r'$. The second-step targets need not match:
$r'(E_i)$ may differ from $q(E_i)$, and $q'(F_j)$ from $r(F_j)$. This fully general schema
is **Field's case**: "Field (1978) was the first to identify conditions sufficient to
ensure that $r'=r$ in this setting, in the special case where E and F are finite" (p. 4).
The matched-target schema (2.7) (p. 3) is the special case that Diaconis and Zabell
treated (Remark 3.4).

- **Theorem 3.1** (p. 4). The identities (3.2) $\beta_{r',q'}(E_{i_1}:E_{i_2})=\beta_{q,p}(E_{i_1}:E_{i_2})$
  and (3.3) $\beta_{q',p}(F_{j_1}:F_{j_2})=\beta_{r,q}(F_{j_1}:F_{j_2})$ imply $r'=r$.
- **Theorem 4.1** (p. 7). Suppose (4.3) $\forall i_1\forall i_2\exists j:\ p(E_{i_1}F_j)p(E_{i_2}F_j)>0$
  and (4.4) $\forall j_1\forall j_2\exists i:\ p(E_iF_{j_1})p(E_iF_{j_2})>0$. Then $r'=r$
  implies (3.2) and (3.3). **Remark 4.1** (p. 8): (4.3)-(4.4) hold when E and F are
  *qualitatively independent* (every $E_iF_j\neq\emptyset$) and $p$ is strictly coherent.
  Full support alone is not enough: with $F=E$, (4.3)-(4.4) fail whatever $p$ is, and
  $r'=r$ can hold while (3.2) fails (the example opening Section 4).
- Diaconis-Zabell's matched-case sufficiency (Remark 3.4) and necessity (Remark 4.3) of
  Jeffrey independence follow as corollaries, through Theorem 3.2. So Wagner's own new
  contribution is necessity for the general schema. Jeffrey (1988)'s probability-factor
  result is also a corollary (Remark 3.3).
- **Remark 5.1** (p. 10) answers Garber (1980) directly. Under Wagner's formulation "we learn
  nothing new from repeated glances and so all Bayes factors beyond the first are equal to
  one" (note 8).
- Note 9 (p. 13), in a discussion of Lange (2000), is the source of the phrase
  "*considered* experience (in light of ambient memory and prior probabilistic commitment)".
  This phrase is sometimes attributed to Wagner 2003, which does not contain it.

## A transcription trap worth recording

The text layer drops primes. `pdftotext` renders $\beta_{r',q'}$ as `βr ,q `, which reads
like the unprimed $\beta_{r',q}$, a cross-route quantity. That misreading was tested first.
It gives a clean, reproducible counterexample to the misread "theorem". Only the rendered
page (Theorem 3.1, p. 4) settles the matter: condition (3.2) is $\beta_{r',q'}$, with both
primes. A second, independent bug turned up in the same pass. $q'(E_i)$, the derived
$E$-marginal of $q'$, is not the $F$-update's own target $q'(F_1)=g$. Conflating the two
produces the same kind of spurious counterexample.

## Lean

`lean/Wagner2002.lean` is a symlink to `lean/Literature/Wagner2002.lean`. The file builds
with no `sorry`. Every theorem, 75 in all, uses only `[propext, Classical.choice, Quot.sound]`.
The setting is a finite $\Omega$, with finite families given as labellings
$E:\Omega\to\iota$ that cover $\Omega$. This is Field's finite case, proved with Wagner's
argument. Countable families are not formalized.

| Lean | Paper |
|---|---|
| `bf`, `pf`, `eq13` | (1.1)-(1.3) |
| `kin`, `ComesByPK`, `eq21`, `eq23` | (2.1)-(2.3), and "comes from $p$ by probability kinematics" |
| `thm31` | **Theorem 3.1** |
| `eq35`, `eq37`, `remark31` | (3.5)-(3.8); Field's geometric-mean form (3.9)-(3.10) |
| `remark32`, `remark33` | Remark 3.2; Remark 3.3 (Jeffrey 1988 as a corollary) |
| `thm32_E`, `thm32_F` | **Theorem 3.2** |
| `remark34`, `remark34_indep` | Remark 3.4 (Diaconis-Zabell as a corollary; $p$-independence gives Jeffrey independence) |
| `thm41` | **Theorem 4.1**, with (4.3)-(4.4) stated as Wagner states them |
| `remark41_qi`, `remark41_FeqE` | Remark 4.1: qualitative independence plus strict coherence gives (4.3)-(4.4); $F=E$ violates them |
| `sec4_FeqE`, `sec4_example` | Section 4's opening example: $r'=r$ while (3.2) fails (1 vs 1/3) |
| `remark42`, `remark43` | Remarks 4.2 and 4.3 |
| `bf_cond`, `note8` | (1.1) likelihood-ratio remark; Remark 5.1 / note 8 (Garber) |
| `remark52`, `remark52_bf` | Remark 5.2 ($F=E$ in (2.7)) |
| `eq55`, `eq55_exists`, `note7` | Remark 5.3, (5.5); for finite families (5.3) always holds; note 7 |

The project's question is kept in a separate, labelled part of the file. It is not Wagner's.

| Lean | Content |
|---|---|
| `benchmark` | Paper B's $\PB(\omega)\propto p(\omega)\,[x_i/p(E_i)]\,[y_j/p(F_j)]$ |
| `PB_endpoint`, `PB_endpoint_exists` | $\PB$ is the common endpoint of schema (3.1) when each cue's first-position revision is its delivered credence against the prior, and the second-position revisions carry the same Bayes factors |
| `PB_unique` | With that anchoring, and (4.3)-(4.4), $r'=r$ forces $r=\PB$ |
| `grid_43_44` | On Paper B's grid with a full-support prior, (4.3)-(4.4) hold |
| `completion` | Any route $p\to q'\to r'$ has a partner route that satisfies (3.2)-(3.3) and ends at $r'$ |
| `wagner_does_not_single_out_PB` | A numerical instance: the B-first Jeffrey sequence (36/55 at cell EF) is a Wagner-consistent commuting endpoint, and $\PB$ gives 27/40 (the A-first sequence gives 27/50) |

## SymPy

`sympy/check_theorems.py` runs 26 exact checks, prints PASS/FAIL for each, and exits 0 iff
all pass (26/26). It covers:

- Theorem 3.1 on the symbolic $(\alpha,\beta,c)$ 2x2 prior, and the fact that the common
  endpoint is $\PB$.
- Theorem 4.1 on a full-support prior. With route 2's first step left free there is a
  one-parameter family of commuting schemas, all Bayes-factor-consistent, with *different*
  endpoints. With both first steps fixed, the completion is unique and equals $\PB$.
- The $\PB$ / $J_{AB}$ / $J_{BA}$ numbers, and the completion that ends at $J_{BA}$.
- Section 4's $F=E$ example.
- Field's geometric-mean form.
- (5.5).
- Note 11's countable example: each term is $(2/21)(16/3)^j$, so the sum diverges.

## What formalizing changed

1. **Theorem 4.1 does not make $\PB$ "the" benchmark.** An earlier version of this README said
   it did, calling Bayes-factor consistency "the only route". Theorem 4.1 says that a
   commuting schema must be Bayes-factor-consistent across positions. It does not say at
   which position a cue's Bayes factor is read. `completion` and
   `wagner_does_not_single_out_PB` make this exact. Read each cue's Bayes factor in first
   position, against the prior, and the commuting endpoint is $\PB$ (`PB_unique`). Read the
   A-cue's Bayes factor in second position, and the commuting endpoint is the B-first Jeffrey
   sequence, a different number. What selects $\PB$ is the modelling choice that a cue's
   evidential content is its Bayes factor against the prior. Wagner does not make that
   choice. His Principle II (modified) takes identical learning to *be* identical Bayes
   factors, whatever the position.
2. **Matched targets are the Diaconis-Zabell case, not Field's.** An earlier version said the
   Lean development covers "matching-target routes, i.e. Field's simpler special case". The
   matched-target schema (2.7) is the Diaconis-Zabell case (Remark 3.4). Field's theorem is
   the general finite schema (3.1).
3. **(4.3)-(4.4) need qualitative independence, not just full support** (Remark 4.1;
   `remark41_FeqE`). Paper B's two cue partitions are qualitatively independent, so the
   theorem applies there (`grid_43_44`).

## What the manuscript may and may not attribute

**May attribute to Wagner (2002):**
- Updates by fixed Bayes factors commute. Wagner shows that if each cue carries the same
  Bayes factor in either position, the two orders agree (Theorem 3.1), for any countable
  partitions. The MS:117 footnote ("commute when their inputs are held fixed in Bayes-factor
  form") is accurate.
- The MS:216-217 sentence may stay: combining the two cues' Bayes-factor content is
  sequence-invariant, and $\PB$ is the single update on that combined content. A more exact
  form: "Combining the two cues' Bayes factors, each computed against the prior, gives the
  same result in either order \citep[Thm 3.1]{Wagner2002}; $\PB$ is that result."
- A converse, if wanted: "Conversely, the two orders can agree only if the second-position
  revisions carry the first-position Bayes factors \citep[Thm 4.1]{Wagner2002}; the
  theorem's conditions hold here because the two cue partitions are qualitatively
  independent and the prior has full support."
- MS:277's "Bayes-factor updating predicts no sequence effect anywhere" holds in the paper's
  finite setting. A tighter form: "updating by fixed Bayes factors is order-invariant
  \citep{Wagner2002}". Wagner's own edge cases are countable families, where the matched
  revision may not exist (Remark 5.3, note 11), and $F=E$ (Remark 5.2). Neither arises in
  Paper B.
- MS:171 ("used as a sequence-free reference \citep{Wagner2002}") is fine as a citation for
  order-invariance.
- The Section 6 two-channels sentence (2026-10-03), "commutation requires the same factor in
  either position \citep{Wagner2002}", is the necessity direction: Theorem 4.1, whose
  conditions hold for the paper's prior when all four cells are positive (`grid_43_44`). The
  orders the same sentence states (marginals at order zero, association at first order, odds
  ratio unchanged) are the paper's own, from `verify_ladder.py` (F), not Wagner's.

**Must not attribute to Wagner (2002):**
- That $\PB$ is *the* or the *unique* sequence-free benchmark, or that Theorem 4.1 singles it
  out. The anchoring at the prior is the paper's own choice (see "What formalizing changed",
  item 1).
- That identical learning *is* identical Bayes factors as a theorem. This is Wagner's
  normative proposal (Principle II modified, p. 9). The theorems show that it is sufficient
  and, under (4.3)-(4.4), necessary for commutativity.
- The phrase "considered experience" to Wagner 2003. It is in note 9 of this paper.
