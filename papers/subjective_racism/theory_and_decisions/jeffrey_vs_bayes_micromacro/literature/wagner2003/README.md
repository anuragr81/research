# Wagner, C.G. (2003), "Commuting Probability Revisions: The Uniformity Rule" (In Memoriam Richard Jeffrey)

*Erkenntnis* 59(3), 349-364. The copy used is the JSTOR scan (cover page plus journal
pp. 349-364). Page references are journal pages. This paper is not in `bibliography.bib`,
and the manuscript does not cite it.

It does not mention Lange, Cassell or "considered experience". The phrase "*considered*
experience" belongs to Wagner **2002**, note 9 (see `../wagner2002/`).

## What the paper proves

"It is assumed throughout this paper that all probabilities are strictly coherent"
(p. 350).

- **Theorem 2.1** (p. 351), on a **purely atomic** σ-algebra (countably many atoms that
  partition Ω). Schema (2.1) has two routes, $p\to Q\to r$ and $p\to q\to R$. The atomic
  identities (2.2) $\beta^r_Q(A:B)=\beta^q_p(A:B)$ and (2.3) $\beta^R_q(A:B)=\beta^Q_p(A:B)$
  imply $r=R$. Remark 2.1 gives (2.6); Remark 2.2 gives formula (2.7),
  $r(A)=R(A)=[q(A)Q(A)/p(A)]/\sum q(A')Q(A')/p(A')$.
- **Section 3, observation-based revision** (pp. 352-355). This section is not about old
  evidence. **Theorem 3.1**: when $q$ comes from $p$ by kinematics on E, the atomic identities
  (3.5) hold iff $r$ comes from $Q$ by kinematics on E and (3.6) holds. **Theorem 3.2** is
  the Field/Wagner two-partition theorem, schema (3.10), for an *arbitrary* σ-algebra. This is
  the 2002 result restated (Remark 3.1), derived from Theorem 2.1 through Theorem 3.1.
  Remark 3.2 calls Theorem 2.1 "the fundamental result".
- **Section 4, explanation-based revision** (pp. 356-359). This is the old-evidence
  application. **Theorem 4.1**: on the atoms of $\{H,E\}$, (4.5) holds iff the three
  conditional Bayes factor identities (4.6)-(4.8) hold.
- **Section 5** (pp. 360-362) has **three** indices: the difference $d=\delta$, the
  normalized difference $D$, and the probability factor $\pi$. Since $D=\pi-1$, "$D$ and
  $\pi$ clearly stand or fall together". There are three criteria:
  - I: commutativity.
  - II: "learning identical to that prompting a probability-kinematical revision should
    prompt a probability-kinematical revision **on the same partition**".
  - III: not unduly restricting the priors.

  Wagner's verdict: "the index $d$ fails to satisfy II, and its satisfying I is vitiated by
  its failure to satisfy III. The index $\pi$ satisfies both I and II, but this is vitiated
  by its failure to satisfy III." So $d$ fails **two** criteria, not one. The $d$-failure of
  II is a **concrete numerical counterexample** in **note 4** (p. 363):
  $p=(.4,.1,.1,.4)$, $q=(.64,.16,.04,.16)$, $p'=(.2,.2,.3,.3)$ and $q'=(.44,.26,.24,.06)$
  on $HE,\bar HE,H\bar E,\bar H\bar E$. Here $q$ comes from $p$ by kinematics on
  $\{E,\bar E\}$ and $q'-p'=q-p$, but $q'(H|E)=.6286\neq .5=p'(H|E)$. The $\pi$-index
  "reaches the point of absurdity" with two atoms: unless $Q=p$, no $r$ exists (note 5).
- **Theorem 5.1** (p. 362): Bayes factors never constrain the prior probability of any
  single atomic event. With finitely many atoms the incompatibility "never materializes".

## Lean

`lean/Wagner2003.lean` is a symlink to `lean/Literature/Wagner2003.lean`. The file builds
with no `sorry`. Every theorem, 31 in all, uses only `[propext, Classical.choice, Quot.sound]`.
The algebra is finite, so it is purely atomic, with its atoms as the elements of a finite
type. All probabilities are strictly coherent, as in the paper.

| Lean | Paper |
|---|---|
| `bf`, `bfa`, `pia`, `eq14` | (1.1)-(1.2), (1.4) |
| `thm21`, `eq23_iff_eq24` | **Theorem 2.1** and its proof step (2.3) ⇔ (2.4) |
| `remark21`, `eq27`, `eq27_exists` | Remark 2.1 (2.6); Remark 2.2 (2.7); on a finite algebra (2.7) always defines $r=R$ |
| `IsPK`, `eq33`, `thm31` | (3.1)/(3.3); **Theorem 3.1** |
| `thm32` | **Theorem 3.2**, derived from `thm21` via `thm31` as in the paper. Wagner's σ-algebra reduction is not needed on a finite space |
| `remark32` | Remark 3.2 |
| `condBF`, `thm41` | (1.3); **Theorem 4.1** (4.5) ⇔ (4.6)-(4.8) |
| `dI`, `DI`, `DI_eq` | the three indices; $D=\pi-1$ |
| `critI_d`, `critI_pi` | criterion I for $d$ and $\pi$ |
| `critII_pi`, `note4_d_fails_critII`, `note4_conditionals` | criterion II: holds for $\pi$; fails for $d$ by **note 4**, exactly, with $q'(H\|E)=22/35\approx .6286$ |
| `critIII_d`, `critIII_pi` | criterion III failures |
| `pi_absurdity`, `absurdity_needs_learning` | the "point of absurdity" at two atoms (note 5) |
| `finite_never_materializes` | finite case of (5.3)-(5.5) and Theorem 5.1 |

Not formalized:
- the infinite purely atomic case, where (2.7) may fail to converge, and Theorem 5.1's
  infinite construction;
- (4.9)-(4.13).

One precision the formalization added: note 5 divides by
$\beta/\alpha-(1-\beta)/(1-\alpha)$, which is zero exactly when $q=p$. So "unless $Q=p$
there is no $r$" needs $q\neq p$, meaning some learning occurred. With $q=p$, $r=Q$ always
works (`absurdity_needs_learning`). This refines Wagner's statement; it does not contradict it.

## SymPy

`sympy/check_theorem21.py` runs 30 exact checks, prints PASS/FAIL for each, and exits 0 iff
all pass (30/30). It covers:

- Theorem 2.1 symbolically on a 4-atom algebra with no 2x2 product structure;
- (2.7);
- Theorem 3.2 on a 6-atom space with a 3-cell and a 2-cell partition and unmatched
  second-step targets;
- Theorem 4.1 in both directions;
- $D=\pi-1$ and criterion I;
- note 4 exactly;
- the $\pi$-absurdity, including the $q=p$ case;
- realizability of any Bayes factor vector.

## Corrections to the earlier version of this README

- It opened with "the 'considered experiences' fix ... load-bearing in the Lange/Cassell
  exchange". That material is Wagner 2002, note 9. This paper mentions neither Lange nor
  Cassell.
- It spoke of "two naive alternative indices". There are three ($d$, $D$, $\pi$). Criterion
  II is about the **same** partition, not "any partition".
- It called the $d$-failure of criterion II "not a single identity to check". Note 4 is a
  numerical counterexample, and it is now checked in Lean and SymPy.
- It said Theorem 2.1 "generalizes Wagner (2002) ... to an arbitrary atomic σ-algebra".
  Theorem 2.1 is for **purely** atomic algebras. The 2002 two-partition theorem reappears as
  **Theorem 3.2** (Section 3). The earlier README filed that section under "old evidence",
  but old evidence is Section 4 only.
- It said "the two alternatives each fail one of three criteria". $d$ fails II and III.

## Bearing on Paper B

Section 5 is the strongest available citation for "why not measure what a cue teaches by
$q(A)-p(A)$ or $q(A)/p(A)$". The difference index fails criterion II on a four-atom example
(note 4). The ratio index is vacuous at two atoms. Theorem 3.2 is the same commutativity
result as Wagner 2002's Theorem 3.1, restated under strict coherence, so it adds nothing to
what `../wagner2002/` licenses about $\PB$. If the manuscript ever cites this paper, it should
cite it for Section 5, and not for "considered experience".
