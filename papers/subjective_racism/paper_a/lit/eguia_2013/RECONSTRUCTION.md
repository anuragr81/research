# Eguia (2013), reconstruction

Jon X. Eguia, "Discrimination and Assimilation", working paper, New York
University, 10 September 2013. 40 pages. The published version is "Discrimination
and assimilation at school", *Journal of Public Economics* (2017), which was not
read (EG-D5).

## Source read

- Cached at `~/.cache/paper_a/sources/eguia_2013_wp.pdf` (356,670 bytes),
  sha256 `8a54c300…df1b9e`, with the text layer extracted to
  `~/.cache/paper_a/sources/eguia_2013_wp.txt`, sha256 `1d3565e6…781126`.
  The text has 41 form-feed segments. Segment `n − 1` is PDF page `n`, and PDF
  page `n` is printed page `n`. Segment 40 is empty.
- Every page, 1 to 40, was read from the text on 16 October 2026, so the
  evidence tag is [F]. Pages 11, 19, 30, 31, 32 and 33, whose equations the
  extraction garbles, were also read from 110 dpi images rendered with
  `pdftoppm`. Nothing beyond these pages is attributed. The add-on file of
  footnote 18 (p. 27), which holds the formal versions of the three
  generalizations, was not read (EG-D6).

## The model, from its own primitives

1. **Agents** (p. 6). A unit mass. Half have the advantaged background `𝒜`
   with endowment `e_𝒜 > 0`, half the disadvantaged background `𝒟` with
   endowment 0. Ability `θ_i` is private information and uniform on `[0, 1]`
   in each background. Background therefore carries no information about
   ability.
2. **Social groups** (pp. 6–8). Two sets of norms, `A` and `D`. All of `𝒜`
   belong to `A`. A fraction `λ ∈ (0, 1]` of `𝒟`, the set `𝒟_Y`, may choose to
   join `A` (`a_i = 1`). The rest of `𝒟` stays in `D`.
3. **Cost of assimilation** (p. 8). `a_i d c(s_i)`, with `c` positive,
   continuously differentiable and strictly decreasing in skill. The difficulty
   `d ≥ 0` is set by a finite subset `𝒜_F` of size `N`. Each member proposes
   `d_i`, and `d` is the largest value that at least `n` members propose, an
   order statistic, so the rule is strategy-proof.
4. **Utility** (eq. 1, p. 9). `ψ(e_i, s_i) + v(s_i, e_J, s_J) − a_i d c(s_i)`.
   The externality `v` depends on one's own skill and on the **average**
   endowment and **average** skill of one's social group `J`, with
   `v_{e_J e_J} ≤ 0`, `v_{s_J s_J} ≤ 0`, `v_{e_J s_J} ≥ 0`, `v_{s_i e_J} = 0`
   (p. 10). Nobody forms a belief about another individual's type. The group
   enters only through its realised means.
5. **Timing** (p. 10). `𝒜_F` sets `d`. Each agent chooses skill
   `s_i ∈ [0, θ_i]`, costless in Sections 1 and 2, so `s_i = θ_i`. Members of
   `𝒟_Y` then choose `a_i`. The solution concept is perfect Bayesian
   equilibrium by backward induction.
6. **Peer pressure** (Section 3, pp. 16–20). A low-ability member `m` of `𝒟`
   chooses a threshold `s^P_𝒟`, and members of `𝒟_Y` with skill above it pay
   a fixed `K > 0`. The analogous `l ∈ 𝒜` chooses `s^P_𝒜`.

## Derivation chain of the main results

- **Composition** (eq. 3, p. 30). If members of `𝒟_Y` assimilate if and only
  if their skill exceeds `s`, then `e_A(s) = e_𝒜/(1 + λ(1 − s))`,
  `s_A(s) = (1 + λ − λs²)/(2 + 2λ(1 − s))` and
  `s_D(s) = (1 − λ + λs²)/(2 − 2λ(1 − s))`. These are the member means of the
  groups the cutoff produces (`eq3_sA`, `eq3_sD`), and the masses times the
  means add to the population total (`total_mean`). For an interior cutoff,
  `s_A > 1/2` (`sA_gt_half`), `s_D < 1/2` (`sD_lt_half`) and `e_A < e_𝒜`
  (`eAbar_lt`).
- **Cutoff** (pp. 10, 29–30). Since `c` falls in skill, `a_j = 1` implies
  `a_i = 1` for `s_i > s_j`, so assimilation is a cutoff rule. `d(s)` is the
  difficulty at which the agent with skill `s` is indifferent when the groups
  have the composition that the cutoff `s` itself produces. The equilibrium is
  a fixed point on composition. Under either sufficient condition (small `λ`,
  or `c′/c` very negative) `d(s)` is strictly increasing (pp. 30–31).
- **Choice of `d`** (pp. 31–33). Each `i ∈ 𝒜_F` prefers the cutoff
  `s*(s_i) = argmax_s v(s_i, e_A(s), s_A(s))`. The optimum is interior for low
  `e_𝒜` and above 1/2, from the first-order condition (eq. 6). Preferences
  over `s`, hence over `d`, are single-peaked, and the order-statistic rule is
  strategy-proof (Moulin 1980). This gives Proposition 1, a unique equilibrium
  with cutoff `θ* ∈ (1/2, 1]`, `θ* < 1` for a small endowment gap, and, if
  `v_{s_i s_J} ≥ 0`, lower-skilled members of `𝒜_F` choosing higher `d`.
- **Proposition 2** (p. 15, proof pp. 33–35) is comparative statics in
  `e_𝒟` from eq. (9).
- **Proposition 3** (p. 19, proof pp. 35–38). `s^P_𝒜 = 1` because more skill
  in `A` helps `l`. `s^P_𝒟 < γ` in every equilibrium because `m` gains by
  keeping able members of `𝒟_Y` below the assimilation cutoff.
- **Example 4** (pp. 19–20) gives numbers for `e_𝒜 = 4`, `λ = 0.1`,
  `c = 1/s`.
- **Screening** (p. 27). A cost that falls with skill can admit the able and
  turn away the rest (`screen_exists`). A lump-sum barrier on an immutable
  trait cannot (`control_lump_sum_no_screen`).

## What could not be reconstructed

- The three generalizations of the appendix (immutable traits, intrinsic
  preferences over diversity, a symmetric society) are given in words only,
  pp. 27–29. Their formal versions are in the unread add-on file.
- The general existence and uniqueness argument of Proposition 1 rests on
  continuity in `λ` and on the sign of `c′/c`, without explicit bounds. Only
  the composition algebra (eq. 3), the derivative of `s_A` on p. 32 and the
  screening logic are formalised.
- Example 4's column without peer pressure (`θ* = 0.610`, `d* = 1.341`,
  `s_D = 0.487`) was reproduced by a numerical grid search for reading only.
  It is not a Lean result and no row relies on it. The peer-pressure column was
  not reproduced.
