# Reconstruction of Fryer and Loury, "Valuing Identity"

**Source read.** Roland G. Fryer, Jr. and Glenn Loury, NBER Working Paper
16568 (December 2010), read in full [F]. The text was extracted with
`pdftotext -layout` and cached at `~/.cache/entry_contest/w16568.txt`. Pages are
printed pages (PDF page minus 2). The published version is "Valuing
Diversity", *JPE* 121(4), 747-774 (2013), which is not checked.

**Why this paper.** The binary-investment search of 13 October 2026 found it as
the closest paper to C12 and C13. The lead came from Fang and Moro's survey
(NBER WP 15860, §6.6.3), which describes an unpublished 2008 version.

## Model (Section 2)

A unit mass of agents in two identity groups `a` and `b` draw an effort cost
`c` from group distributions `G_a` and `G_b`. Assumption 1 is that `g_a/g_b`
is strictly decreasing, so group `b` is disadvantaged. Each agent chooses
effort `e ∈ {0,1}`. Productivity `µ` then has law `F_1` after effort and `F_0`
without it, where Assumption 2 is that `f_1/f_0` is strictly increasing.
Agents are assigned to a continuum of slots `z ∈ [0,1]`, and an agent in slot
`z` produces `µ β(z)` with `β` increasing. A direct mechanism `(Z, P)` assigns
slots and prices.

## Results

1. **Laissez-faire (Lemma 1, Theorems 1-2).** The efficient assignment is
   top-down, `Z(µ) = F(π, µ)`, and is implemented by a unique incentive
   compatible, balanced mechanism. The private return to effort,
   `∫ W* (f_1 - f_0) dµ`, equals the social return, so laissez-faire investment
   is efficient.
2. **The group gap (p.11).** Under laissez-faire,
   `s_b(z) - s_a(z) = (π_a - π_b) ΔF(µ(π,z)) > 0`: group `b` sits uniformly
   lower in the hierarchy. In the two-job version (p.20) group `b` holds a
   smaller fraction of the slots.
3. **Sighted policy (Lemma 2, Theorem 3).** The efficient policy prefers group
   `b` at assignment and gives no direct subsidy for skills.
4. **Blind policy (Theorem 4, Lemma 3, Proposition 1).** A blind ex post policy
   caps the slot price and rations at random. A universal skills subsidy is
   efficient if and only if disadvantaged agents are more prevalent on the
   development margin than on the assignment margin.

## What could not be reconstructed

Nothing material. The concluding remarks (pp.25-26) contain a draft passage
with a conjecture marked "(I think!)" (FL-D4).
