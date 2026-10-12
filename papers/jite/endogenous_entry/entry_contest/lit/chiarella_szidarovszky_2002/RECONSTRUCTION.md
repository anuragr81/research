# Reconstruction of Chiarella and Szidarovszky, "The asymptotic behavior of dynamic rent-seeking games"

**Source read.** Carl Chiarella and Ferenc Szidarovszky, *Computers and
Mathematics with Applications* 43, 169-178 (2002). The author uploaded it on
Drive as `The_asymptotic_behavior_of_dynamic_rent`, expecting Corcoran and
Karels (1985). It was read in full [F] and checked on page images [P]. The text
was extracted with `pdftotext -layout` and cached at
`~/.cache/entry_contest/asymptotic_dynamic_rent.txt`. Pages are the printed
pages; the printed page is the PDF page plus 168. The equations do not survive
extraction, so they were read from the page images, which are cached in
`~/.cache/entry_contest/chiarella_pages/` and pinned by sha256.

**Why this paper.** The author uploaded it as the Corcoran and Karels paper. It
is not that paper and cites neither Corcoran paper. It is recorded because it
was read, and every paper read gets a record and Lean.

## Model (Section 2)

A fixed number `n` of agents compete for a prize worth one. Agent `i` spends
`x_i`, wins with probability `x_i / Σ x_j`, and pays `c_i(x_i)`. Each agent
moves its effort in continuous time in proportion to its marginal profit, at
speed `k_i > 0` (eq. 7). At an interior equilibrium with total effort `s̄`, the
Jacobian is `(D + a 1ᵀ)/s̄^3`. Here `D` is diagonal with entries
`d_i = k_i(-s̄ - s̄^3 c_i'')`, and `a` has entries `e_i = k_i(x̄_i - Q̄_i)`, where
`Q̄_i` is the rivals' total effort (eqs. 8-9). The second-order condition is
eq. (6), `c_i'' > -2 Q̄_i / s̄^3`.

## Results

1. **Eigenvalues (Section 3, eqs. 10-12).** Agents with the same `d_i` form a
   group `l` of size `m_l` with `γ_l = Σ e_i`. Then `d_l` is an eigenvalue of
   multiplicity `m_l - 1`, and the remaining eigenvalues solve
   `1 + Σ γ_l/(d_l - λ) = 0` (eq. 12). Theorem 1 reads stability from their
   signs.
2. **Case A, every `γ_l ≤ 0` (Theorem 2, Figure 1).** This is the case with no
   agent spending more than all the others together. All roots of eq. (12)
   are real, one below the smallest `d_l` and one between each pair of
   neighbours. Stability then depends on the signs of the `d_l` and on `g(0)`.
3. **Case B, a dominant agent (Theorem 3).** Only the dominant agent's group
   can have `γ_l > 0`, and that group has `d_l < -γ_l < 0` (eq. 18). At least
   `r - 2` roots are real, so there is at most one complex pair. The authors
   call a dominant agent "not very realistic" (p.176).
4. **Examples and limit cycles (Section 4).** Example 1 has two groups and
   shows that real and complex roots both occur. In Example 2 a pair of
   roots crosses the imaginary axis as `γ_3` passes 0.799, with
   `dλ/dγ ≈ 0.468 + 0.719 i`; the authors read this as a Hopf bifurcation and
   the possibility of limit cycles. They do not determine whether the cycles
   are stable.

## What could not be reconstructed

Nothing material. The paper has no entry decision, no entry cost and no wealth.
The number of agents is fixed, and every agent competes.
