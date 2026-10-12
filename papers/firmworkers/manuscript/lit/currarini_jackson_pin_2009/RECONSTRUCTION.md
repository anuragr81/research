# Reconstruction of Currarini, Jackson and Pin, 2009

**Source read.**

| Short | Full | Copy | Evidence |
|---|---|---|---|
| CJP | "An Economic Model of Friendship: Homophily, Minorities and Segregation", working paper dated "January 2007, Revised: July 29, 2008", marked "forthcoming: Econometrica", 46 numbered pages on 47 PDF pages; published as *Econometrica* 77(4), 2009, pp.1003–1045 | PDF with text layer (pdfTeX), Drive `netminority-1.pdf`, id `1CxQjRabMr_khbGaWgWytPNasEAgDN7uR`, uploaded 12 Oct 2026; cached `~/.cache/firmworkers/currarini_jackson_pin_wp.pdf`, sha256 `cc2bb2b47dec2ff9d3a231651475bc0d8c8a2b8a001585de1ac9ec25bff1f562` | [F], accepted working paper; the Econometrica pages are not read |

Text extracted with `pdftotext -layout`. Every page ends with its number; two
PDF pages carry the number 2 (an overflow page with the keywords, then the
Introduction), so printed page `p ≥ 2` is form-feed block `p` and page 1 is
block 0. Nothing is quoted from the overflow page. Read in full on
12 Oct 2026, including Appendices A–D. The working paper is cited as the
Econometrica article with `version_read` (author's rule of 10 Oct 2026).

**Why this paper.** The novelty check for the closure parameter `σ` (M29) and
for its identification from the share of cross-group partners (X25,
`Isc.closure_from_crossShare`): a bias in meetings toward one's own type is
CJP's model of opportunity-driven segregation, and their inbreeding-homophily
index (Coleman 1958) is the established measure.

## Primitives (§2, §4)

- Types `i` with population shares `w_i`; homophily index
  `H_i = s_i/(s_i + d_i)` (Definition 1), baseline `H_i = w_i`, inbreeding
  homophily `IH_i = (H_i − w_i)/(1 − w_i)` (Definition 6, after Coleman 1958).
- Agents enter a continuous-time matching process at cost `c` per unit time,
  accept all meetings, and choose when to exit, with utility `U(s, d)` over
  same- and different-type friends (strictly concave in each, diminishing
  returns overall).
- A matching process maps stocks `M_i` to meeting probabilities `q_ij`, balanced
  so that `M_i q_ij = M_j q_ji`; unbiased if `q_ij = M_j/M`.

## Derivation chain

1. **Data (§3).** Relative homophily, larger groups form more friendships, and
   inbreeding homophily is largest for middle-sized groups.
2. **Agent's problem (Lemma 1).** `q U_s + (1 − q) U_d = c`.
3. **Example (§4.5).** `U = (s + γd)^α`: `t = (α/c)^{1/(1−α)}(γ + (1 − γ)q)^{α/(1−α)}`;
   with `α = ½`, `γ = ½`, `w = (⅔, ⅓)`: `q₁ = √3 − 1`, `q₂ = 2 − √3`.
4. **Benchmarks (Propositions 1–2).** Type-neutral preferences give equal
   numbers of friends; with unbiased matching, if one type is homophilous
   another must be heterophilous.
5. **Two types, unbiased (Propositions 3–5).** Existence and relative homophily;
   same-type bias makes the larger group form more friends and inbreed, the
   smaller one heterophilous; identity (6), `IH₂ = (w₁ − q₁)/w₁`.
6. **Biased matching (§7).** `q₁^β + q₂^β = 1`, `β > 1`, lets all groups inbreed;
   fit `α = .2, β = 2, γ = .8, c = .04`; preferences explain friend counts,
   matching bias explains inbreeding.
7. **Many types (Proposition 6), welfare (Propositions 7–8).** With `U`
   homogeneous of degree `α`, a group's welfare is proportional to its number of
   friends.

## What was reconstructed

The indices and their signs, identity (6) and its consequence, Proposition 2
for finitely many types, the first-order condition of the §4.5 example and its
closed-form solution, the welfare proportionality behind Proposition 7, and a
link to the firmworkers closure parameter: under weights `1` and `1 − σ`, with
`n` same-group and `m` other-group partners available, `σ = IH/H`. See
`CLAIMS.md`.

## What was not reconstructed

Propositions 1, 3–6 and 8–10, the matching foundation of Appendix A, the
fitting exercise and the welfare figures.
