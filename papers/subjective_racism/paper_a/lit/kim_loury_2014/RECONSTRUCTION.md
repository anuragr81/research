# Kim and Loury (2014), reconstruction

Young-Chul Kim and Glenn C. Loury, "Collective Reputation and the Dynamics of
Statistical Discrimination", MPRA Paper No. 54950, version of 31 March 2014.
Published as *International Economic Review* (2018), which is not read.

## Source read

- `~/.cache/paper_a/novelty/mpra54950.pdf`, 38 pages, 461,177 bytes, sha256
  `599ca923…04eb`, with its text layer `mpra54950.txt`, sha256 `12a1657a…36e9`.
  Cached on 15 October 2026. The MPRA cover (PDF p. 1) is dated 12 August 2012
  and says the paper was posted on 3 April 2014. The title page (PDF p. 2) is
  dated 31 March 2014. The version is cited as the 2014 working paper
  (KLB-D1).
- Every page was read on 16 October 2026 from the text layer, namely the paper (PDF
  pp. 2–24), Appendix A (pp. 25–27), Appendix B (pp. 28–32), the references
  (p. 33) and the five figures (pp. 34–38). The evidence tag is [F]. PDF pages
  8, 9, 10, 11, 12, 16, 18 and 23, where the text layer breaks equations, were
  also read as 110 dpi images rendered with `pdftoppm`. The images are
  reproducible from the pinned PDF and are not pinned separately.
- PDF page `n` is the paper's page `n − 1`.
- MPRA Paper 18765 (2009), "Group Reputation and the Dynamics of Statistical
  Discrimination", is an earlier version with a separate record (KLB-D2).

## The model, from its own primitives

1. **Tasks and payoffs** (PDF p. 6). Employers assign each worker to Task One
   or Task Zero. Task One pays the worker `ω` per period. The employer gains
   `xq` from a qualified worker in Task One and loses `xu` from an unqualified
   one, with `ρ = xq/xu`. Task Zero pays nothing to either side.
2. **Population** (PDF pp. 6–7). Group `i` has a constant population `Nⁱ`.
   Each worker dies at Poisson rate `λ` and is replaced by a newborn, who
   invests once, at birth. The cohort's costs take three values. A share `Π_l`
   has cost near zero, a share `Π_h − Π_l` has cost `cm`, and a share `1 − Π_h`
   has a cost above any return. So `G(c) = Π_l` on `(ε, cm)` and `Π_h` on
   `[cm, K)` (KLB-D9).
3. **Two markets** (PDF pp. 7–8). A fraction `n` of workers is in a temporary
   market, where employers see the group and a test signal `θ` and do not see
   qualification or age. Revelation of the true type arrives at Poisson rate
   `η` and moves the worker to a permanent market, where assignment follows
   the true qualification (KLB-Q5). A constant market size gives
   `λ' ≡ λ + η = λ/n`, eq. (1).
4. **Test** (PDF p. 8). The unqualified signal is uniform on `[0, θu]` and the
   qualified one on `[θq, θ̄]`, with `θq < θu`. The result is pass, fail or
   unclear. `Pq = (θ̄ − θu)/(θ̄ − θq)` is the chance that a qualified worker
   passes, and `Pu = θq/θu` the chance that an unqualified worker fails, with
   `0 < Pq < Pu < 1` (KLB-Q7, KLB-D7).
5. **Return to investment in the temporary market** (PDF p. 8, eq. 2). With
   the benefit of the doubt (BOD) given with probability `ξ`,
   `β(ξ) = ω(Pq + ξ(Pu − Pq))` (KLB-Q8).
6. **The employer's rule** (PDF p. 9). The posterior that an unclear worker of
   group `i` is qualified is `Πⁱ(1 − Pq)/(Πⁱ(1 − Pq) + (1 − Πⁱ)(1 − Pu))`. BOD
   is given iff `xq·P − xu·(1 − P) ≥ 0`, which is `Πⁱ ≥ Π* = (1 − Pu)/(ρ(1 −
   Pq) + 1 − Pu)` (KLB-Q10). So `ξ = 1` above `Π*` and `ξ = 0` below.
7. **The newborn's return** (PDF pp. 8–9, eqs. 3, 4). With discount rate `δ`
   and expected rules `{ξᵉ_τ}`, `R = ∫ β(ξᵉ_τ) e^{−(δ+λ')(τ−t)} dτ + ω'/(δ + λ')`
   with `ω' = ηω/(δ + λ)`. The newborn invests iff `R ≥ c`, so the cohort's
   investment rate is `φ = G(R)`.
8. **Consistency** (PDF p. 10). Employers' belief about each group equals the
   group's true share of qualified workers in the temporary market (KLB-Q13).

## Derivation chain

- **Static equilibria, Lemma 1** (PDF p. 9). `R({0}) = (ωPq + ω')/(δ + λ')` and
  `R({1}) = (ωPu + ω')/(δ + λ')`. If `cm` lies strictly between them and
  `Π_l < Π* < Π_h`, then `Π_l` and `Π_h` both reproduce themselves (KLB-Q12).
  Derived from items 2, 5, 6 and 7. Formalised as `lemma1`, with two controls.
- **Recovery date, Lemma 2** (PDF p. 10, eqs. 6, 7). Under the conjecture that
  BOD starts at `T`, `R = [ωPq + ω' + (ωPu − ωPq)e^{−(δ+λ')T}]/(δ + λ')`, and
  `R ≥ cm` iff `T ≤ (1/(δ+λ')) ln[(ωPu − ωPq)/((δ+λ')cm − ωPq − ω')]`.
  Formalised as `lemma2_iff`.
- **Law of motion** (PDF p. 11, eqs. 8, 9). `Π̇ = λ'(φ − Π)`. With `φ = Π_h`
  the path from `Π_l` reaches `Π*` at `T = (1/λ') ln[(Π_h − Π_l)/(Π_h − Π*)]`,
  eq. (10). Proposition 1, eq. (11), combines (7) and (10). Corollary 1,
  eq. (12), restates it in `δ` (KLB-D6). Formalised as `eq8_quotient`, `eq9`,
  `eq10` (with uniqueness of the solution, `linear_unique`), `prop1` and
  `cor1`. Paper A1 has no law of motion, and the record formalises the
  source's own.
- **Dynamic system, Theorem 1** (PDF pp. 12–13). `Π̇ = λ'(φ − Π)` and
  `V̇ᵉ = (δ + λ')(Vᵉ − β(ξ) − ω')`, with `φ = G(Vᵉ/(δ + λ'))`. `Π` is a flow
  variable and `Vᵉ` jumps. The steady states are `Q_l = (ωPq + ω', Π_l)` and
  `Q_h = (ωPu + ω', Π_h)`.
- **Overlap and trap** (PDF pp. 14–17, eqs. 15–19). Integrating along the
  optimistic and pessimistic paths gives `π° = Π_h + (Π* − Π_h) v*^{−λ'/(δ+λ')}`
  and `πᵖ = Π_l + (Π* − Π_l)(1 − v*)^{−λ'/(δ+λ')}`, with
  `v* = ((δ+λ')cm − ωPq − ω')/(ωPu − ωPq)`. Proposition 3 gives the overlap
  `[π°, πᵖ]`. Theorem 2 says a trap `[0, π°)` containing `Π_l` exists iff
  `(Π_h − Π*)/(Π_h − Π_l) < v*^{λ'/(δ+λ')}`. Both integrals are formalised as
  equal hitting times of the explicit solutions (`eq15_return`, `eq17`,
  `eq16_return`, `eq18`). Lemma 3 and Proposition 3 are formalised on
  `[Π_l, Π_h]` (`opt_feasible_iff`, `pes_feasible_iff`, `prop3`), and
  Theorem 2 as `theorem2` and `trap_iff`.
- **Comparative statics in `δ`** (PDF p. 16). The paper states that `π°`
  rises and `πᵖ` falls with `δ`, without proof. The first holds
  (`piO_strictMono_delta`). The second holds only with `v*` held fixed
  (`piP_anti_fixed_v`). Since `v*` rises with `δ` (`vSd_strictMono`), `πᵖ`
  in fact rises with `δ` whenever `ωPq > 0` and Lemma 1 holds
  (`piP_strictMono_delta`, with a numerical case in `piP_rises_example`).
  The proof compares `ln(1/(1 − v*))/(δ + λ')` at two rates, using
  `ln x ≤ x − 1` twice (KLB-D10).
- **Paths and welfare** (PDF pp. 18–19, 28–31). The paths spiral out from
  `((δ+λ')cm, Π*)` when the overlap is interior (Proposition 4, Corollary 2).
  The curves of eqs. (31) and (34) and Condition 1 of the proof are
  formalised (`va_eq31`, `va'_eq34`, `prop4_condition1`). `Q_h` Pareto
  dominates `Q_l` (Corollary 3, `cor3_medium`, `cor3_workers`,
  `cor3_employers`).
- **Policy** (PDF pp. 19–23). A training subsidy that lowers `cm` to `c̃m` of
  eq. (20), or a hiring standard `θ̃` in `[θ̃'', θ̃']`, makes recovery
  feasible. A standard below `θ̃''` lowers the return to investment and is
  "patronizing" (Proposition 5), and an equal-hiring quota can be.
  Formalised as `eq20`, `lemma4`, `betaHat_lt`, `betaHat_gt_iff`, `eq23` to
  `eq25`, `prop5` and `quota`.
- **Appendix A** (PDF pp. 24–26). With a monotone likelihood ratio, the hiring
  standard is `s(Π) = min{θ : ψ(θ) ≤ ρΠ/(1 − Π)}`, the return to investment is
  hump-shaped in `Π`, and with three steady states `Q_h` and `Q_l` are saddles
  and `Q_m` is a source (Lemma 5). Paths spiral near `Q_m` above a critical
  `(δ/λ')*` (Lemma 6). Formalised are eq. (26) for `Π < 1`
  (`sStd_antitone`) and the linear algebra of Lemmas 5 and 6 (`JE_trace`,
  `JE_det`, `lemma5_saddle`, `lemma5_source`, `lemma6`, `lemma6_no_real_root`,
  `lemma6_node`).

## What could not be reconstructed

The coverage table in `CLAIMS.md` lists every result. What is short of
general is the following.

- Eqs. (13) and (14) are formalised along paths whose flow return switches
  once, the only paths the paper uses, and not for a general expected path.
- Theorem 1 is formalised for its steady states. The direction arrows of
  Figure 1 are not formalised.
- Lemma 3 and Proposition 3 are formalised for reputations in `[Π_l, Π_h]`.
  The clause that the lower boundary is zero when `π° ≤ Π_l` rests on paths
  from below `Π_l`, which the paper reaches through the spiral (KLB-D12).
- Proposition 4 is formalised in Condition 1 only. Condition 2 (eqs. 37 and
  38) and the step from the two conditions to the spiral are not
  formalised, and neither are the region integrations of eqs. (29), (30),
  (32) and (33). Corollary 2 is not formalised.
- Eq. (26) is formalised for `Π < 1`. The hump shape of `β(Π)` and the
  existence of a steady state in Appendix A rest on assumptions about `F_q`,
  `F_u` and `G` that the paper does not state.
- Lemmas 5 and 6 are formalised in their linear algebra. The slope ordering
  at `Q_h` and `Q_l` is read from Appendix Figure 1, and the step from the
  linearised to the nonlinear system is not formalised.
- The figures (PDF pp. 34–38) are schematic and carry no numbers.
