# Batch read notes — 1 Sep 2026

## RyvkinDrugov2020 — TE 15(4) 1587-1626 — PUBLISHED version read [F]
Setting: symmetric risk-neutral players, continuous effort e, output y=e+X additive iid noise cdf F pdf f, WTA prize=1, cost c strictly convex c(0)=c'(0)=0. Number of players K deterministic or stochastic (pmf p). NO entry margin. NO heterogeneity (explicitly assumed away, fn 3).
Key objects:
- individual effort: c'(e*_k) = b_k = (k-1)∫F^{k-2} f dF = E[f(X_(k-1:k-1))] (eq 3, 5, 9) — density of noise at the max of the other k-1.
- aggregate effort with quadratic cost: E*_k = k b_k = E[h(X_(k-1:k))] (eq 11) — hazard rate at 2nd-highest of k.
Results:
- Prop 1 / Cor 1: f unimodal ⇒ e*_k unimodal in k (Karlin 1968 variation-diminishing / log-supermodularity of F^{k-1}(1-F)).
- Cor 4: f increasing ⇒ effort increasing; f decreasing (and p1=0) ⇒ decreasing; constant iff f constant (uniform).
- Prop 2: f symmetric ⇒ e*_2=e*_3; symmetric unimodal ⇒ decreasing for k≥3.
- Prop 3: f IFR & c more convex than quadratic ⇒ aggregate increasing in K (FOSD); f DFR & c less convex & K≥2 ⇒ decreasing.
- Prop 4: h unimodal & quadratic cost ⇒ aggregate unimodal.
- Prop 5: SOSD in K, same mean, K≥2: log-concave f ⇒ uncertainty lowers aggregate; log-convex reverses.
- Prop 6: disclosure optimal iff c''' ≤ 0.
- Prop 7 (App A.1): existence/uniqueness of symmetric PSE via c''≥c0 > D(f bounds) and E(K|K≥1)c(c'^{-1}(f_m))<1.
- App A.2: multimodal f ⇒ effort has at most as many modes (TP_r).
- Tullock recovered as Gumbel noise + quadratic cost (§2.4, via Jia 2008): b_k = r(k-1)/k^2.
- Fn 29: Drugov & Ryvkin 2020 JET 188 105065 "How noise affects effort in tournaments" — dispersive order is NECESSARY AND SUFFICIENT to rank equilibrium effort across noise distributions for arbitrary sizes & prize schedules. ← dispersive order again, in the contest branch.
- Fn 5: Fu et al 2015 = restricting entry with symmetric agents + endogenous participation; this paper shows exclusion can be optimal with exogenous number.
Relation to entry_contest:
- P-MU: sign of dΔ(0)/dQ. Their b_k = E[f(X_(k-1:k-1))] is the intensive-margin marginal benefit; Δ(j) is the extensive-margin analogue (win-prob gain for the (j+1)th investor). Both are order-statistic-weighted functionals of the noise distribution; their unimodality-inheritance result (Prop 1) is the structural template for P-MU's hump-shaped weighting. MUST check PROOFS.tex P-MU statement to map W onto f or h.
- Contrast: they have no entry margin and identical agents; P5's k* is a count of heterogeneous entrants, their K is exogenous.
- The "no universal sign" message (p.1589: not a single prediction that cannot be reversed for some noise distribution) is exactly the framing for refuting R2's Q conjecture.
- Their Prop 7 existence conditions (concavity via bounds on f, f') are worth comparing to whatever P1-P3 assume.
Bib: author order Ryvkin, Drugov — confirmed from title page. Entry correct.
NEW BIB CANDIDATE: DrugovRyvkin2020b — "How noise affects effort in tournaments", JET 188 (2020) 105065 [from their ref list; verify].

## FuJiaoLu2015 — IJGT 44:387-424 — PUBLISHED version read [F]
Setting: M ≥ 2 IDENTICAL risk-neutral potential bidders; fixed entry cost Δ>0 (Assumption 1: Δ<V); simultaneous entry; entrants do NOT observe N; generalized nested lottery CSF with impact g; prizes capped at V (contingent on N allowed); bidding cost x^α, α≥1. Strategy = (q, μ(x)): entry prob + bid distribution. Symmetric equilibria only (asymmetric/semi-symmetric exist, §5.2).
Key results:
- Thm 1: symmetric eq exists (Dasgupta-Maskin 1986 multidim). q*∈(0,1]; if q*<1 payoff = 0.
- Def 1: q0 solves (1-q)^{M-1} V = Δ; any symmetric eq has q* ≥ q0.
- Eq (2) RENT ACCOUNTING: [1-(1-q)^M] V ≥ Mq(Δ + E x^α). ⇒ expected entrants Mq ≤ V/Δ · [1-(1-q)^M] ≤ V/Δ. ← THIS IS THE ACCOUNTING LOGIC BEHIND P7, in homogeneous mixed-entry form.
- Thm 2: upper bound on expected overall bid x̄_T(q) = (Mq)^{(α-1)/α}{[1-(1-q)^M]V - MqΔ}^{1/α}, single-peaked at q̂∈(q0,1]; q̂<1 iff V/M < αΔ/(α-1); q̂ decreasing in M.
- Cor 1: linear cost ⇒ optimum requires random entry.
- Thm 3/4: pure-strategy bidding iff r ≤ min{r0, r̄}; else must randomize bids.
- Thm 7: any r∈(0,α) + unique entry fee/subsidy F(r) achieves bound.
- Lemma 6: x̄*_T(M) decreases in M when V/M < αΔ/(α-1). Thm 9: optimal shortlist M* = M̄ or M̄-1, M̄ = min{N : V/N < αΔ/(α-1)} — i.e. designer excludes when pool exceeds ≈ (α-1)V/(αΔ).
- Cor 4: α=1 ⇒ invite exactly 2.
- Thm 11: q̂, x̄*_T, M̄ all decrease in Δ.
- §5.3: larger entry cost never desirable to designer.
Relation to entry_contest:
- P7 (k* ≤ V/κ): the accounting bound "entrants × entry cost ≤ prize mass" is ALREADY in their eq (2) for homogeneous mixed entry. P7's content is the DETERMINISTIC heterogeneous version: pure-strategy count k*, κ(w) in utility units, no designer, order statistics. Must state honestly: P7 is the extensive-margin heterogeneous analogue of a known accounting inequality, not a new inequality. Its value is that it holds for every Q with no limit and no designer.
- Their M̄ (shortlist) vs our k*: theirs is a design choice, ours an equilibrium count. Different objects; both scale like V/Δ.
- Entry structure: theirs symmetric MIXED (identical agents must randomize); ours pure threshold (heterogeneity breaks symmetry). Same point as Levin-Smith.
- Informational: their entrants don't see N; ours (check PROOFS.tex) — P5 is simultaneous complete info? verify.
- Thm 1 existence uses Dasgupta-Maskin; P5 existence is by direct construction. Note.
- Their "designer should exclude" (Thm 9) vs Ryvkin-Drugov's exclusion-with-exogenous-N: two different exclusion rationales, neither ours.
- Cite Cason-Masters-Sheremeta 2010 JPubE 94:604-611 (entry experiment) — from their refs; verify before adding.
- Cite Higgins-Shughart-Tollison 1985 Public Choice 46:247-258 — from refs; verify.
- Cite Samuelson 1985 Econ Lett 17:53-57 — from refs; verify.
- Cite Lu 2009 Econ Theory 38(1):73-103 "Auction design with opportunity cost" — from refs.
Bib entry FuJiaoLu2015 confirmed: vol 44, pp 387-424. Issue not shown on page header; earlier verification gave (2). Keep.
NOTE: extraction renders Δ (entry cost symbol) as blank — I inferred it from context. Fine.

## CornesHartley2012 — Manchester EDP-0806, May 2008 WP (→ Econ Theory 51(2) 2012) [F, WP version]
Setting: n≥2, logistic CSF p_i = f_i(x_i)/Σf_j, f_i concave production (A1), indivisible common rent R, continuous expenditure x_i ≥ 0, concave u_i (A2), complete info. No entry cost. Aggregative game; share-correspondence method (Selten fitting-in).
- Ex.1: n=10, u(c)=c−0.45c² (IARA): symmetric eq x=0.0563 all; asymmetric eq 3 active at 0.184, 7 inactive at 0. Active players' payoff HIGHER in asymmetric eq (fn 10). ⇒ risk aversion ⇒ multiplicity.
- Lemma 3.2: dropout value Ȳ_i = f_i'(0)[u_i(R)−u_i(0)]/u_i'(0); contestant inactive iff aggregate Y ≥ Ȳ_i. If f'(0)=∞ (Tullock r<1) all active (Cor 3.1).
- Thm 3.1 existence (A1,A2). Thm 4.1 symmetric contest ⇒ unique SYMMETRIC eq. Thm 4.2 regular ⇒ unique eq. Lemma 4.2 regularity: 2u'(R−x) ≥ u'(−x) on (0,R) (curvature bound). Cor 4.1 CRRA with initial wealth I_i regular iff I_i ≥ R(1−2^{−1/γ})^{−1}. Cor 4.2 small rent ⇒ regular. Cor 4.3 prudent + ARA ≤ 1/(2R) ⇒ regular. CARA regular [their 2003]. Open conjecture: non-increasing ARA ⇒ unique.
- Prop 5.1 symmetric: individual expenditure ↓ in n; regular ⇒ aggregate lobbying ↑. Prop 5.2 regular asymmetric: entry ↑ aggregate, ↓ incumbents' win prob & payoff.
- Prop 6.1: prudent + no one wins w.p.>1/2 ⇒ risk aversion lowers aggregate lobbying (extends Treich).
- Lemma 6.2: limiting dissipation ratio H[u] = [u(R)−u(0)]/[R u'(0)]; Cor 6.1 more risk averse ⇒ lower. Prop 6.2 large asymmetric: SELECTION — only types with max dropout value stay active; dissipation → H[u] of least risk-averse type.
- CRRA (12): u(c) = (I_i + c)^{1−γ}/(1−γ), I_i = initial wealth, additive shift, static. Rent and expenditure commensurable with wealth ⇒ S-T rent-seeking contest.
Relation to entry_contest:
- P5 CONTRAST (clean): their multiplicity is coordination among IDENTICAL agents on who is active (3-active/7-inactive), arising because the continuous aggregative best-response can support several fixed points under IARA. P5 has heterogeneous agents ordered by κ(w), so "who is active" is pinned by the order — no coordination problem, no curvature bound needed. P5 needs only κ decreasing; C-H need Lemma 4.2's 2u'(R−x) ≥ u'(−x). State this explicitly as the reason P5 escapes.
- Dropout value Ȳ_i is a curvature threshold on AGGREGATE effort; κ(w) is a curvature threshold on OWN resource. Different arguments, same idea "curvature selects who's active".
- Prop 6.2 selection by risk attitude (heterogeneous u_i, common wealth); ours selection by resource (common u, heterogeneous w). Complementary.
- Their "asymmetric eq preferred by active players" is a point R2's asymmetric-equilibria comment rhymes with.
- Bib: Cornes & Hartley 2003 Public Choice 117:1-25 "Risk aversion, heterogeneity and contests" [from refs; verify]; Konrad & Schlesinger 1997 EJ 107:1671-1683 [refs]; Millner & Pratt 1991 Public Choice 69:81-92; Hillman & Katz 1984 EJ 94:104-110.

## LevinSmith1994 — AER 84(3) 585-599 — PUBLISHED [F]
Setting: N identical risk-neutral potential bidders; fixed entry cost c paid BEFORE learning private value (ex ante; contrast Samuelson 1985 interim cost, fn 7); two-stage; n revealed before bidding (A4; irrelevant for IPV, fn 15). Symmetric equilibrium in MIXED entry strategies q*; n ~ Binomial(N,q*).
- p.586 & fn 2: prior literature (Smith 82/84, Engelbrecht-Wiggans 87/91, McAfee-McMillan 87b) assumed PURE entry ⇒ deterministic ASYMMETRIC eq with exactly n* enter, N−n* stay out — "the process by which potential bidders divide into these two groups is not explained". L-S restore symmetry by mixing. ← KEY for P5: with identical agents the pure threshold count n* exists but WHO is in is indeterminate; heterogeneity in κ(w) is exactly what pins it. P5 is the answer to L-S's "not explained".
- n* := unique integer with E[n*,m] ≥ 0 > E[n*+1,m] (p.587). Same object as k* for identical agents.
- fn 5: asymmetric pure eq (n enter, N−n out) and semi-mixed eq exist; no asymmetric eq with 0<q_i<q_j<1.
- Eq (1)/(6): zero-profit indifference defines q*. Lemma 1: ∂q*/∂e ≤ 0, ∂q*/∂c ≤ 0 iff Cov(profit, #rivals) ≤ 0.
- Prop 1: revenue-max mechanism induces socially optimal entry (entry fees OK, reservation prices not). Prop 2: revenue equivalence survives entry.
- Prop 3 (CV): free entry EXCESSIVE — business-stealing (Mankiw-Whinston 1986); seller taxes entry. Eq (9): (1−q*)^{N−1} V = c at social optimum (same form as FJL Def 1 q0!).
- Prop 6 (IPV): free entry OPTIMAL; private gain = social gain via V_n − W_n = n(V_n − V_{n−1}) (eq 18).
- Prop 7 (APV): second-price ⇒ optimal entry; first-price ⇒ excessive.
- Prop 8/9: market thickness θ = N/n*; beyond n*, welfare (and seller revenue at optimal mechanism) DECREASES monotonically in N — coordination costs of mixed entry; seller prequalifies/restricts pool.
Relation to entry_contest:
- P5: L-S's n* is k* for identical agents; their "unexplained division" is explained by κ(w) ordering. Cleanest citation for why P5's pure-strategy uniqueness is available and theirs isn't. (Same point as FJL, but L-S state the gap explicitly.)
- Welfare (R2 c.3): WTA fixed prize is CV-like (prize independent of n) ⇒ L-S Prop 3 says free entry excessive by business-stealing. That's the natural benchmark objective: social value of entry vs private. Template for the missing welfare section.
- P7 vs L-S thickness: L-S welfare ↓ in N via coordination; P7 bound is a count cap; both say "more potential entrants doesn't help beyond a point" but for different reasons.
- Their eq (9) and FJL Def 1 coincide: (1−q)^{N−1}V = c — the "win only if alone" reservation logic. Note if PROOFS.tex has an analogue (entry condition at j=0: κ(w) ≤ Δ(0) — Δ(0) is win prob gain when no other invests, ≈ analogous).
- Bib: Mankiw & Whinston 1986 RAND 17(1):48-58; Samuelson 1985 Econ Lett 17(1-2):53-57; McAfee & McMillan 1987 JET 43(1):1-19; Engelbrecht-Wiggans 1987 Mgmt Sci 33(6):763-770. [from refs]
Bib entry LevinSmith1994 confirmed: 84(3):585-599 June 1994.

## FuLu2010 — MPRA 945, Nov 2006 WP (→ Econ Inquiry 48(1) 2010) [F, WP version]
Setting: M≥3 IDENTICAL risk-neutral; fixed entry cost C>0; SEQUENTIAL entry with full knowledge of current participants (fn 6: this is what yields pure entry strategies); organizer budget Ω0, chooses prize V and per-entrant transfer S (fee if S<0, subsidy if S>0); ratio-form CSF with concave impact f; linear effort cost. Three stages.
- Lemma 1: symmetric eq effort e(N,V,S) = H^{-1}(V(1−1/N)/N), H=f/f'. Payoff π(N,V,S) = V/N − e + S − C, strictly decreasing in N.
- Lemma 2: N(V,S) = max{N : π(N,V,S) ≥ 0} — deterministic threshold count with identical agents; who enters = arrival order.
- Lemma 4/5: optimal contest ⇒ break-even + budget exhausted.
- Eq (7): E = Ω0 − N·C (total effort = budget − total entry costs, technology-independent) ⇒ Thm 1: optimal contest induces EXACTLY TWO entrants, E = Ω0 − 2C.
- Thm 2: V* = 4H(Ω0/2 − C); fee when C ≤ Ω0/2 − H^{-1}(Ω0/4), subsidy when larger.
- Thm 3: C=0 ⇒ any N∈{2..M} optimal, all yield Ω0.
- Conclusion: "allow for different types of contestants" is stated as future research — heterogeneity NOT done.
Relation to entry_contest:
- P5: F-L obtain a pure-strategy count with identical agents by SEQUENTIAL entry (arrival order breaks symmetry); P5 obtains it under SIMULTANEOUS entry by κ(w) heterogeneity. Two different symmetry-breaking devices; L-S mixing is the third. Worth a sentence contrasting all three.
- Eq (7) is another accounting identity of the P7 family (effort = budget − N·C).
- "exactly two" is a DESIGN result (organizer picks V,S); k* is positive under fixed rules. Not the same object.
- Confirms: the entire endogenous-entry design literature (FL 2010, FJL 2015) is homogeneous-agent.
Bib FuLu2010 entry: EI 48(1):80-88 (FJL 2015 ref list says 80-89; earlier IDEAS verification said 80-88; keep 80-88 with note). Title in FJL refs: "Optimal endogenous entry in tournaments" — DIFFERENT from MPRA title "Contest design and optimal endogenous entry". Earlier IDEAS verification gave EI title as "Contest Design and Optimal Endogenous Entry". FJL's ref may be wrong; flag for published-version check.

## CheGale1997 — Public Choice 92:109-126 — PUBLISHED [F]
Setting: N≥2 risk-NEUTRAL bidders, common value v, publicly known wealths w1>…>wN>0, HARD budget cap (no borrowing §3; credit at rate r §4). Compare all-pay auction (R=∞) vs lottery (R=1) vs standard auction. Linear utility; wealth enters ONLY as cap on bid. NO entry cost, NO participation margin (all-pay: passive bidders bid zero; lottery: all active).
- Prop 1 (all-pay): E[total expenditure] = min{v, w2}; only bidders 1,2 active; bidder 1 keeps surplus v−w2 if w2<v. Multiple eq, unique expected expenditure. High-wealth PREEMPTS low-wealth.
- Lemma 1-3 (lottery): bids weakly increasing in wealth; critical bidder i*: wealth > b* ⇒ bid b* (unconstrained, all equal); wealth ≤ b* ⇒ bid own wealth. Unique eq. Prop 2 total = (i*−1)b* + Σ_{i≥i*} w_i < v(N−1)/N. ALL bidders participate (p.116, contrast Hillman-Riley asymmetric valuations where some drop out).
- Prop 3: lottery > all-pay expenditure iff v > v* > w2.
- Entry (p.117): adding low-wealth entrants — no effect on all-pay/standard auction; always raises lottery expenditure.
- §4 credit: two cutoffs B*, B** — unconstrained / bid wealth / borrow (Lemma 5, Prop 5). Interest payments = dissipation not counted in expenditure.
- Hillman-Katz 1984 = risk-averse but IDENTICAL; C-G = risk-neutral HETEROGENEOUS wealth.
Relation to entry_contest:
- S-T's characterisation verified first-hand: budget constraint = kink at w_i in otherwise linear payoff; no curvature elsewhere. Our κ(w) has curvature everywhere, no kink, no cap. State as first-hand [F] now, not [C]-on-[F].
- Lemma 2 critical-bidder structure is the hard-constraint cousin of the κ threshold: above it behaviour is wealth-independent; below it wealth binds. But theirs is on the INTENSIVE margin (bid size), ours on the EXTENSIVE (enter or not).
- Prop 1 cap min{v,w2}: dissipation bounded by 2nd-highest wealth (wealth-driven); P7 cap V/κ is prize/utility-cost-driven and independent of the wealth distribution. Different bounds, worth one sentence.
- Their "all participate in lottery" vs our k* < Q: participation in C-G is never a choice; in ours it is the only choice.
- Their entry result (low-wealth entrants irrelevant in all-pay) rhymes with anonymity + P7: adding low-w agents to the pool doesn't move k* because they don't enter.
Bib CheGale1997 confirmed 92(1-2)? header says 92:109-126; keep number={1--2}. Accepted 7 March 1995.

## Suen2007 — J Econ Inequal 5:149-158 — PUBLISHED [F]
Setting: one-to-one TU assignment, Q=θ(x)φ(y), θ,φ increasing (concave for comparative statics), PAM, matching function μ=G^{-1}∘F ("can itself be regarded as a distribution function"), wage w(x)=ŵ+∫θ'φ(μ). Lowest type's payoff fixed by outside option (vs Costrell-Loury: Y-side zero rents). NOT a contest; no entry margin (unemployment cutoff only if measures unequal).
- Prop 1: FOSD ↑ in G raises all X payoffs and all differentials (no curvature needed).
- Prop 2: G1 more dispersed than G0 (SOSD, same mean) ⇒ total X earnings W1 < W0, if θ,φ concave and 1−F log-concave (IFR). PROOF handles GENERAL MPS with MULTIPLE crossings k0<k1<…<k2n of the quantile functions; sign alternates; SOSD ⇒ each partial integral ≤ 0; concavity + integration by parts against ρ=(1−F)/f.
- Prop 3: all X payoffs fall iff ∫0^x[G1^{-1}(F)−G0^{-1}(F)]dt ≤ 0 ∀x, i.e. μ1 SOSD μ0. Cor 1: f non-decreasing suffices. Cor 2: symmetric g's + symmetric unimodal f suffices.
- Fn 1: Costrell-Loury rescale to rank ŷ=G(y); Suen keeps levels because concavity is not preserved under rescaling unless g increasing.
- Cites: G1 SOSD-dominated ⇒ G1^{-1} SOSD-dominates G0^{-1} (from Costrell-Loury).
Relation to entry_contest:
- OPEN ITEM 2 LEAD: Suen signs an AGGREGATE under a GENERAL Rothschild-Stiglitz spread (arbitrary finite crossings), using (i) quantile-difference decomposition at crossing points, (ii) concavity of the payoff map, (iii) log-concavity of the survival function of the OWN-side distribution. P9-gen restricts to single-pivot T. The Suen technique — integrate the quantile difference against the hazard-rate weight — is a candidate route to extend P9 to general spreads, with a log-concavity hypothesis on Λ replacing the pivot hypothesis. Concrete, checkable lead; do NOT claim it works until tried.
- His T-object is μ=G^{-1}∘F (cross-side); ours is T=Λ_β^{-1}∘Λ_α (same distribution, before/after). Same map class.
- His sign is uniform (more dispersion ⇒ lower total) — no pivot — but it's a cross-side effect on a continuous rent, not own-side count. Not a competitor to P9's pivot rule.
- Filename says "contests"; the paper is matching. Tag accordingly.
Bib Suen2007 confirmed 5(2):149-158.

## DrugovRyvkin2020 (Tournament Rewards and Heavy Tails) — WP 13 Mar 2019 (→ JET 190, 105116) [F, WP version]
Setting: n≥2 symmetric risk-neutral, additive noise, principal allocates budget 1 across monotone rank prizes v; maximise aggregate effort. No entry margin.
- β_{r,n} = marginal prob of rank r; B_{r,n} = Σ_{k≤r} β = E[m(Z_{n−r:n−1})], m = f∘F^{-1} (inverse quantile density).
- Prop 1: optimum is two-prize schedule, top r* equal; r* = argmax B_{r,n}/r = (1/n)E[h(Z_{n−r:n})], h = hazard quantile fn.
- Prop 2: IFR ⇒ WTA; DFR ⇒ reward all but last; exponential ⇒ any.
- Prop 3: symmetric f ⇒ r* < n/2. Prop 4: convex transform order (more skewed/less IFR) ⇒ r* weakly higher. Prop 5/6: interior-unimodal hazard ⇒ r*>1 for large n, r* ↑ in n.
- Prop 7: log-concave f ⇒ effort Schur-convex in v (majorization = closeness to WTA).
- Prop 8: status tournaments (MSS07): IFR ⇒ top category singleton (MSS Thm 3 recovered); log-concave ⇒ finest partition; DFR ⇒ top category n−1.
- Prop 9: existence conditions (c''≥c0 > D bounds on f,f').
Relation: same hazard-rate machinery as RyvkinDrugov2020 TE; not directly about P-MU (this is prize design, single prize V here). Prop 8 is the status-tournament link — with heavy tails the top status tier should be wide. Marginal for positioning; cite as companion.
NEW BIB: DrugovRyvkin2020a — "How noise affects effort in tournaments", JET 188 (2020) 105065 [verified from RyvkinDrugov2020 TE ref list]. This is the one with dispersive order N&S for effort ranking.
Bib DrugovRyvkin2020 (JET 190 105116) entry stands; WP title matches.

## MorganOrzenSefton2012 — Econ Theory 51:435-463 — PUBLISHED (open access) [F]
Theory (§3): N IDENTICAL risk-neutral, equal endowment w, outside option F, Tullock prize P; continuous-time entry, decisions observable, irreversible. Symmetric eq investment x*_n=(n−1)P/n², payoff w+P/n². Pure-strategy count n* = ⌊√(P/F)⌋ (largest n with P/n² > F). p.441 EXPLICIT: "since all of the players are identical in the model, the identity of the players choosing to opt into the contest is not uniquely determined." Fix: private random delays d_i; Prop 1: unique PBE has the first n* in delay order enter immediately. §3.2: relative-earnings preferences raise investment but earnings equalisation survives.
Experiment: groups of 6, w=100, F=10, P∈{50,200}, n*∈{2,4}, 50 rounds. Results: small prize over-entry (2.5 vs 2) and over-investment, contestants earn LESS than outside option; large prize under-entry (3.7 vs 4), contestants earn MORE. Earnings NOT equalised. Sorting Gini 0.63/0.72 — partial persistence of who enters. Self-selection-by-risk-attitude hypothesis REJECTED (3-player contests differ across treatments at 1%). QRE fits entry only with implausible λ.
Relation to entry_contest:
- THIRD explicit statement (after Levin-Smith 1994, Fu-Lu 2010) that identical agents leave the identity of entrants indeterminate; three fixes in the literature: mixing (L-S), sequential/arrival order (F-L, MOS), κ(w) heterogeneity (P5). Good sentence.
- Their design HOLDS ENDOWMENT EQUAL (w=100 for all) — wealth heterogeneity designed out. So the experimental literature has not tested the resource-sorting channel. Worth one sentence: the natural experiment for P8/P9 is a MOS design with heterogeneous endowments.
- Their conclusion "power of the marginal individual is limited" because inframarginal investment moves the marginal calculus — in our binary model inframarginal behaviour is fixed (invest or not), which is why the cutoff is clean.
Bib confirmed 51(2):435-463.

## CostrellLoury2004 — draft 11 Dec 2003 (→ JPE 112(6) 2004) [F, draft version]
Setting: fixed-proportions hierarchical assignment; continuum of tasks t∈[0,1] ordered by ability-sensitivity β(t) non-decreasing; one-to-one so rank p=t; w'(p)=β(p)μ'(p) (no-arbitrage), level by zero profit; Prop 1 wage profile w(p)=β(p)μ(p)+∫∫μ dβ with MP interpretation via reassignment. §5 variable proportions (Cobb-Douglas/CES crowding).
- TWO-JOB MODEL (§2): jobs filled in proportions θ,1−θ; SUFFICIENT STATISTIC for the whole wage schedule is μ̂ = F^{-1}(θ), the ability of the MARGINAL worker. Any shift raising μ̂ raises low-ability wages and lowers high-ability wages. MPS on fixed support: "the effect of a more unequal ability distribution depends on whether the worker on the margin between jobs, at quantile θ, lies in the upper or lower tail" — θ high (marginal worker in right tail) ⇒ MPS raises μ̂ ⇒ narrows wages; θ low ⇒ reduces μ̂ ⇒ widens. ← SINGLE-PIVOT SIGN RULE located at the MARGINAL AGENT relative to the MPS crossing. STRUCTURALLY THE CLOSEST PUBLISHED PRECEDENT FOR P9-gen's pivot rule (w_(k*) vs x0).
- Prop 4: FOSD improvement raises w(0), lowers w(1).
- Prop 5: MPS (general, multiple crossings, same mean, fixed support) raises output Q (β non-decreasing).
- Lemma 1 / proof of Prop 5: G MUT F (SOSD-riskier) ⇒ F^{-1} MUT G^{-1}: quantile functions reverse the order (fixed support [0,1], atomless). Tool for open item 2.
- Prop 6: G MUT F: β concave ⇒ w(0)↓, span ↑; β convex ⇒ w(1)↓, span ↓. Linear β ⇒ span unchanged, middle gains.
- Theorem 1: Blackwell-more-informative test ⇒ SOSD-riskier distribution of posterior-mean ability. (Information = MPS.)
- Prop 10 (Cobb-Douglas crowding): MPS ALWAYS narrows span regardless of β shape. Prop 11 (CES): depends on substitutability r.
- Cor 1-2, Prop 7: integration of two groups; both gain on average.
Relation to entry_contest:
- P9 PRECEDENT: their two-job pivot is the same logic as P9-gen — a single marginal agent at a fixed quantile, and the MPS effect's sign flips according to whether that quantile lies above or below the crossing point. Their object is the wage schedule via μ̂; ours is k* via w_(k*). The survey should say P9-gen is the entry-count analogue of the Costrell-Loury two-job comparative static, and that the continuum version (Prop 6) shows the flip is governed by curvature of the payoff map (β) — which for us is κ.
- Open item 2: their Prop 5 handles GENERAL MPS with multiple crossings via Γ(ρ)=∫[G^{-1}−F^{-1}] ≥ 0 and integration by parts against dβ; with Suen's version, this is the standard toolkit. Two independent sources now point the same way.
- Theorem 1 (Blackwell ⇒ MPS): gives P9 an INFORMATION reading — a finer signal of who will win is an MPS of the relevant distribution. Not for the wealth distribution, but if r_i (talent) were imperfectly observed, the same theorem applies to F. Flag only.
- Fn 1/§5: they note assortative matching (Kremer O-ring) raises inequality — outside their scope; our model has no complementarity across entrants.
Bib CostrellLoury2004 stands (JPE 112(6):1322-1363). Draft ≠ published; published-version check outstanding.

## MorenoWooders2011 — RAND 42(2):313-336 — PUBLISHED [F]
Setting: N risk-neutral buyers, IPV auction with screening value v and admission fee φ; entry cost Z_i iid ~ H on [c̲,c̄], PRIVATELY known before entry; value learned after entry. Symmetric equilibrium is a common THRESHOLD t: enter iff z < t. Number of bidders ~ Binomial(N, H(t)) — STOCHASTIC.
- Prop 1: u(0,n) = s(0,n) − s(0,n−1): bidder utility = marginal social contribution (screening value 0).
- Prop 2: unique symmetric threshold t*(v,φ) solving U(v,H(t)) = t + φ; continuous; decreasing in v and φ.
- Prop 3: v=φ=0 maximises social surplus (dW/dt = N h(t)[U(0,H(t)) − t]: private gain of the marginal type = its social contribution).
- Prop 4: inframarginal buyers (z < t*) keep information rents; seller does not capture all surplus (unlike homogeneous case).
- Prop 5: revenue-max screening value v* ∈ (0, v_F). Prop 6: with admission fee feasible, v*=0, φ*>0 — screen by entry cost not by value.
- Prop 7/8: entry cap n̄ = n*(c̲) + fee raises revenue (homogeneous: captures unconstrained max surplus).
- Prop 9: as N→∞ heterogeneity irrelevant; only lower bound c̲ matters; buyer surplus → 0.
- §6/fn 13: revenue may rise OR fall in N (unlike Levin-Smith): coordination effect vs better cost selection.
- NO comparative statics in the distribution H. No MPS. No wealth. No concavity.
Relation to entry_contest — THE RELABELLING CHALLENGE, answered:
(a) INFORMATION. M-W's cost is PRIVATE ⇒ symmetric Bayesian threshold, count binomial. PROOFS.tex uses the order statistics w_(j) in the entry condition κ(w_(j)) ≤ Δ(j−1), which presupposes the wealth profile is COMMON KNOWLEDGE ⇒ deterministic k*. PROOFS.tex does NOT state this assumption anywhere (grep: no "complete information"/"common knowledge"). MUST BE STATED. This is the first real structural difference: complete-info heterogeneous costs pin the exact count and the exact identities; private heterogeneous costs pin only a distribution.
(b) PRIMITIVE. M-W's H is primitive and they do no comparative statics on it. Ours is κ∘Λ^{-1}, induced from a resource distribution by a known concave map; P9's MPS on Λ has no M-W counterpart because there is no distributional comparative static in M-W at all.
(c) DESIGNER. M-W is mechanism design (v, φ, cap); ours has none.
(d) OBJECTIVE. Their Prop 3 (marginal private gain = marginal social gain) is the IPV logic; for a fixed-prize WTA contest the CV/business-stealing logic (Levin-Smith Prop 3) applies instead. Their Prop 4 (inframarginal rents) carries over: entrants with w > w̄ keep surplus.
Verdict: the threshold-in-cost-space is theirs; the count-pinning under complete info, the induced-cost structure, and the distributional comparative static are not. The relabelling objection is answered, but only once PROOFS.tex states the information assumption.
Bib MorenoWooders2011 confirmed 42(2):313-336, Summer 2011.

## Treich2010 — LERNA WP 09-013, 17 Feb 2009 (→ Public Choice 145(3) 2010) [F, WP version]
Setting: n≥2 IDENTICAL EU maximisers, initial wealth w, concave u, rent b, payoff p_i u(w+b−x_i) + (1−p_i) u(w−x_i) (Konrad-Schlesinger 1997 model; S-T "rent-seeking contest" form). General CSF with A1-A4. Continuous effort. No entry.
- Prop 1: DARA + small rent ⇒ unique symmetric eq (general CSF). CARA ⇒ unique for any b (appendix).
- Prop 2: prudence (u''' ≥ 0) ⇒ risk aversion lowers effort vs risk neutrality, any n, any regular CSF. Proof via Eeckhoudt-Gollier 2005 Lemma: 0.5[u'(B)+u'(A)](B−A) ≥ u(B)−u(A) iff u' convex.
- Cor 1: n=2, prudence is NECESSARY AND SUFFICIENT. Ex 1: quadratic u, n=2 ⇒ no effect.
- Prop 3: risky rent + risk aversion + prudence ⇒ lower effort (Jensen twice).
- Insight: in symmetric contests p ≤ 1/2, so raising p RAISES payoff variance — opposite of self-protection where p>1/2. That's why 1/2 is pivotal (cf. Cornes-Hartley Prop 6.1 "no one wins w.p. > 1/2").
- Conclusion: "how risk affects the incentive to engage in rent-seeking activities in a model where entry is possible" listed as future research — ENTRY NOT DONE.
Relation to entry_contest:
- Confirms the risk-aversion strand is intensive-margin, identical agents, wealth as additive shift, prudence (u''') doing the work. Our κ(w) uses only u'' (concavity) — consistent with the "no higher-order restrictions" contribution claim. Worth stating: the risk-aversion strand needs u''' to sign anything; P8/P9 need only u''.
- His closing sentence explicitly flags entry as undone. Cite.
- Wealth w here: additive, static, one-period. Same object again.
Bib Treich2010 stands (Public Choice 145(3):339-349). WP ≠ published; check.

## ColeMailathPostlewaite1995 — CARESS WP 95-14, 1 Aug 1995 (→ FRB Minneapolis Quarterly Review 19(3):12-21) [F, WP version]
Title on WP: "Incorporating Concern for Relative Wealth into Economic Models" (singular "Concern"). HK2010 ref list has "Concerns". Published title still unverified.
Setting: instrumental concern for relative standing — no status in u; rank matters because a non-market decision (matching) depends on it. §2 complete-info effort model: continuum of women with productivity a(j) choose effort; PAM on wealth; matching function m(y) = CDF of female output; FOC a(j)[u'(y) + m'(y)] = v'(l). §3 incomplete info: wealth unobservable ⇒ signalling by wealth DESTRUCTION d (Veblen); d(j)/y(j) increasing in j; separating eq via Mailath 1987.
Key results/insights:
- m' large when the output distribution is TIGHT ⇒ intense competition; dispersed ⇒ slack (§2.1 closed form; §2.3). "It is competition from below that distorts individuals' effort decisions" — truncating from the top changes nothing; truncating from below does (fn 8, p.7).
- Individual vs aggregate shocks differ because the matching function (a "price" in the non-market sector) moves with aggregate shocks (§2.3, §4).
- §4: prizes allocated by rank rather than sold ⇒ standard welfare theorems do not apply; examples: club memberships, board invitations, trusteeships.
- Fn 3: CMP92's main point is that PAM is NOT the only norm — multiple equilibria via social norms.
Relation to entry_contest:
- FINDING 5: this is the paper that JUSTIFIES a fixed prize allocated by rank without status in u — exactly the premise PROOFS.tex asserts. Cite as the microfoundation; state that V is a "prize not sold" in their sense.
- "Competition from below" is the CMP version of our anonymity/ordering: only those who could out-rank you matter. In our model the marginal entrant w_(k*) is pinned from below by κ.
- Their §2.3 tight-vs-dispersed intuition (dispersion lowers m' ⇒ lowers effort) IS the "standard result" in intensive-margin form, predating HK2004. P9's exclusive-branch reversal is against this.
- No entry margin; continuous effort; women identical in u, differ in productivity a(j) — heterogeneity is ABILITY not resources. Men's endowments (the prizes) are heterogeneous.
- FRB QR published version: title & pages to verify before bib entry. Provisional key ColeMailathPostlewaite1995.

## ColeMailathPostlewaite1992 — JPE 100(6):1092-1125 — PUBLISHED [F]
Setting: multigenerational; continuum of men (capital k, bequest) and women (endowment j ~ U[0,1], nontraded). Joint consumption; CRRA u, γ; woman's endowment enters linearly. Status = "a ranking device that determines how well he fares in the nonmarket sector". Matching voluntary, complete info. Two social norms: WEALTH-IS-STATUS (richest man ↔ richest woman) and ARISTOCRATIC (status inherited; deviation punished by status loss).
Key results:
- §IV.A two-period example: matching raises savings above the no-matching level for all but the bottom man (undistorted); with an initially degenerate income distribution the only equilibria produce inequality; a MORE COMPACT wealth distribution ⇒ HIGHER savings rate ("the agent from the economy with the more compact income distribution will tend to have a higher savings rate", p.1103). ← The intensive-margin "standard result" again, 1992.
- Prop 1: wealth-is-status eq exists (Mas-Colell 1984 continuum-game existence; atoms smoothed).
- Property 1: NO RANK SWITCHING in equilibrium — relative positions never change despite everyone competing. Property 2: everyone (weakly) oversaves vs no-matching; bottom undistorted. Property 3/4: no atoms; capital distribution strictly increasing. Property 5 (γ<1): oversaving vanishes asymptotically. Property 6 (γ>1): either the poorer saves everything or the wealth ratio → ∞.
- Prop 2: aristocratic eq exists if γ>1, β high, and the capital distribution is sufficiently SPREAD OUT at the lower tail (so no low-status man can bequeath enough to poach a match). Not fully subgame perfect; discussed.
- §V.D: matching AGGRAVATES inequality; "when the capital distribution becomes sufficiently dispersed, the increased incentive to save disappears" (γ<1).
- §V.A fn 20: a housing market where relative position determines the house in competitive equilibrium CANNOT generate their multiplicity — prizes must be non-market.
- §V.E: reduced form u(c)+r_t (rank) is derivable from the full model.
Relation to entry_contest:
- FINDING 5 microfoundation (with CMP95): the fixed prize V allocated by rank is legitimate only because it is a "nonmarket decision"; the paper's own fn 20 says a market-priced analogue kills the results. PROOFS.tex should cite this when it posits V.
- Property 1 (no rank switching) vs our model: in ours the entry decision does not change wealth rank either — entrants are the top-k* by w, and the winner is by score. State that our sorting is consistent with their no-switching property; it's the SCORE, not the rank, that the prize follows.
- Multiplicity here is ACROSS social norms (wealth-is-status vs aristocratic), not within a norm — different from Cornes-Hartley's within-game multiplicity. P5's uniqueness is within the wealth-is-status norm; the aristocratic analogue in our model would be an incumbent with inherited access. Worth a sentence; not a threat.
- p.1103 compact ⇒ more saving; §V.D dispersed ⇒ incentive disappears: this is the 1992 origin of the "inequality reduces status expenditure" claim R2 invoked. P9's exclusive branch reverses it on the extensive margin. Cite as the origin.
- No entry margin; continuous bequest; heterogeneity in initial capital k_0(i) (resources!) — this IS resource heterogeneity, but on the intensive margin with no fixed cost.
Bib ColeMailathPostlewaite1992 confirmed 100(6):1092-1125, Dec 1992 Centennial Issue.

## BeckerMurphyWerning2005 — "Revised December 2003" draft (→ JPE 113(2):282-310) [F, draft version]
Setting: continuum of agents, u(c,s) with u_cc<0, u_s>0 and CRUCIALLY u_cs>0 (status raises MU of consumption). Fixed supply of status positions (normalised uniform). Status either SOLD in a hedonic market (P(s)), or via a fixed-supply intrinsically-worthless "social good" z ranked by consumption (Prop 1: equivalent), or by INCOME RANK (§7). Stage 1: fair lotteries over income; stage 2: buy status.
- Prop 2: if the constant-MU allocation Φ* is a mean-preserving spread of the initial full-income distribution F, then Φ* is the competitive lottery equilibrium — i.e. a MINIMUM equilibrium inequality; any more compact initial distribution is spread out by lotteries to the SAME final distribution ("manufactured inequality", Rosen 1997).
- Prop 3: with a status market, the lottery equilibrium coincides with the utilitarian planner's allocation; the planner INCREASES inequality.
- §3.3: if the social good were producible, its production would be wasteful (Frank's rat race); fixed supply + zero intrinsic value avoid this.
- §7 income-rank version: Prop 4 same MPS result, but rank lotteries impose REAL externalities (winning lowers others' rank), so equilibrium ≠ planner.
- §6: redistribution below the equilibrium inequality is undone by lotteries.
Relation to entry_contest:
- NOT a contest paper; no entry margin, no effort, no fixed fee. Its mechanism (u_cs>0 ⇒ convex kink in indirect utility ⇒ demand for fair gambles) is orthogonal to κ(w). Relevance is (i) the MPS appears as an equilibrium OUTPUT here, not an input — the opposite direction from P9; (ii) §7's point that rank-based status carries real externalities while purchased status does not — the welfare distinction R2 Comment 3 needs: our prize is rank-allocated, so the externality logic of §7 (zero-sum, business-stealing) is the right welfare frame, and BMW state it explicitly; (iii) Prop 3/§6: a planner may WANT inequality — which is a warning against assuming "inequality reduces expenditure" is normatively bad.
- Their "status raises MU of consumption" (u_cs>0) is the structural opposite of our separable V. Note it as the assumption we do not make.
- Cite for the welfare section only. Low priority otherwise.
Bib BeckerMurphyWerning2005 stands (JPE 113(2):282-310). Draft ≠ published.

## MoldovanuSela2001 — draft 5 June 2000 (→ AER 91(3):542-558) [F, draft version]
Setting: k≥p contestants, p prizes V1≥…≥Vp, Σ=1; ex-ante symmetric, risk-neutral; PRIVATE ability c_i ~ F on [m,1] (cost c·γ(x), separable); all-pay: highest bid wins V1 etc.; all pay. Designer maximises expected sum of bids. Perfectly discriminating (no noise).
- Prop 3.1: symmetric eq bid b(c)=A(c)V1+B(c)V2 with A,B explicit (eqs 3.1-3.2); Prop 4.1: general γ ⇒ b = γ^{-1}(A V1 + B V2).
- Prop 3.2/4.2: linear or concave cost ⇒ single first prize optimal (Lemma 8.1: ∫(B−A)dF < 0 for ANY F).
- Prop 4.3: convex cost ⇒ two prizes optimal iff ∫(B−A)g'(A)dF > 0; intuition: second prize helps middle/low types.
- Lemma 8.1-3: B'(c*)=0 at F(c*)=1/(k−1) — the marginal effect of the second prize on bids flips sign at a fixed QUANTILE 1/(k−1). Another fixed-quantile sign flip.
- §5 entry fees: types [m, c_E] participate; boundary conditions (5.3)-(5.4); with linear cost a single prize + optimal fee is revenue-maximising among all IC/IR mechanisms; Ex 5.1: with convex cost two prizes still beat one even with fees.
- Fn 12: "The independence assumption is problematic in some models with endogenous entry." Fn 33: restricting entry is never optimal in a WTA all-pay auction with ex-ante symmetric agents (Bulow-Klemperer).
Relation to entry_contest:
- Background. Heterogeneity is ABILITY (private), not resources; perfectly discriminating; continuous bid. Entry fee §5 produces a threshold in ABILITY space c_E — a third threshold variant (cost/ability/resource). Note alongside Moreno-Wooders.
- Fn 12's caveat on independence + endogenous entry is worth quoting in spirit: in our model entrants are the top-k* by w, so the entrant pool is not an iid sample of the population — same issue.
- Lemma 8.1-3 fixed-quantile flip at 1/(k−1): another example of a comparative static whose sign is pinned at a specific rank; rhymes with the pivot logic but is about prize structure, not spreads.
Bib MoldovanuSela2001 stands. Draft ≠ published.

## HoppeMoldovanuSela2009 — REStud 76:253-281 — PUBLISHED [F]
Setting: FINITE n men, k women (n≥k), two-sided PRIVATE attributes x~F, y~G, output xy; costly wasteful signals; assortative matching on signals. Perfectly discriminating. Barlow-Proschan (1966) normalized-spacings machinery. Continuum limit §7.
- Prop 1: separating eq β(x) explicit in mean order statistics of the other side. Prop 2: total signalling S_m = Σ (n−i+1)(EY_(i)−EY_(i−1)) EX_(i−1) — a weighted sum of NORMALIZED SPACINGS; welfare ≥ half output (no full dissipation, ever).
- §3.2 externality interpretation: signals = Vickrey payments for the negative externality on lower-ranked same-side agents; higher-ranked agents unaffected by one's presence.
- §4 (Barlow-Proschan Thm 1, Thm 2): H^{-1}F convex (star/convex transform order) ⇒ mean order statistics majorized; IFR ⇒ normalized spacings decreasing in rank, DFR ⇒ increasing. §4.2: more heterogeneity on one side ⇒ output ↑; own-side signalling ↓ if other side IFR; other-side signalling ↑. Points 1-3 hold under plain SOSD.
- Prop 4 (ENTRY on the long side): own-side total signalling ↑ for all F; other-side signalling ↑ (↓) if F is DFR (IFR); own welfare ↑(↓) if DFR (IFR); Example 1: W(n,3) decreasing in n up to n=8 then increasing — entry can lower welfare in a small finite market but not in the continuum (Ex 3).
- Prop 5: the relatively more HOMOGENEOUS side signals more (perceives more dispersed prizes).
- Prop 6/9: random vs assortative matching: IFR ⇒ random better; DFR ⇒ assortative better; continuum: iff CV ≥ 1. Prop 7: all types may prefer random matching (a signalling trap). Lemma 1: single cutoff type x̂ separating who prefers which.
- Fn 41: cites Costrell-Loury, Suen, HK for cross-side distributional comparative statics.
Relation to entry_contest:
- Heterogeneity is ABILITY (private). Signalling is intensive-margin. No fixed entry cost, no resource heterogeneity. Perfectly discriminating.
- Prop 4 (entry on the long side raises own-side signalling for ALL F) is the perfectly-discriminating, private-ability counterpart of the Q-comparative-static; contrast: our P-MU makes the sign of the Q-effect on the marginal entrant distribution-dependent even with symmetric agents, because noise. Cite as the no-noise benchmark.
- Prop 5 (homogeneous side signals more) is the same-side/other-side version of "dispersion lowers competition". Another member of the standard-result family; intensive margin.
- Their IFR/DFR machinery on spacings is the same as Ryvkin-Drugov's; both families are Barlow-Proschan descendants. Note lineage in the toolkit paragraph.
- Fn 41 places Costrell-Loury and Suen as the comparators on cross-side comparative statics — consistent with our §assignment.
Bib HoppeMoldovanuSela2009 confirmed 76(1):253-281.

## LazearRosen1981 — JPE 89(5):841-864 — PUBLISHED [F]
Setting: two-player tournament, q_j = μ_j + ε_j, investment μ at cost C(μ), prizes W1>W2 fixed; winner by highest q; symmetric Nash; free entry among firms sets prizes. §II risk-neutral: C'(μ)=(W1−W2)g(0) (eq 6); tournament efficient (V=C'(μ)); eq (10): prizes = expected product ± V/2g(0), the "entrance fee or bond" interpretation — players post a bond and take a fair WTA gamble over the pool. Fn 2: pure-strategy eq exists only if σ² is large enough ("contests are feasible only when chance is a significant factor").
§III risk aversion: tournaments truncate tails; can dominate piece rates under DARA; Table 1; §"Income Distributions": with common u but different ENDOWED income y0, the rich prefer contests and the poor prefer piece rates ⇒ self-selection into payment scheme BY WEALTH; resulting income distribution positively skewed (Friedman 1953 link). Contests eliminate common-error risk.
§IV heterogeneous ability (two types, costs C_a<C_b): ADVERSE SELECTION — everyone wants the a-league; mixed play inefficient unless α=1/2; separation costs overinvestment; nonprice rationing/credentials needed. Handicaps: competitive handicap h*=Δμ/2 (not "fair"); reverse discrimination consistent with efficiency.
Relation to entry_contest:
- The "bond" reading of eq (10) is the cleanest published statement that a fixed entry fee and a WTA prize are two faces of the same contract. Our c is exactly that bond, paid out of income, hence κ(w).
- §III "Income Distributions": SELF-SELECTION BY ENDOWED WEALTH into the contest vs the safe scheme — this is the first appearance of "who enters a contest is determined by wealth" in the tournament literature (1981!), via DARA rather than via a fixed fee. MUST CITE against finding 5: the extensive-margin-by-wealth idea is in Lazear-Rosen, but as a risk-attitude channel (DARA, wealthier ⇒ less absolute risk aversion ⇒ prefers the gamble), not a utility-cost-of-fee channel. Our κ(w) needs only concavity; theirs needs DARA. State both.
- §IV adverse selection: heterogeneous types all prefer the top league — a "contamination" result. Ours: challengers are heterogeneous in w, and entry is by κ; no league choice. But the R2 "open question" (incumbent abstains, challenger invests) is a handicap-like configuration; their §IV handicap algebra is the tool.
- Fn 2 existence caveat (large σ²) is the ancestor of Ryvkin-Drugov Prop 7.
Bib LazearRosen1981 confirmed 89(5):841-864.
