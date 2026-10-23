# theory/verify_theory.py, run 23 October 2026 11:05 UTC

```
========================================================================
THEORY PAPER (theory/lean/)
========================================================================
      files ['BCVW.lean', 'Bounds.lean', 'CapGeometry.lean', 'Coordinate.lean', 'Envelope.lean', 'Identification.lean', 'ImpulseCount.lean', 'QVI_Part1.lean', 'RateBased.lean', 'SecondOrder.lean', 'SmoothFit.lean', 'Targeting.lean']
      theorems declared 98; Lean names cited by MANUSCRIPT.tex 89
[PASS] TH-1 every theory file is identical to its source in lean/mathlib   []
[PASS] TH-2 every Lean name cited by MANUSCRIPT.tex is declared in theory/lean
------------------------------------------------------------------------
CONTROLS
[PASS] TH-C1 a fabricated citation is detected as missing
[PASS] TH-C2 a row's last brace group is read as its Lean names
------------------------------------------------------------------------
========================================================================
LEAN WITH MATHLIB (theory/lean/)
========================================================================
      toolchain leanprover/lean4:v4.32.0-rc1; modules ['BCVW', 'Bounds', 'CapGeometry', 'Coordinate', 'Envelope', 'Identification', 'ImpulseCount', 'QVI_Part1', 'RateBased', 'SecondOrder', 'SmoothFit', 'Targeting']; build roots ['SmoothFit', 'QVI_Part1', 'RateBased', 'Envelope', 'ImpulseCount', 'CapGeometry', 'BCVW', 'Identification', 'SecondOrder', 'Coordinate', 'Bounds', 'Targeting']
[PASS] ML-1 every .lean file in theory/lean is a build root
[PASS] ML-2 lake build succeeds    (6.8s)
✔ [8585/8592] Built RateBased (9.3s)
✔ [8586/8592] Built QVI_Part1 (8.6s)
✔ [8587/8592] Built SmoothFit (8.8s)
✔ [8588/8592] Built Bounds (8.6s)
✔ [8589/8592] Built Envelope (6.7s)
✔ [8590/8592] Built BCVW (7.2s)
✔ [8591/8592] Built Coordinate (7.6s)
Build completed successfully (8592 jobs).
[PASS] ML-3 no sorry in any Mathlib source
      theorems: 98; audited: 98; axiom-free: 0; within propext/Classical.choice/Quot.sound: 98
        BCVW.piBar_mul_y: ['Classical.choice', 'Quot.sound', 'propext']
        BCVW.piBar_at_one: ['Classical.choice', 'Quot.sound', 'propext']
        BCVW.diffusion_vanishes_at_one: ['Classical.choice', 'Quot.sound', 'propext']
        BCVW.drift_at_one: ['Classical.choice', 'Quot.sound', 'propext']
        BCVW.sig2_decomp: ['Classical.choice', 'Quot.sound', 'propext']
        BCVW.sig2_nondegenerate: ['Classical.choice', 'Quot.sound', 'propext']
        BCVW.switching_point_baseline: ['Classical.choice', 'Quot.sound', 'propext']
        BCVW.piBar_tendsto: ['Classical.choice', 'Quot.sound', 'propext']
        BCVW.u_concave: ['Classical.choice', 'Quot.sound', 'propext']
        BCVW.control_nondegenerate_needs_abs_c_lt_one: ['Classical.choice', 'Quot.sound', 'propext']
        BCVW.control_limit_needs_a1_le_a3: ['Classical.choice', 'Quot.sound', 'propext']
        Bounds.qv_zero: ['Classical.choice', 'Quot.sound', 'propext']
        Bounds.nu2_eq_qv: ['Classical.choice', 'Quot.sound', 'propext']
        Bounds.qv_le_max: ['Classical.choice', 'Quot.sound', 'propext']
        Bounds.zvol2_eq_qv: ['Classical.choice', 'Quot.sound', 'propext']
        Bounds.deficit_bound: ['Classical.choice', 'Quot.sound', 'propext']
        Bounds.surplus_bound: ['Classical.choice', 'Quot.sound', 'propext']
        Bounds.control_bound_needs_long_exposure: ['Classical.choice', 'Quot.sound', 'propext']
        Bounds.control_bound_needs_compliance: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.hasDerivAt_zmap: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.zmap_inverse: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.zmap_of_inverse: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.xbar_sub_one: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.one_lt_xbar: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.solvency_binds: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.liquidity_binds: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.zvol2_eq: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.zvol2_solvency: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.deficit_limit: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.surplus_limit: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.variance_ratio_is_cap_geometry: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.control_surplus_limit_needs_a1_lt_a3: ['Classical.choice', 'Quot.sound', 'propext']
        CapGeometry.control_constancy_needs_binding: ['Classical.choice', 'Quot.sound', 'propext']
        Coordinate.zmap_strictMonoOn: ['Classical.choice', 'Quot.sound', 'propext']
        Coordinate.zmap_at_requirement: ['Classical.choice', 'Quot.sound', 'propext']
        Coordinate.zmap_tendsto_atBot: ['Classical.choice', 'Quot.sound', 'propext']
        Coordinate.zmap_tendsto_atTop: ['Classical.choice', 'Quot.sound', 'propext']
        Coordinate.cappedDrift_at_one: ['Classical.choice', 'Quot.sound', 'propext']
        Coordinate.zdrift_tendsto_atTop: ['Classical.choice', 'Quot.sound', 'propext']
        Coordinate.control_zdrift_needs_positive_limit: ['Classical.choice', 'Quot.sound', 'propext']
        Envelope.smf_permits: ['Classical.choice', 'Quot.sound', 'propext']
        Envelope.wode_of_envelope: ['Classical.choice', 'Quot.sound', 'propext']
        Envelope.node_of_envelope: ['Classical.choice', 'Quot.sound', 'propext']
        Envelope.neumann_at_ystar: ['Classical.choice', 'Quot.sound', 'propext']
        Envelope.trigger_jump: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.vppp_at_barrier: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.fpp_at_barrier: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.barrier_slope: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.barrier_slope_pos: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.boundary_slope: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.trigger_rises_in_lambda: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.trigger_falls_in_K: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.target_rises_in_K: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.gap_increasing_in_K: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.det_formula: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.det_neg_of_opposite: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.det_neg: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.control_same_direction_can_fail: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.control_barrier_sign_needs_gap: ['Classical.choice', 'Quot.sound', 'propext']
        Identification.control_trigger_slope_needs_curvature: ['Classical.choice', 'Quot.sound', 'propext']
        ImpulseCount.partial_sums_bounded: ['Classical.choice', 'Quot.sound', 'propext']
        ImpulseCount.count_summable: ['Classical.choice', 'Quot.sound', 'propext']
        ImpulseCount.count_le: ['Classical.choice', 'Quot.sound', 'propext']
        ImpulseCount.impulse_sum_summable: ['Classical.choice', 'Quot.sound', 'propext']
        ImpulseCount.control_count_needs_fixed_cost: ['Classical.choice', 'Quot.sound', 'propext']
        QVI_Part1.lambda_asymmetry_vanishes_at_one: ['Classical.choice', 'Quot.sound', 'propext']
        QVI_Part1.neg_Lambda_concave: ['Classical.choice', 'Quot.sound', 'propext']
        QVI_Part1.control_concavity_needs_lambda_ge_one: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.phi_minus_tendsto_one: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.phi_minus_strictAnti: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.persistence_asymmetry_eq: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.persistence_asymmetry_tendsto: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.Vresp_ratio_eq: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.tanh_eq_one_sub: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.tendsto_tanh_atTop: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.saturated_ratio_tendsto: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.saturated_ratio_lambda4: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.sojourn_strictAnti: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.sojourn_tendsto_atTop: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.balanced_growth_iff: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.control_identity_needs_retention: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.control_persistence_needs_partial_retention: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.control_sojourn_needs_positive_depth: ['Classical.choice', 'Quot.sound', 'propext']
        RateBased.refuted_asymmetry_vanishes: ['Classical.choice', 'Quot.sound', 'propext']
        SecondOrder.psi_identity: ['Classical.choice', 'Quot.sound', 'propext']
        SecondOrder.psi_nonneg_of_qvi: ['Classical.choice', 'Quot.sound', 'propext']
        SecondOrder.trigger_convex: ['Classical.choice', 'Quot.sound', 'propext']
        SecondOrder.trigger_strict_iff: ['Classical.choice', 'Quot.sound', 'propext']
        SecondOrder.control_trigger_flat_when_psi_zero: ['Classical.choice', 'Quot.sound', 'propext']
        SecondOrder.refuted_target_strict: ['Classical.choice', 'Quot.sound', 'propext']
        SmoothFit.hasDeriv_Mv: ['Classical.choice', 'Quot.sound', 'propext']
        SmoothFit.kink_is_convex: ['Classical.choice', 'Quot.sound', 'propext']
        SmoothFit.no_convex_kink: ['Classical.choice', 'Quot.sound', 'propext']
        SmoothFit.smooth_fit: ['Classical.choice', 'Quot.sound', 'propext']
        SmoothFit.control_kink_needs_obstacle: ['Classical.choice', 'Quot.sound', 'propext']
        Targeting.target_free_of_start: ['Classical.choice', 'Quot.sound', 'propext']
        Targeting.target_same_for_all_starts: ['Classical.choice', 'Quot.sound', 'propext']
        Targeting.control_target_moves_with_convex_cost: ['Classical.choice', 'Quot.sound', 'propext']
[PASS] ML-4 axiom audit covers every declared theorem
[PASS] ML-5 no axiom outside propext, Classical.choice and Quot.sound
------------------------------------------------------------------------
CONTROLS  the audit parser must reject what it is meant to reject
[PASS] ML-C1 sorryAx is flagged as disallowed
[PASS] ML-C2 the three standard axioms are allowed
[PASS] ML-C3 an unaudited name is detected as missing
========================================================================
MATHLIB SUMMARY: 8 checks, 0 failures
========================================================================
[PASS] TH-3 build, sorry and axiom audit of theory/lean (checks/verify_mathlib.py)
========================================================================
THEORY SUMMARY: 5 checks, 0 failures
========================================================================
```
