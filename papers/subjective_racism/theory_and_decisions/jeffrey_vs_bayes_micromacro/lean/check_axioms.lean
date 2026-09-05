/-
Axiom audit: every headline theorem of the formalisation should depend only on
Lean's three standard axioms (`propext`, `Classical.choice`, `Quot.sound`) --
in particular on no `sorry`.

    lake env lean check_axioms.lean
-/
import JeffreyOrder
import AssocLocality
import AssocLocalityStructural
open JeffreyOrder

-- Proposition IMM
#print axioms propIMM_single_cue
#print axioms propIMM_single_cue_B
#print axioms propIMM_indep
#print axioms propIMM_no_gap
#print axioms propIMM_no_sequence_effect

-- Proposition DIV (i), (ii) and Appendix A.1
#print axioms propDIV_gap_AB_a00
#print axioms propDIV_gap_AB_a01
#print axioms propDIV_gap_AB_a10
#print axioms propDIV_gap_AB_a11
#print axioms propDIV_gap_BA_a00
#print axioms propDIV_gap_BA_a01
#print axioms propDIV_gap_BA_a10
#print axioms propDIV_gap_BA_a11
#print axioms propDIV_seq_a00
#print axioms propDIV_seq_a01
#print axioms propDIV_seq_a10
#print axioms propDIV_seq_a11
#print axioms kappa_eq_zero_iff
#print axioms kappa'_eq_zero_iff

-- Lemma ASC
#print axioms inner_gradAssoc_R1
#print axioms inner_gradAssoc_R2
#print axioms lemmaASC_first_order_vanishes
#print axioms lemmaASC_first_order_vanishes_BA

-- Lemma SEP (arbitrary N)
#print axioms isSeparable_applySteps
#print axioms sep_no_pair_interaction
#print axioms applySteps_no_pair_interaction
#print axioms sep_rescales_association

-- Lemma SCR
#print axioms lemmaSCR_gap_AB
#print axioms lemmaSCR_gap_BA
#print axioms lemmaSCR_protected_weight

-- Proposition DEC
#print axioms sep_assoc
#print axioms sep_oddsRatio
#print axioms PJab_sep
#print axioms PJba_sep
#print axioms PB_sep
#print axioms oddsRatio_PJab_eq_prior
#print axioms oddsRatio_gap_eq_zero

-- Proposition DRF
#print axioms jeffreyA_pins_mA1
#print axioms PJba_mA1
#print axioms propDRF_route_AB
#print axioms propDRF_route_BA
#print axioms propDRF
#print axioms propDRF_B_at_one
#print axioms Kdrift_pos
#print axioms Kdrift_neg

-- Proposition PRO
#print axioms R1_R2_indep
#print axioms Jmat_gradAssoc_indep
#print axioms propPRO_protection
#print axioms propPRO_protection_each
#print axioms annihilator_eq_span
#print axioms propPRO_uniqueness
#print axioms margA_not_in_span

-- Section 4.4, Theorem LOS, Proposition SHR
#print axioms flips_iff
#print axioms abs_le_of_flips
#print axioms not_flips_of_lt_cstar
#print axioms not_flips_of_pointing_away
#print axioms flipSet_eq_Ico_of_pos
#print axioms volume_flipSet
#print axioms lintegral_stake_le
#print axioms lintegral_stake_le_volume

-- Association-locality (general prior, arbitrary target marginals): the
-- association order-gap numerator is divisible by c^2, i.e. second order.
#print axioms assoc_gap_second_order
#print axioms assoc_gap_has_c_sq_factor

-- Structural version: the Jeffrey updates are DEFINED and the association
-- order-gap is DERIVED (not supplied) to equal c^2*gapQuot/(D1 D2 D3 D4).
#print axioms PaperB.assoc_order_gap_structural
