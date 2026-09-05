// Lean compiler output
// Module: AssocLocalityStructural
// Imports: public import Init public meta import Init public import Mathlib
#include <lean/lean.h>
#if defined(__clang__)
#pragma clang diagnostic ignored "-Wunused-parameter"
#pragma clang diagnostic ignored "-Wunused-label"
#elif defined(__GNUC__) && !defined(__CLANG__)
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wunused-label"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif
#ifdef __cplusplus
extern "C" {
#endif
lean_object* lp_mathlib_Field_toSemifield___redArg(lean_object*);
lean_object* lp_mathlib_Semifield_toDivisionSemiring___redArg(lean_object*);
lean_object* lp_mathlib_instDistribOfSemiring___redArg(lean_object*);
lean_object* lp_mathlib_Field_toDivisionRing___redArg(lean_object*);
lean_object* lp_mathlib_Ring_toAddGroupWithOne___redArg(lean_object*);
lean_object* lp_mathlib_AddGroupWithOne_toAddGroup___redArg(lean_object*);
lean_object* lp_mathlib_Ring_toAddCommGroup___redArg(lean_object*);
lean_object* lp_mathlib_SubNegZeroMonoid_toNegZeroClass___redArg(lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p11___redArg(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p11___redArg___boxed(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p11(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p11___boxed(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p10___redArg(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p10(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p01___redArg(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p01(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p00___redArg(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p00(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_assoc___redArg(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_assoc(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D1___redArg(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D1(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D2___redArg(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D2(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D3___redArg(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D3(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D4___redArg(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D4(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_gapQuot___redArg(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_gapQuot(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p11___redArg(lean_object* v_inst_1_, lean_object* v_a_2_, lean_object* v_b_3_, lean_object* v_c_4_){
_start:
{
lean_object* v___x_5_; lean_object* v___x_6_; lean_object* v_toSemiring_7_; lean_object* v___x_8_; lean_object* v_toMul_9_; lean_object* v_toAdd_10_; lean_object* v___x_11_; lean_object* v___x_12_; 
v___x_5_ = lp_mathlib_Field_toSemifield___redArg(v_inst_1_);
v___x_6_ = lp_mathlib_Semifield_toDivisionSemiring___redArg(v___x_5_);
v_toSemiring_7_ = lean_ctor_get(v___x_6_, 0);
lean_inc_ref(v_toSemiring_7_);
lean_dec_ref(v___x_6_);
v___x_8_ = lp_mathlib_instDistribOfSemiring___redArg(v_toSemiring_7_);
v_toMul_9_ = lean_ctor_get(v___x_8_, 0);
lean_inc(v_toMul_9_);
v_toAdd_10_ = lean_ctor_get(v___x_8_, 1);
lean_inc(v_toAdd_10_);
lean_dec_ref(v___x_8_);
v___x_11_ = lean_apply_2(v_toMul_9_, v_a_2_, v_b_3_);
v___x_12_ = lean_apply_2(v_toAdd_10_, v___x_11_, v_c_4_);
return v___x_12_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p11___redArg___boxed(lean_object* v_inst_13_, lean_object* v_a_14_, lean_object* v_b_15_, lean_object* v_c_16_){
_start:
{
lean_object* v_res_17_; 
v_res_17_ = lp_jeffreyorder_PaperB_p11___redArg(v_inst_13_, v_a_14_, v_b_15_, v_c_16_);
lean_dec_ref(v_inst_13_);
return v_res_17_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p11(lean_object* v_K_18_, lean_object* v_inst_19_, lean_object* v_a_20_, lean_object* v_b_21_, lean_object* v_c_22_){
_start:
{
lean_object* v___x_23_; 
v___x_23_ = lp_jeffreyorder_PaperB_p11___redArg(v_inst_19_, v_a_20_, v_b_21_, v_c_22_);
return v___x_23_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p11___boxed(lean_object* v_K_24_, lean_object* v_inst_25_, lean_object* v_a_26_, lean_object* v_b_27_, lean_object* v_c_28_){
_start:
{
lean_object* v_res_29_; 
v_res_29_ = lp_jeffreyorder_PaperB_p11(v_K_24_, v_inst_25_, v_a_26_, v_b_27_, v_c_28_);
lean_dec_ref(v_inst_25_);
return v_res_29_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p10___redArg(lean_object* v_inst_30_, lean_object* v_a_31_, lean_object* v_b_32_, lean_object* v_c_33_){
_start:
{
lean_object* v___x_34_; lean_object* v_toRing_35_; lean_object* v___x_36_; lean_object* v___x_37_; lean_object* v_toSub_38_; lean_object* v___x_39_; lean_object* v___x_40_; lean_object* v_toSemiring_41_; lean_object* v___x_42_; lean_object* v_toAddMonoidWithOne_43_; lean_object* v_toMul_44_; lean_object* v_toOne_45_; lean_object* v___x_46_; lean_object* v___x_47_; lean_object* v___x_48_; 
lean_inc_ref(v_inst_30_);
v___x_34_ = lp_mathlib_Field_toDivisionRing___redArg(v_inst_30_);
v_toRing_35_ = lean_ctor_get(v___x_34_, 0);
lean_inc_ref(v_toRing_35_);
lean_dec_ref(v___x_34_);
v___x_36_ = lp_mathlib_Ring_toAddGroupWithOne___redArg(v_toRing_35_);
v___x_37_ = lp_mathlib_AddGroupWithOne_toAddGroup___redArg(v___x_36_);
v_toSub_38_ = lean_ctor_get(v___x_37_, 2);
lean_inc_n(v_toSub_38_, 2);
lean_dec_ref(v___x_37_);
v___x_39_ = lp_mathlib_Field_toSemifield___redArg(v_inst_30_);
lean_dec_ref(v_inst_30_);
v___x_40_ = lp_mathlib_Semifield_toDivisionSemiring___redArg(v___x_39_);
v_toSemiring_41_ = lean_ctor_get(v___x_40_, 0);
lean_inc_ref(v_toSemiring_41_);
lean_dec_ref(v___x_40_);
v___x_42_ = lp_mathlib_instDistribOfSemiring___redArg(v_toSemiring_41_);
v_toAddMonoidWithOne_43_ = lean_ctor_get(v___x_36_, 1);
lean_inc_ref(v_toAddMonoidWithOne_43_);
lean_dec_ref(v___x_36_);
v_toMul_44_ = lean_ctor_get(v___x_42_, 0);
lean_inc(v_toMul_44_);
lean_dec_ref(v___x_42_);
v_toOne_45_ = lean_ctor_get(v_toAddMonoidWithOne_43_, 2);
lean_inc(v_toOne_45_);
lean_dec_ref(v_toAddMonoidWithOne_43_);
v___x_46_ = lean_apply_2(v_toSub_38_, v_toOne_45_, v_b_32_);
v___x_47_ = lean_apply_2(v_toMul_44_, v_a_31_, v___x_46_);
v___x_48_ = lean_apply_2(v_toSub_38_, v___x_47_, v_c_33_);
return v___x_48_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p10(lean_object* v_K_49_, lean_object* v_inst_50_, lean_object* v_a_51_, lean_object* v_b_52_, lean_object* v_c_53_){
_start:
{
lean_object* v___x_54_; 
v___x_54_ = lp_jeffreyorder_PaperB_p10___redArg(v_inst_50_, v_a_51_, v_b_52_, v_c_53_);
return v___x_54_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p01___redArg(lean_object* v_inst_55_, lean_object* v_a_56_, lean_object* v_b_57_, lean_object* v_c_58_){
_start:
{
lean_object* v___x_59_; lean_object* v_toRing_60_; lean_object* v___x_61_; lean_object* v___x_62_; lean_object* v_toSub_63_; lean_object* v___x_64_; lean_object* v___x_65_; lean_object* v_toSemiring_66_; lean_object* v___x_67_; lean_object* v_toAddMonoidWithOne_68_; lean_object* v_toMul_69_; lean_object* v_toOne_70_; lean_object* v___x_71_; lean_object* v___x_72_; lean_object* v___x_73_; 
lean_inc_ref(v_inst_55_);
v___x_59_ = lp_mathlib_Field_toDivisionRing___redArg(v_inst_55_);
v_toRing_60_ = lean_ctor_get(v___x_59_, 0);
lean_inc_ref(v_toRing_60_);
lean_dec_ref(v___x_59_);
v___x_61_ = lp_mathlib_Ring_toAddGroupWithOne___redArg(v_toRing_60_);
v___x_62_ = lp_mathlib_AddGroupWithOne_toAddGroup___redArg(v___x_61_);
v_toSub_63_ = lean_ctor_get(v___x_62_, 2);
lean_inc_n(v_toSub_63_, 2);
lean_dec_ref(v___x_62_);
v___x_64_ = lp_mathlib_Field_toSemifield___redArg(v_inst_55_);
lean_dec_ref(v_inst_55_);
v___x_65_ = lp_mathlib_Semifield_toDivisionSemiring___redArg(v___x_64_);
v_toSemiring_66_ = lean_ctor_get(v___x_65_, 0);
lean_inc_ref(v_toSemiring_66_);
lean_dec_ref(v___x_65_);
v___x_67_ = lp_mathlib_instDistribOfSemiring___redArg(v_toSemiring_66_);
v_toAddMonoidWithOne_68_ = lean_ctor_get(v___x_61_, 1);
lean_inc_ref(v_toAddMonoidWithOne_68_);
lean_dec_ref(v___x_61_);
v_toMul_69_ = lean_ctor_get(v___x_67_, 0);
lean_inc(v_toMul_69_);
lean_dec_ref(v___x_67_);
v_toOne_70_ = lean_ctor_get(v_toAddMonoidWithOne_68_, 2);
lean_inc(v_toOne_70_);
lean_dec_ref(v_toAddMonoidWithOne_68_);
v___x_71_ = lean_apply_2(v_toSub_63_, v_toOne_70_, v_a_56_);
v___x_72_ = lean_apply_2(v_toMul_69_, v___x_71_, v_b_57_);
v___x_73_ = lean_apply_2(v_toSub_63_, v___x_72_, v_c_58_);
return v___x_73_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p01(lean_object* v_K_74_, lean_object* v_inst_75_, lean_object* v_a_76_, lean_object* v_b_77_, lean_object* v_c_78_){
_start:
{
lean_object* v___x_79_; 
v___x_79_ = lp_jeffreyorder_PaperB_p01___redArg(v_inst_75_, v_a_76_, v_b_77_, v_c_78_);
return v___x_79_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p00___redArg(lean_object* v_inst_80_, lean_object* v_a_81_, lean_object* v_b_82_, lean_object* v_c_83_){
_start:
{
lean_object* v___x_84_; lean_object* v___x_85_; lean_object* v_toSemiring_86_; lean_object* v___x_87_; lean_object* v_toMul_88_; lean_object* v_toAdd_89_; lean_object* v___x_90_; lean_object* v_toRing_91_; lean_object* v___x_92_; lean_object* v___x_93_; lean_object* v_toAddMonoidWithOne_94_; lean_object* v_toSub_95_; lean_object* v_toOne_96_; lean_object* v___x_97_; lean_object* v___x_98_; lean_object* v___x_99_; lean_object* v___x_100_; 
v___x_84_ = lp_mathlib_Field_toSemifield___redArg(v_inst_80_);
v___x_85_ = lp_mathlib_Semifield_toDivisionSemiring___redArg(v___x_84_);
v_toSemiring_86_ = lean_ctor_get(v___x_85_, 0);
lean_inc_ref(v_toSemiring_86_);
lean_dec_ref(v___x_85_);
v___x_87_ = lp_mathlib_instDistribOfSemiring___redArg(v_toSemiring_86_);
v_toMul_88_ = lean_ctor_get(v___x_87_, 0);
lean_inc(v_toMul_88_);
v_toAdd_89_ = lean_ctor_get(v___x_87_, 1);
lean_inc(v_toAdd_89_);
lean_dec_ref(v___x_87_);
v___x_90_ = lp_mathlib_Field_toDivisionRing___redArg(v_inst_80_);
v_toRing_91_ = lean_ctor_get(v___x_90_, 0);
lean_inc_ref(v_toRing_91_);
lean_dec_ref(v___x_90_);
v___x_92_ = lp_mathlib_Ring_toAddGroupWithOne___redArg(v_toRing_91_);
v___x_93_ = lp_mathlib_AddGroupWithOne_toAddGroup___redArg(v___x_92_);
v_toAddMonoidWithOne_94_ = lean_ctor_get(v___x_92_, 1);
lean_inc_ref(v_toAddMonoidWithOne_94_);
lean_dec_ref(v___x_92_);
v_toSub_95_ = lean_ctor_get(v___x_93_, 2);
lean_inc_n(v_toSub_95_, 2);
lean_dec_ref(v___x_93_);
v_toOne_96_ = lean_ctor_get(v_toAddMonoidWithOne_94_, 2);
lean_inc_n(v_toOne_96_, 2);
lean_dec_ref(v_toAddMonoidWithOne_94_);
v___x_97_ = lean_apply_2(v_toSub_95_, v_toOne_96_, v_a_81_);
v___x_98_ = lean_apply_2(v_toSub_95_, v_toOne_96_, v_b_82_);
v___x_99_ = lean_apply_2(v_toMul_88_, v___x_97_, v___x_98_);
v___x_100_ = lean_apply_2(v_toAdd_89_, v___x_99_, v_c_83_);
return v___x_100_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_p00(lean_object* v_K_101_, lean_object* v_inst_102_, lean_object* v_a_103_, lean_object* v_b_104_, lean_object* v_c_105_){
_start:
{
lean_object* v___x_106_; 
v___x_106_ = lp_jeffreyorder_PaperB_p00___redArg(v_inst_102_, v_a_103_, v_b_104_, v_c_105_);
return v___x_106_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_assoc___redArg(lean_object* v_inst_107_, lean_object* v_q11_108_, lean_object* v_q10_109_, lean_object* v_q01_110_, lean_object* v_q00_111_){
_start:
{
lean_object* v___x_112_; lean_object* v_toRing_113_; lean_object* v___x_114_; lean_object* v___x_115_; lean_object* v_toSub_116_; lean_object* v___x_117_; lean_object* v___x_118_; lean_object* v_toSemiring_119_; lean_object* v___x_120_; lean_object* v_toMul_121_; lean_object* v___x_122_; lean_object* v___x_123_; lean_object* v___x_124_; 
lean_inc_ref(v_inst_107_);
v___x_112_ = lp_mathlib_Field_toDivisionRing___redArg(v_inst_107_);
v_toRing_113_ = lean_ctor_get(v___x_112_, 0);
lean_inc_ref(v_toRing_113_);
lean_dec_ref(v___x_112_);
v___x_114_ = lp_mathlib_Ring_toAddGroupWithOne___redArg(v_toRing_113_);
v___x_115_ = lp_mathlib_AddGroupWithOne_toAddGroup___redArg(v___x_114_);
lean_dec_ref(v___x_114_);
v_toSub_116_ = lean_ctor_get(v___x_115_, 2);
lean_inc(v_toSub_116_);
lean_dec_ref(v___x_115_);
v___x_117_ = lp_mathlib_Field_toSemifield___redArg(v_inst_107_);
lean_dec_ref(v_inst_107_);
v___x_118_ = lp_mathlib_Semifield_toDivisionSemiring___redArg(v___x_117_);
v_toSemiring_119_ = lean_ctor_get(v___x_118_, 0);
lean_inc_ref(v_toSemiring_119_);
lean_dec_ref(v___x_118_);
v___x_120_ = lp_mathlib_instDistribOfSemiring___redArg(v_toSemiring_119_);
v_toMul_121_ = lean_ctor_get(v___x_120_, 0);
lean_inc_n(v_toMul_121_, 2);
lean_dec_ref(v___x_120_);
v___x_122_ = lean_apply_2(v_toMul_121_, v_q11_108_, v_q00_111_);
v___x_123_ = lean_apply_2(v_toMul_121_, v_q10_109_, v_q01_110_);
v___x_124_ = lean_apply_2(v_toSub_116_, v___x_122_, v___x_123_);
return v___x_124_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_assoc(lean_object* v_K_125_, lean_object* v_inst_126_, lean_object* v_q11_127_, lean_object* v_q10_128_, lean_object* v_q01_129_, lean_object* v_q00_130_){
_start:
{
lean_object* v___x_131_; 
v___x_131_ = lp_jeffreyorder_PaperB_assoc___redArg(v_inst_126_, v_q11_127_, v_q10_128_, v_q01_129_, v_q00_130_);
return v___x_131_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D1___redArg(lean_object* v_inst_132_, lean_object* v_a_133_, lean_object* v_b_134_, lean_object* v_c_135_, lean_object* v_tB_136_){
_start:
{
lean_object* v___x_137_; lean_object* v_toRing_138_; lean_object* v___x_139_; lean_object* v___x_140_; lean_object* v_toSub_141_; lean_object* v___x_142_; lean_object* v___x_143_; lean_object* v_toSemiring_144_; lean_object* v___x_145_; lean_object* v_toMonoid_146_; lean_object* v_toMul_147_; lean_object* v_toAdd_148_; lean_object* v_npow_149_; lean_object* v___x_150_; lean_object* v___x_151_; lean_object* v___x_152_; lean_object* v___x_153_; lean_object* v___x_154_; lean_object* v___x_155_; lean_object* v___x_156_; lean_object* v___x_157_; lean_object* v___x_158_; lean_object* v___x_159_; lean_object* v___x_160_; 
lean_inc_ref(v_inst_132_);
v___x_137_ = lp_mathlib_Field_toDivisionRing___redArg(v_inst_132_);
v_toRing_138_ = lean_ctor_get(v___x_137_, 0);
lean_inc_ref(v_toRing_138_);
lean_dec_ref(v___x_137_);
v___x_139_ = lp_mathlib_Ring_toAddGroupWithOne___redArg(v_toRing_138_);
v___x_140_ = lp_mathlib_AddGroupWithOne_toAddGroup___redArg(v___x_139_);
lean_dec_ref(v___x_139_);
v_toSub_141_ = lean_ctor_get(v___x_140_, 2);
lean_inc_n(v_toSub_141_, 3);
lean_dec_ref(v___x_140_);
v___x_142_ = lp_mathlib_Field_toSemifield___redArg(v_inst_132_);
lean_dec_ref(v_inst_132_);
v___x_143_ = lp_mathlib_Semifield_toDivisionSemiring___redArg(v___x_142_);
v_toSemiring_144_ = lean_ctor_get(v___x_143_, 0);
lean_inc_ref_n(v_toSemiring_144_, 2);
lean_dec_ref(v___x_143_);
v___x_145_ = lp_mathlib_instDistribOfSemiring___redArg(v_toSemiring_144_);
v_toMonoid_146_ = lean_ctor_get(v_toSemiring_144_, 1);
lean_inc_ref(v_toMonoid_146_);
lean_dec_ref(v_toSemiring_144_);
v_toMul_147_ = lean_ctor_get(v___x_145_, 0);
lean_inc_n(v_toMul_147_, 4);
v_toAdd_148_ = lean_ctor_get(v___x_145_, 1);
lean_inc_n(v_toAdd_148_, 2);
lean_dec_ref(v___x_145_);
v_npow_149_ = lean_ctor_get(v_toMonoid_146_, 2);
lean_inc(v_npow_149_);
lean_dec_ref(v_toMonoid_146_);
v___x_150_ = lean_unsigned_to_nat(2u);
lean_inc_n(v_b_134_, 3);
v___x_151_ = lean_apply_2(v_npow_149_, v___x_150_, v_b_134_);
lean_inc(v___x_151_);
lean_inc(v_a_133_);
v___x_152_ = lean_apply_2(v_toMul_147_, v_a_133_, v___x_151_);
v___x_153_ = lean_apply_2(v_toMul_147_, v_a_133_, v_b_134_);
v___x_154_ = lean_apply_2(v_toSub_141_, v___x_152_, v___x_153_);
v___x_155_ = lean_apply_2(v_toSub_141_, v___x_154_, v___x_151_);
lean_inc(v_c_135_);
v___x_156_ = lean_apply_2(v_toMul_147_, v_b_134_, v_c_135_);
v___x_157_ = lean_apply_2(v_toAdd_148_, v___x_155_, v___x_156_);
v___x_158_ = lean_apply_2(v_toAdd_148_, v___x_157_, v_b_134_);
v___x_159_ = lean_apply_2(v_toMul_147_, v_c_135_, v_tB_136_);
v___x_160_ = lean_apply_2(v_toSub_141_, v___x_158_, v___x_159_);
return v___x_160_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D1(lean_object* v_K_161_, lean_object* v_inst_162_, lean_object* v_a_163_, lean_object* v_b_164_, lean_object* v_c_165_, lean_object* v_tB_166_){
_start:
{
lean_object* v___x_167_; 
v___x_167_ = lp_jeffreyorder_PaperB_D1___redArg(v_inst_162_, v_a_163_, v_b_164_, v_c_165_, v_tB_166_);
return v___x_167_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D2___redArg(lean_object* v_inst_168_, lean_object* v_a_169_, lean_object* v_b_170_, lean_object* v_c_171_, lean_object* v_tB_172_){
_start:
{
lean_object* v___x_173_; lean_object* v_toRing_174_; lean_object* v___x_175_; lean_object* v___x_176_; lean_object* v_toSub_177_; lean_object* v___x_178_; lean_object* v___x_179_; lean_object* v_toSemiring_180_; lean_object* v___x_181_; lean_object* v_toMonoid_182_; lean_object* v_toMul_183_; lean_object* v_toAdd_184_; lean_object* v_npow_185_; lean_object* v___x_186_; lean_object* v___x_187_; lean_object* v___x_188_; lean_object* v___x_189_; lean_object* v___x_190_; lean_object* v___x_191_; lean_object* v___x_192_; lean_object* v___x_193_; lean_object* v___x_194_; 
lean_inc_ref(v_inst_168_);
v___x_173_ = lp_mathlib_Field_toDivisionRing___redArg(v_inst_168_);
v_toRing_174_ = lean_ctor_get(v___x_173_, 0);
lean_inc_ref(v_toRing_174_);
lean_dec_ref(v___x_173_);
v___x_175_ = lp_mathlib_Ring_toAddGroupWithOne___redArg(v_toRing_174_);
v___x_176_ = lp_mathlib_AddGroupWithOne_toAddGroup___redArg(v___x_175_);
lean_dec_ref(v___x_175_);
v_toSub_177_ = lean_ctor_get(v___x_176_, 2);
lean_inc_n(v_toSub_177_, 2);
lean_dec_ref(v___x_176_);
v___x_178_ = lp_mathlib_Field_toSemifield___redArg(v_inst_168_);
lean_dec_ref(v_inst_168_);
v___x_179_ = lp_mathlib_Semifield_toDivisionSemiring___redArg(v___x_178_);
v_toSemiring_180_ = lean_ctor_get(v___x_179_, 0);
lean_inc_ref_n(v_toSemiring_180_, 2);
lean_dec_ref(v___x_179_);
v___x_181_ = lp_mathlib_instDistribOfSemiring___redArg(v_toSemiring_180_);
v_toMonoid_182_ = lean_ctor_get(v_toSemiring_180_, 1);
lean_inc_ref(v_toMonoid_182_);
lean_dec_ref(v_toSemiring_180_);
v_toMul_183_ = lean_ctor_get(v___x_181_, 0);
lean_inc_n(v_toMul_183_, 4);
v_toAdd_184_ = lean_ctor_get(v___x_181_, 1);
lean_inc(v_toAdd_184_);
lean_dec_ref(v___x_181_);
v_npow_185_ = lean_ctor_get(v_toMonoid_182_, 2);
lean_inc(v_npow_185_);
lean_dec_ref(v_toMonoid_182_);
v___x_186_ = lean_unsigned_to_nat(2u);
lean_inc_n(v_b_170_, 2);
v___x_187_ = lean_apply_2(v_npow_185_, v___x_186_, v_b_170_);
lean_inc(v_a_169_);
v___x_188_ = lean_apply_2(v_toMul_183_, v_a_169_, v___x_187_);
v___x_189_ = lean_apply_2(v_toMul_183_, v_a_169_, v_b_170_);
v___x_190_ = lean_apply_2(v_toSub_177_, v___x_188_, v___x_189_);
lean_inc(v_c_171_);
v___x_191_ = lean_apply_2(v_toMul_183_, v_b_170_, v_c_171_);
v___x_192_ = lean_apply_2(v_toAdd_184_, v___x_190_, v___x_191_);
v___x_193_ = lean_apply_2(v_toMul_183_, v_c_171_, v_tB_172_);
v___x_194_ = lean_apply_2(v_toSub_177_, v___x_192_, v___x_193_);
return v___x_194_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D2(lean_object* v_K_195_, lean_object* v_inst_196_, lean_object* v_a_197_, lean_object* v_b_198_, lean_object* v_c_199_, lean_object* v_tB_200_){
_start:
{
lean_object* v___x_201_; 
v___x_201_ = lp_jeffreyorder_PaperB_D2___redArg(v_inst_196_, v_a_197_, v_b_198_, v_c_199_, v_tB_200_);
return v___x_201_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D3___redArg(lean_object* v_inst_202_, lean_object* v_a_203_, lean_object* v_b_204_, lean_object* v_c_205_, lean_object* v_tA_206_){
_start:
{
lean_object* v___x_207_; lean_object* v_toRing_208_; lean_object* v___x_209_; lean_object* v___x_210_; lean_object* v_toSub_211_; lean_object* v___x_212_; lean_object* v___x_213_; lean_object* v_toSemiring_214_; lean_object* v___x_215_; lean_object* v_toMonoid_216_; lean_object* v_toMul_217_; lean_object* v_toAdd_218_; lean_object* v_npow_219_; lean_object* v___x_220_; lean_object* v___x_221_; lean_object* v___x_222_; lean_object* v___x_223_; lean_object* v___x_224_; lean_object* v___x_225_; lean_object* v___x_226_; lean_object* v___x_227_; lean_object* v___x_228_; 
lean_inc_ref(v_inst_202_);
v___x_207_ = lp_mathlib_Field_toDivisionRing___redArg(v_inst_202_);
v_toRing_208_ = lean_ctor_get(v___x_207_, 0);
lean_inc_ref(v_toRing_208_);
lean_dec_ref(v___x_207_);
v___x_209_ = lp_mathlib_Ring_toAddGroupWithOne___redArg(v_toRing_208_);
v___x_210_ = lp_mathlib_AddGroupWithOne_toAddGroup___redArg(v___x_209_);
lean_dec_ref(v___x_209_);
v_toSub_211_ = lean_ctor_get(v___x_210_, 2);
lean_inc_n(v_toSub_211_, 2);
lean_dec_ref(v___x_210_);
v___x_212_ = lp_mathlib_Field_toSemifield___redArg(v_inst_202_);
lean_dec_ref(v_inst_202_);
v___x_213_ = lp_mathlib_Semifield_toDivisionSemiring___redArg(v___x_212_);
v_toSemiring_214_ = lean_ctor_get(v___x_213_, 0);
lean_inc_ref_n(v_toSemiring_214_, 2);
lean_dec_ref(v___x_213_);
v___x_215_ = lp_mathlib_instDistribOfSemiring___redArg(v_toSemiring_214_);
v_toMonoid_216_ = lean_ctor_get(v_toSemiring_214_, 1);
lean_inc_ref(v_toMonoid_216_);
lean_dec_ref(v_toSemiring_214_);
v_toMul_217_ = lean_ctor_get(v___x_215_, 0);
lean_inc_n(v_toMul_217_, 4);
v_toAdd_218_ = lean_ctor_get(v___x_215_, 1);
lean_inc(v_toAdd_218_);
lean_dec_ref(v___x_215_);
v_npow_219_ = lean_ctor_get(v_toMonoid_216_, 2);
lean_inc(v_npow_219_);
lean_dec_ref(v_toMonoid_216_);
v___x_220_ = lean_unsigned_to_nat(2u);
lean_inc_n(v_a_203_, 2);
v___x_221_ = lean_apply_2(v_npow_219_, v___x_220_, v_a_203_);
lean_inc(v_b_204_);
v___x_222_ = lean_apply_2(v_toMul_217_, v___x_221_, v_b_204_);
v___x_223_ = lean_apply_2(v_toMul_217_, v_a_203_, v_b_204_);
v___x_224_ = lean_apply_2(v_toSub_211_, v___x_222_, v___x_223_);
lean_inc(v_c_205_);
v___x_225_ = lean_apply_2(v_toMul_217_, v_a_203_, v_c_205_);
v___x_226_ = lean_apply_2(v_toAdd_218_, v___x_224_, v___x_225_);
v___x_227_ = lean_apply_2(v_toMul_217_, v_c_205_, v_tA_206_);
v___x_228_ = lean_apply_2(v_toSub_211_, v___x_226_, v___x_227_);
return v___x_228_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D3(lean_object* v_K_229_, lean_object* v_inst_230_, lean_object* v_a_231_, lean_object* v_b_232_, lean_object* v_c_233_, lean_object* v_tA_234_){
_start:
{
lean_object* v___x_235_; 
v___x_235_ = lp_jeffreyorder_PaperB_D3___redArg(v_inst_230_, v_a_231_, v_b_232_, v_c_233_, v_tA_234_);
return v___x_235_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D4___redArg(lean_object* v_inst_236_, lean_object* v_a_237_, lean_object* v_b_238_, lean_object* v_c_239_, lean_object* v_tA_240_){
_start:
{
lean_object* v___x_241_; lean_object* v_toRing_242_; lean_object* v___x_243_; lean_object* v___x_244_; lean_object* v_toSub_245_; lean_object* v___x_246_; lean_object* v___x_247_; lean_object* v_toSemiring_248_; lean_object* v___x_249_; lean_object* v_toMonoid_250_; lean_object* v_toMul_251_; lean_object* v_toAdd_252_; lean_object* v_npow_253_; lean_object* v___x_254_; lean_object* v___x_255_; lean_object* v___x_256_; lean_object* v___x_257_; lean_object* v___x_258_; lean_object* v___x_259_; lean_object* v___x_260_; lean_object* v___x_261_; lean_object* v___x_262_; lean_object* v___x_263_; lean_object* v___x_264_; 
lean_inc_ref(v_inst_236_);
v___x_241_ = lp_mathlib_Field_toDivisionRing___redArg(v_inst_236_);
v_toRing_242_ = lean_ctor_get(v___x_241_, 0);
lean_inc_ref(v_toRing_242_);
lean_dec_ref(v___x_241_);
v___x_243_ = lp_mathlib_Ring_toAddGroupWithOne___redArg(v_toRing_242_);
v___x_244_ = lp_mathlib_AddGroupWithOne_toAddGroup___redArg(v___x_243_);
lean_dec_ref(v___x_243_);
v_toSub_245_ = lean_ctor_get(v___x_244_, 2);
lean_inc_n(v_toSub_245_, 3);
lean_dec_ref(v___x_244_);
v___x_246_ = lp_mathlib_Field_toSemifield___redArg(v_inst_236_);
lean_dec_ref(v_inst_236_);
v___x_247_ = lp_mathlib_Semifield_toDivisionSemiring___redArg(v___x_246_);
v_toSemiring_248_ = lean_ctor_get(v___x_247_, 0);
lean_inc_ref_n(v_toSemiring_248_, 2);
lean_dec_ref(v___x_247_);
v___x_249_ = lp_mathlib_instDistribOfSemiring___redArg(v_toSemiring_248_);
v_toMonoid_250_ = lean_ctor_get(v_toSemiring_248_, 1);
lean_inc_ref(v_toMonoid_250_);
lean_dec_ref(v_toSemiring_248_);
v_toMul_251_ = lean_ctor_get(v___x_249_, 0);
lean_inc_n(v_toMul_251_, 4);
v_toAdd_252_ = lean_ctor_get(v___x_249_, 1);
lean_inc_n(v_toAdd_252_, 2);
lean_dec_ref(v___x_249_);
v_npow_253_ = lean_ctor_get(v_toMonoid_250_, 2);
lean_inc(v_npow_253_);
lean_dec_ref(v_toMonoid_250_);
v___x_254_ = lean_unsigned_to_nat(2u);
lean_inc_n(v_a_237_, 3);
v___x_255_ = lean_apply_2(v_npow_253_, v___x_254_, v_a_237_);
lean_inc(v_b_238_);
lean_inc(v___x_255_);
v___x_256_ = lean_apply_2(v_toMul_251_, v___x_255_, v_b_238_);
v___x_257_ = lean_apply_2(v_toSub_245_, v___x_256_, v___x_255_);
v___x_258_ = lean_apply_2(v_toMul_251_, v_a_237_, v_b_238_);
v___x_259_ = lean_apply_2(v_toSub_245_, v___x_257_, v___x_258_);
lean_inc(v_c_239_);
v___x_260_ = lean_apply_2(v_toMul_251_, v_a_237_, v_c_239_);
v___x_261_ = lean_apply_2(v_toAdd_252_, v___x_259_, v___x_260_);
v___x_262_ = lean_apply_2(v_toAdd_252_, v___x_261_, v_a_237_);
v___x_263_ = lean_apply_2(v_toMul_251_, v_c_239_, v_tA_240_);
v___x_264_ = lean_apply_2(v_toSub_245_, v___x_262_, v___x_263_);
return v___x_264_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_D4(lean_object* v_K_265_, lean_object* v_inst_266_, lean_object* v_a_267_, lean_object* v_b_268_, lean_object* v_c_269_, lean_object* v_tA_270_){
_start:
{
lean_object* v___x_271_; 
v___x_271_ = lp_jeffreyorder_PaperB_D4___redArg(v_inst_266_, v_a_267_, v_b_268_, v_c_269_, v_tA_270_);
return v___x_271_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_gapQuot___redArg(lean_object* v_inst_272_, lean_object* v_a_273_, lean_object* v_b_274_, lean_object* v_c_275_, lean_object* v_tA_276_, lean_object* v_tB_277_){
_start:
{
lean_object* v___x_278_; lean_object* v___x_279_; lean_object* v_toSemiring_280_; lean_object* v___x_281_; lean_object* v_toMul_282_; lean_object* v_toAdd_283_; lean_object* v___x_284_; lean_object* v_toRing_285_; lean_object* v___x_286_; lean_object* v___x_287_; lean_object* v_toSub_288_; lean_object* v___x_289_; lean_object* v___x_290_; lean_object* v_toAddMonoidWithOne_291_; lean_object* v_toMonoid_292_; lean_object* v_toNeg_293_; lean_object* v_toNatCast_294_; lean_object* v_npow_295_; lean_object* v___x_296_; lean_object* v___x_297_; lean_object* v___x_298_; lean_object* v___x_299_; lean_object* v___x_300_; lean_object* v___x_301_; lean_object* v___x_302_; lean_object* v___x_303_; lean_object* v___x_304_; lean_object* v___x_305_; lean_object* v___x_306_; lean_object* v___x_307_; lean_object* v___x_308_; lean_object* v___x_309_; lean_object* v___x_310_; lean_object* v___x_311_; lean_object* v___x_312_; lean_object* v___x_313_; lean_object* v___x_314_; lean_object* v___x_315_; lean_object* v___x_316_; lean_object* v___x_317_; lean_object* v___x_318_; lean_object* v___x_319_; lean_object* v___x_320_; lean_object* v___x_321_; lean_object* v___x_322_; lean_object* v___x_323_; lean_object* v___x_324_; lean_object* v___x_325_; lean_object* v___x_326_; lean_object* v___x_327_; lean_object* v___x_328_; lean_object* v___x_329_; lean_object* v___x_330_; lean_object* v___x_331_; lean_object* v___x_332_; lean_object* v___x_333_; lean_object* v___x_334_; lean_object* v___x_335_; lean_object* v___x_336_; lean_object* v___x_337_; lean_object* v___x_338_; lean_object* v___x_339_; lean_object* v___x_340_; lean_object* v___x_341_; lean_object* v___x_342_; lean_object* v___x_343_; lean_object* v___x_344_; lean_object* v___x_345_; lean_object* v___x_346_; lean_object* v___x_347_; lean_object* v___x_348_; lean_object* v___x_349_; lean_object* v___x_350_; lean_object* v___x_351_; lean_object* v___x_352_; lean_object* v___x_353_; lean_object* v___x_354_; lean_object* v___x_355_; lean_object* v___x_356_; lean_object* v___x_357_; lean_object* v___x_358_; lean_object* v___x_359_; lean_object* v___x_360_; lean_object* v___x_361_; lean_object* v___x_362_; lean_object* v___x_363_; lean_object* v___x_364_; lean_object* v___x_365_; lean_object* v___x_366_; lean_object* v___x_367_; lean_object* v___x_368_; lean_object* v___x_369_; lean_object* v___x_370_; lean_object* v___x_371_; lean_object* v___x_372_; lean_object* v___x_373_; lean_object* v___x_374_; lean_object* v___x_375_; lean_object* v___x_376_; lean_object* v___x_377_; lean_object* v___x_378_; lean_object* v___x_379_; lean_object* v___x_380_; lean_object* v___x_381_; lean_object* v___x_382_; lean_object* v___x_383_; lean_object* v___x_384_; lean_object* v___x_385_; lean_object* v___x_386_; lean_object* v___x_387_; lean_object* v___x_388_; lean_object* v___x_389_; lean_object* v___x_390_; lean_object* v___x_391_; lean_object* v___x_392_; lean_object* v___x_393_; lean_object* v___x_394_; lean_object* v___x_395_; lean_object* v___x_396_; lean_object* v___x_397_; lean_object* v___x_398_; lean_object* v___x_399_; lean_object* v___x_400_; lean_object* v___x_401_; lean_object* v___x_402_; lean_object* v___x_403_; lean_object* v___x_404_; lean_object* v___x_405_; lean_object* v___x_406_; lean_object* v___x_407_; lean_object* v___x_408_; lean_object* v___x_409_; lean_object* v___x_410_; lean_object* v___x_411_; lean_object* v___x_412_; lean_object* v___x_413_; lean_object* v___x_414_; lean_object* v___x_415_; lean_object* v___x_416_; lean_object* v___x_417_; lean_object* v___x_418_; lean_object* v___x_419_; lean_object* v___x_420_; lean_object* v___x_421_; lean_object* v___x_422_; lean_object* v___x_423_; lean_object* v___x_424_; lean_object* v___x_425_; lean_object* v___x_426_; lean_object* v___x_427_; lean_object* v___x_428_; lean_object* v___x_429_; lean_object* v___x_430_; lean_object* v___x_431_; lean_object* v___x_432_; lean_object* v___x_433_; lean_object* v___x_434_; lean_object* v___x_435_; lean_object* v___x_436_; lean_object* v___x_437_; lean_object* v___x_438_; lean_object* v___x_439_; lean_object* v___x_440_; lean_object* v___x_441_; lean_object* v___x_442_; lean_object* v___x_443_; lean_object* v___x_444_; lean_object* v___x_445_; lean_object* v___x_446_; lean_object* v___x_447_; lean_object* v___x_448_; lean_object* v___x_449_; lean_object* v___x_450_; lean_object* v___x_451_; lean_object* v___x_452_; lean_object* v___x_453_; lean_object* v___x_454_; lean_object* v___x_455_; lean_object* v___x_456_; lean_object* v___x_457_; lean_object* v___x_458_; lean_object* v___x_459_; lean_object* v___x_460_; lean_object* v___x_461_; lean_object* v___x_462_; lean_object* v___x_463_; lean_object* v___x_464_; lean_object* v___x_465_; lean_object* v___x_466_; lean_object* v___x_467_; lean_object* v___x_468_; lean_object* v___x_469_; lean_object* v___x_470_; lean_object* v___x_471_; lean_object* v___x_472_; lean_object* v___x_473_; lean_object* v___x_474_; lean_object* v___x_475_; lean_object* v___x_476_; lean_object* v___x_477_; lean_object* v___x_478_; lean_object* v___x_479_; lean_object* v___x_480_; lean_object* v___x_481_; lean_object* v___x_482_; lean_object* v___x_483_; lean_object* v___x_484_; lean_object* v___x_485_; lean_object* v___x_486_; lean_object* v___x_487_; lean_object* v___x_488_; lean_object* v___x_489_; lean_object* v___x_490_; lean_object* v___x_491_; lean_object* v___x_492_; lean_object* v___x_493_; lean_object* v___x_494_; lean_object* v___x_495_; lean_object* v___x_496_; lean_object* v___x_497_; lean_object* v___x_498_; lean_object* v___x_499_; lean_object* v___x_500_; lean_object* v___x_501_; lean_object* v___x_502_; lean_object* v___x_503_; lean_object* v___x_504_; lean_object* v___x_505_; lean_object* v___x_506_; lean_object* v___x_507_; lean_object* v___x_508_; lean_object* v___x_509_; lean_object* v___x_510_; lean_object* v___x_511_; lean_object* v___x_512_; lean_object* v___x_513_; lean_object* v___x_514_; lean_object* v___x_515_; lean_object* v___x_516_; lean_object* v___x_517_; lean_object* v___x_518_; lean_object* v___x_519_; lean_object* v___x_520_; lean_object* v___x_521_; lean_object* v___x_522_; lean_object* v___x_523_; lean_object* v___x_524_; lean_object* v___x_525_; lean_object* v___x_526_; lean_object* v___x_527_; lean_object* v___x_528_; lean_object* v___x_529_; lean_object* v___x_530_; lean_object* v___x_531_; lean_object* v___x_532_; lean_object* v___x_533_; lean_object* v___x_534_; lean_object* v___x_535_; lean_object* v___x_536_; lean_object* v___x_537_; lean_object* v___x_538_; lean_object* v___x_539_; lean_object* v___x_540_; lean_object* v___x_541_; lean_object* v___x_542_; lean_object* v___x_543_; lean_object* v___x_544_; lean_object* v___x_545_; lean_object* v___x_546_; lean_object* v___x_547_; lean_object* v___x_548_; lean_object* v___x_549_; lean_object* v___x_550_; lean_object* v___x_551_; lean_object* v___x_552_; lean_object* v___x_553_; lean_object* v___x_554_; lean_object* v___x_555_; lean_object* v___x_556_; lean_object* v___x_557_; lean_object* v___x_558_; lean_object* v___x_559_; lean_object* v___x_560_; lean_object* v___x_561_; lean_object* v___x_562_; lean_object* v___x_563_; lean_object* v___x_564_; lean_object* v___x_565_; lean_object* v___x_566_; lean_object* v___x_567_; lean_object* v___x_568_; lean_object* v___x_569_; lean_object* v___x_570_; lean_object* v___x_571_; lean_object* v___x_572_; lean_object* v___x_573_; lean_object* v___x_574_; lean_object* v___x_575_; lean_object* v___x_576_; lean_object* v___x_577_; 
v___x_278_ = lp_mathlib_Field_toSemifield___redArg(v_inst_272_);
v___x_279_ = lp_mathlib_Semifield_toDivisionSemiring___redArg(v___x_278_);
v_toSemiring_280_ = lean_ctor_get(v___x_279_, 0);
lean_inc_ref_n(v_toSemiring_280_, 2);
lean_dec_ref(v___x_279_);
v___x_281_ = lp_mathlib_instDistribOfSemiring___redArg(v_toSemiring_280_);
v_toMul_282_ = lean_ctor_get(v___x_281_, 0);
lean_inc_n(v_toMul_282_, 182);
v_toAdd_283_ = lean_ctor_get(v___x_281_, 1);
lean_inc_n(v_toAdd_283_, 42);
lean_dec_ref(v___x_281_);
v___x_284_ = lp_mathlib_Field_toDivisionRing___redArg(v_inst_272_);
v_toRing_285_ = lean_ctor_get(v___x_284_, 0);
lean_inc_ref_n(v_toRing_285_, 2);
lean_dec_ref(v___x_284_);
v___x_286_ = lp_mathlib_Ring_toAddGroupWithOne___redArg(v_toRing_285_);
v___x_287_ = lp_mathlib_AddGroupWithOne_toAddGroup___redArg(v___x_286_);
v_toSub_288_ = lean_ctor_get(v___x_287_, 2);
lean_inc_n(v_toSub_288_, 41);
lean_dec_ref(v___x_287_);
v___x_289_ = lp_mathlib_Ring_toAddCommGroup___redArg(v_toRing_285_);
lean_dec_ref(v_toRing_285_);
v___x_290_ = lp_mathlib_SubNegZeroMonoid_toNegZeroClass___redArg(v___x_289_);
lean_dec_ref(v___x_289_);
v_toAddMonoidWithOne_291_ = lean_ctor_get(v___x_286_, 1);
lean_inc_ref(v_toAddMonoidWithOne_291_);
lean_dec_ref(v___x_286_);
v_toMonoid_292_ = lean_ctor_get(v_toSemiring_280_, 1);
lean_inc_ref(v_toMonoid_292_);
lean_dec_ref(v_toSemiring_280_);
v_toNeg_293_ = lean_ctor_get(v___x_290_, 1);
lean_inc(v_toNeg_293_);
lean_dec_ref(v___x_290_);
v_toNatCast_294_ = lean_ctor_get(v_toAddMonoidWithOne_291_, 0);
lean_inc_n(v_toNatCast_294_, 3);
lean_dec_ref(v_toAddMonoidWithOne_291_);
v_npow_295_ = lean_ctor_get(v_toMonoid_292_, 2);
lean_inc_n(v_npow_295_, 10);
lean_dec_ref(v_toMonoid_292_);
v___x_296_ = lean_unsigned_to_nat(2u);
v___x_297_ = lean_apply_1(v_toNatCast_294_, v___x_296_);
lean_inc_n(v___x_297_, 3);
v___x_298_ = lean_apply_1(v_toNeg_293_, v___x_297_);
v___x_299_ = lean_unsigned_to_nat(3u);
lean_inc_n(v_a_273_, 8);
v___x_300_ = lean_apply_2(v_npow_295_, v___x_299_, v_a_273_);
lean_inc_n(v___x_300_, 4);
v___x_301_ = lean_apply_2(v_toMul_282_, v___x_298_, v___x_300_);
lean_inc_n(v_b_274_, 11);
v___x_302_ = lean_apply_2(v_npow_295_, v___x_296_, v_b_274_);
lean_inc_n(v___x_302_, 9);
v___x_303_ = lean_apply_2(v_toMul_282_, v___x_301_, v___x_302_);
lean_inc_n(v_tA_276_, 23);
v___x_304_ = lean_apply_2(v_npow_295_, v___x_296_, v_tA_276_);
lean_inc_n(v___x_304_, 20);
v___x_305_ = lean_apply_2(v_toMul_282_, v___x_303_, v___x_304_);
lean_inc_n(v_tB_277_, 32);
v___x_306_ = lean_apply_2(v_npow_295_, v___x_299_, v_tB_277_);
lean_inc_n(v___x_306_, 19);
v___x_307_ = lean_apply_2(v_toMul_282_, v___x_305_, v___x_306_);
v___x_308_ = lean_apply_1(v_toNatCast_294_, v___x_299_);
lean_inc_n(v___x_308_, 2);
v___x_309_ = lean_apply_2(v_toMul_282_, v___x_308_, v___x_300_);
lean_inc(v___x_309_);
v___x_310_ = lean_apply_2(v_toMul_282_, v___x_309_, v___x_302_);
lean_inc(v___x_310_);
v___x_311_ = lean_apply_2(v_toMul_282_, v___x_310_, v___x_304_);
v___x_312_ = lean_apply_2(v_npow_295_, v___x_296_, v_tB_277_);
lean_inc_n(v___x_312_, 29);
v___x_313_ = lean_apply_2(v_toMul_282_, v___x_311_, v___x_312_);
v___x_314_ = lean_apply_2(v_toAdd_283_, v___x_307_, v___x_313_);
v___x_315_ = lean_apply_2(v_toMul_282_, v___x_300_, v___x_302_);
lean_inc(v___x_315_);
v___x_316_ = lean_apply_2(v_toMul_282_, v___x_315_, v___x_304_);
v___x_317_ = lean_apply_2(v_toMul_282_, v___x_316_, v_tB_277_);
v___x_318_ = lean_apply_2(v_toSub_288_, v___x_314_, v___x_317_);
v___x_319_ = lean_apply_2(v_toMul_282_, v___x_297_, v___x_300_);
lean_inc(v___x_319_);
v___x_320_ = lean_apply_2(v_toMul_282_, v___x_319_, v___x_302_);
v___x_321_ = lean_apply_2(v_toMul_282_, v___x_320_, v_tA_276_);
v___x_322_ = lean_apply_2(v_toMul_282_, v___x_321_, v___x_306_);
v___x_323_ = lean_apply_2(v_toAdd_283_, v___x_318_, v___x_322_);
v___x_324_ = lean_apply_2(v_toMul_282_, v___x_310_, v_tA_276_);
v___x_325_ = lean_apply_2(v_toMul_282_, v___x_324_, v___x_312_);
v___x_326_ = lean_apply_2(v_toSub_288_, v___x_323_, v___x_325_);
v___x_327_ = lean_apply_2(v_toMul_282_, v___x_315_, v_tA_276_);
v___x_328_ = lean_apply_2(v_toMul_282_, v___x_327_, v_tB_277_);
v___x_329_ = lean_apply_2(v_toAdd_283_, v___x_326_, v___x_328_);
v___x_330_ = lean_apply_2(v_toMul_282_, v___x_319_, v_b_274_);
lean_inc(v___x_330_);
v___x_331_ = lean_apply_2(v_toMul_282_, v___x_330_, v___x_304_);
v___x_332_ = lean_apply_2(v_toMul_282_, v___x_331_, v___x_306_);
v___x_333_ = lean_apply_2(v_toAdd_283_, v___x_329_, v___x_332_);
v___x_334_ = lean_apply_2(v_toMul_282_, v___x_309_, v_b_274_);
lean_inc(v___x_334_);
v___x_335_ = lean_apply_2(v_toMul_282_, v___x_334_, v___x_304_);
v___x_336_ = lean_apply_2(v_toMul_282_, v___x_335_, v___x_312_);
v___x_337_ = lean_apply_2(v_toSub_288_, v___x_333_, v___x_336_);
v___x_338_ = lean_apply_2(v_toMul_282_, v___x_300_, v_b_274_);
lean_inc(v___x_338_);
v___x_339_ = lean_apply_2(v_toMul_282_, v___x_338_, v___x_304_);
v___x_340_ = lean_apply_2(v_toMul_282_, v___x_339_, v_tB_277_);
v___x_341_ = lean_apply_2(v_toAdd_283_, v___x_337_, v___x_340_);
v___x_342_ = lean_apply_2(v_toMul_282_, v___x_330_, v_tA_276_);
v___x_343_ = lean_apply_2(v_toMul_282_, v___x_342_, v___x_306_);
v___x_344_ = lean_apply_2(v_toSub_288_, v___x_341_, v___x_343_);
v___x_345_ = lean_apply_2(v_toMul_282_, v___x_334_, v_tA_276_);
v___x_346_ = lean_apply_2(v_toMul_282_, v___x_345_, v___x_312_);
v___x_347_ = lean_apply_2(v_toAdd_283_, v___x_344_, v___x_346_);
v___x_348_ = lean_apply_2(v_toMul_282_, v___x_338_, v_tA_276_);
v___x_349_ = lean_apply_2(v_toMul_282_, v___x_348_, v_tB_277_);
v___x_350_ = lean_apply_2(v_toSub_288_, v___x_347_, v___x_349_);
v___x_351_ = lean_apply_2(v_npow_295_, v___x_296_, v_a_273_);
lean_inc_n(v___x_351_, 5);
v___x_352_ = lean_apply_2(v_toMul_282_, v___x_297_, v___x_351_);
v___x_353_ = lean_apply_2(v_npow_295_, v___x_299_, v_b_274_);
lean_inc_n(v___x_353_, 5);
lean_inc(v___x_352_);
v___x_354_ = lean_apply_2(v_toMul_282_, v___x_352_, v___x_353_);
v___x_355_ = lean_apply_2(v_npow_295_, v___x_299_, v_tA_276_);
lean_inc_n(v___x_355_, 9);
v___x_356_ = lean_apply_2(v_toMul_282_, v___x_354_, v___x_355_);
lean_inc(v___x_356_);
v___x_357_ = lean_apply_2(v_toMul_282_, v___x_356_, v___x_312_);
v___x_358_ = lean_apply_2(v_toAdd_283_, v___x_350_, v___x_357_);
v___x_359_ = lean_apply_2(v_toMul_282_, v___x_356_, v_tB_277_);
v___x_360_ = lean_apply_2(v_toSub_288_, v___x_358_, v___x_359_);
v___x_361_ = lean_apply_2(v_toMul_282_, v___x_308_, v___x_351_);
lean_inc_n(v___x_361_, 2);
v___x_362_ = lean_apply_2(v_toMul_282_, v___x_361_, v___x_353_);
v___x_363_ = lean_apply_2(v_toMul_282_, v___x_362_, v___x_304_);
lean_inc(v___x_363_);
v___x_364_ = lean_apply_2(v_toMul_282_, v___x_363_, v___x_312_);
v___x_365_ = lean_apply_2(v_toSub_288_, v___x_360_, v___x_364_);
v___x_366_ = lean_apply_2(v_toMul_282_, v___x_363_, v_tB_277_);
v___x_367_ = lean_apply_2(v_toAdd_283_, v___x_365_, v___x_366_);
v___x_368_ = lean_apply_2(v_toMul_282_, v___x_351_, v___x_353_);
v___x_369_ = lean_apply_2(v_toMul_282_, v___x_368_, v_tA_276_);
lean_inc(v___x_369_);
v___x_370_ = lean_apply_2(v_toMul_282_, v___x_369_, v___x_312_);
v___x_371_ = lean_apply_2(v_toAdd_283_, v___x_367_, v___x_370_);
v___x_372_ = lean_apply_2(v_toMul_282_, v___x_369_, v_tB_277_);
v___x_373_ = lean_apply_2(v_toSub_288_, v___x_371_, v___x_372_);
v___x_374_ = lean_apply_2(v_toMul_282_, v___x_361_, v___x_302_);
lean_inc_n(v___x_374_, 2);
v___x_375_ = lean_apply_2(v_toMul_282_, v___x_374_, v___x_355_);
lean_inc(v___x_375_);
v___x_376_ = lean_apply_2(v_toMul_282_, v___x_375_, v___x_312_);
v___x_377_ = lean_apply_2(v_toSub_288_, v___x_373_, v___x_376_);
v___x_378_ = lean_apply_2(v_toMul_282_, v___x_375_, v_tB_277_);
v___x_379_ = lean_apply_2(v_toAdd_283_, v___x_377_, v___x_378_);
v___x_380_ = lean_apply_2(v_toMul_282_, v___x_374_, v___x_304_);
lean_inc(v___x_380_);
v___x_381_ = lean_apply_2(v_toMul_282_, v___x_380_, v___x_306_);
v___x_382_ = lean_apply_2(v_toAdd_283_, v___x_379_, v___x_381_);
v___x_383_ = lean_apply_2(v_toMul_282_, v___x_380_, v_tB_277_);
v___x_384_ = lean_apply_2(v_toSub_288_, v___x_382_, v___x_383_);
v___x_385_ = lean_apply_2(v_toMul_282_, v___x_374_, v_tA_276_);
lean_inc(v___x_385_);
v___x_386_ = lean_apply_2(v_toMul_282_, v___x_385_, v___x_306_);
v___x_387_ = lean_apply_2(v_toSub_288_, v___x_384_, v___x_386_);
v___x_388_ = lean_apply_2(v_toMul_282_, v___x_385_, v___x_312_);
v___x_389_ = lean_apply_2(v_toAdd_283_, v___x_387_, v___x_388_);
v___x_390_ = lean_apply_2(v_toMul_282_, v___x_352_, v_b_274_);
lean_inc_n(v_c_275_, 10);
v___x_391_ = lean_apply_2(v_toMul_282_, v___x_390_, v_c_275_);
lean_inc(v___x_391_);
v___x_392_ = lean_apply_2(v_toMul_282_, v___x_391_, v___x_304_);
v___x_393_ = lean_apply_2(v_toMul_282_, v___x_392_, v___x_306_);
v___x_394_ = lean_apply_2(v_toSub_288_, v___x_389_, v___x_393_);
v___x_395_ = lean_apply_2(v_toMul_282_, v___x_361_, v_b_274_);
lean_inc_n(v___x_395_, 2);
v___x_396_ = lean_apply_2(v_toMul_282_, v___x_395_, v_c_275_);
lean_inc(v___x_396_);
v___x_397_ = lean_apply_2(v_toMul_282_, v___x_396_, v___x_304_);
v___x_398_ = lean_apply_2(v_toMul_282_, v___x_397_, v___x_312_);
v___x_399_ = lean_apply_2(v_toAdd_283_, v___x_394_, v___x_398_);
v___x_400_ = lean_apply_2(v_toMul_282_, v___x_351_, v_b_274_);
lean_inc_n(v___x_400_, 2);
v___x_401_ = lean_apply_2(v_toMul_282_, v___x_400_, v_c_275_);
lean_inc(v___x_401_);
v___x_402_ = lean_apply_2(v_toMul_282_, v___x_401_, v___x_304_);
v___x_403_ = lean_apply_2(v_toMul_282_, v___x_402_, v_tB_277_);
v___x_404_ = lean_apply_2(v_toSub_288_, v___x_399_, v___x_403_);
v___x_405_ = lean_apply_2(v_toMul_282_, v___x_391_, v_tA_276_);
v___x_406_ = lean_apply_2(v_toMul_282_, v___x_405_, v___x_306_);
v___x_407_ = lean_apply_2(v_toAdd_283_, v___x_404_, v___x_406_);
v___x_408_ = lean_apply_2(v_toMul_282_, v___x_396_, v_tA_276_);
v___x_409_ = lean_apply_2(v_toMul_282_, v___x_408_, v___x_312_);
v___x_410_ = lean_apply_2(v_toSub_288_, v___x_407_, v___x_409_);
v___x_411_ = lean_apply_2(v_toMul_282_, v___x_401_, v_tA_276_);
v___x_412_ = lean_apply_2(v_toMul_282_, v___x_411_, v_tB_277_);
v___x_413_ = lean_apply_2(v_toAdd_283_, v___x_410_, v___x_412_);
v___x_414_ = lean_apply_2(v_toMul_282_, v___x_400_, v___x_355_);
lean_inc(v___x_414_);
v___x_415_ = lean_apply_2(v_toMul_282_, v___x_414_, v___x_312_);
v___x_416_ = lean_apply_2(v_toAdd_283_, v___x_413_, v___x_415_);
v___x_417_ = lean_apply_2(v_toMul_282_, v___x_414_, v_tB_277_);
v___x_418_ = lean_apply_2(v_toSub_288_, v___x_416_, v___x_417_);
v___x_419_ = lean_apply_2(v_toMul_282_, v___x_395_, v___x_304_);
lean_inc(v___x_419_);
v___x_420_ = lean_apply_2(v_toMul_282_, v___x_419_, v___x_306_);
v___x_421_ = lean_apply_2(v_toSub_288_, v___x_418_, v___x_420_);
v___x_422_ = lean_apply_2(v_toMul_282_, v___x_419_, v___x_312_);
v___x_423_ = lean_apply_2(v_toAdd_283_, v___x_421_, v___x_422_);
v___x_424_ = lean_apply_2(v_toMul_282_, v___x_395_, v_tA_276_);
v___x_425_ = lean_apply_2(v_toMul_282_, v___x_424_, v___x_306_);
v___x_426_ = lean_apply_2(v_toAdd_283_, v___x_423_, v___x_425_);
v___x_427_ = lean_unsigned_to_nat(4u);
v___x_428_ = lean_apply_1(v_toNatCast_294_, v___x_427_);
lean_inc(v___x_428_);
v___x_429_ = lean_apply_2(v_toMul_282_, v___x_428_, v___x_351_);
v___x_430_ = lean_apply_2(v_toMul_282_, v___x_429_, v_b_274_);
v___x_431_ = lean_apply_2(v_toMul_282_, v___x_430_, v_tA_276_);
v___x_432_ = lean_apply_2(v_toMul_282_, v___x_431_, v___x_312_);
v___x_433_ = lean_apply_2(v_toSub_288_, v___x_426_, v___x_432_);
v___x_434_ = lean_apply_2(v_toMul_282_, v___x_400_, v_tA_276_);
v___x_435_ = lean_apply_2(v_toMul_282_, v___x_434_, v_tB_277_);
v___x_436_ = lean_apply_2(v_toAdd_283_, v___x_433_, v___x_435_);
v___x_437_ = lean_apply_2(v_toMul_282_, v___x_351_, v_c_275_);
lean_inc(v___x_437_);
v___x_438_ = lean_apply_2(v_toMul_282_, v___x_437_, v___x_304_);
v___x_439_ = lean_apply_2(v_npow_295_, v___x_427_, v_tB_277_);
lean_inc_n(v___x_439_, 3);
lean_inc(v___x_438_);
v___x_440_ = lean_apply_2(v_toMul_282_, v___x_438_, v___x_439_);
v___x_441_ = lean_apply_2(v_toAdd_283_, v___x_436_, v___x_440_);
v___x_442_ = lean_apply_2(v_toMul_282_, v___x_438_, v___x_306_);
v___x_443_ = lean_apply_2(v_toSub_288_, v___x_441_, v___x_442_);
v___x_444_ = lean_apply_2(v_toMul_282_, v___x_437_, v_tA_276_);
lean_inc(v___x_444_);
v___x_445_ = lean_apply_2(v_toMul_282_, v___x_444_, v___x_439_);
v___x_446_ = lean_apply_2(v_toSub_288_, v___x_443_, v___x_445_);
v___x_447_ = lean_apply_2(v_toMul_282_, v___x_444_, v___x_306_);
v___x_448_ = lean_apply_2(v_toAdd_283_, v___x_446_, v___x_447_);
v___x_449_ = lean_apply_2(v_toMul_282_, v___x_297_, v_a_273_);
lean_inc_n(v___x_449_, 2);
v___x_450_ = lean_apply_2(v_toMul_282_, v___x_449_, v___x_353_);
v___x_451_ = lean_apply_2(v_toMul_282_, v___x_450_, v___x_355_);
lean_inc(v___x_451_);
v___x_452_ = lean_apply_2(v_toMul_282_, v___x_451_, v___x_312_);
v___x_453_ = lean_apply_2(v_toSub_288_, v___x_448_, v___x_452_);
v___x_454_ = lean_apply_2(v_toMul_282_, v___x_451_, v_tB_277_);
v___x_455_ = lean_apply_2(v_toAdd_283_, v___x_453_, v___x_454_);
v___x_456_ = lean_apply_2(v_toMul_282_, v___x_308_, v_a_273_);
lean_inc(v___x_456_);
v___x_457_ = lean_apply_2(v_toMul_282_, v___x_456_, v___x_353_);
v___x_458_ = lean_apply_2(v_toMul_282_, v___x_457_, v___x_304_);
lean_inc(v___x_458_);
v___x_459_ = lean_apply_2(v_toMul_282_, v___x_458_, v___x_312_);
v___x_460_ = lean_apply_2(v_toAdd_283_, v___x_455_, v___x_459_);
v___x_461_ = lean_apply_2(v_toMul_282_, v___x_458_, v_tB_277_);
v___x_462_ = lean_apply_2(v_toSub_288_, v___x_460_, v___x_461_);
v___x_463_ = lean_apply_2(v_toMul_282_, v_a_273_, v___x_353_);
v___x_464_ = lean_apply_2(v_toMul_282_, v___x_463_, v_tA_276_);
lean_inc(v___x_464_);
v___x_465_ = lean_apply_2(v_toMul_282_, v___x_464_, v___x_312_);
v___x_466_ = lean_apply_2(v_toSub_288_, v___x_462_, v___x_465_);
v___x_467_ = lean_apply_2(v_toMul_282_, v___x_464_, v_tB_277_);
v___x_468_ = lean_apply_2(v_toAdd_283_, v___x_466_, v___x_467_);
v___x_469_ = lean_apply_2(v_toMul_282_, v___x_449_, v___x_302_);
v___x_470_ = lean_apply_2(v_toMul_282_, v___x_469_, v_c_275_);
v___x_471_ = lean_apply_2(v_toMul_282_, v___x_470_, v___x_355_);
lean_inc(v___x_471_);
v___x_472_ = lean_apply_2(v_toMul_282_, v___x_471_, v___x_312_);
v___x_473_ = lean_apply_2(v_toAdd_283_, v___x_468_, v___x_472_);
v___x_474_ = lean_apply_2(v_toMul_282_, v___x_471_, v_tB_277_);
v___x_475_ = lean_apply_2(v_toSub_288_, v___x_473_, v___x_474_);
v___x_476_ = lean_apply_2(v_toMul_282_, v___x_456_, v___x_302_);
lean_inc_n(v___x_476_, 2);
v___x_477_ = lean_apply_2(v_toMul_282_, v___x_476_, v_c_275_);
v___x_478_ = lean_apply_2(v_toMul_282_, v___x_477_, v___x_304_);
lean_inc(v___x_478_);
v___x_479_ = lean_apply_2(v_toMul_282_, v___x_478_, v___x_312_);
v___x_480_ = lean_apply_2(v_toSub_288_, v___x_475_, v___x_479_);
v___x_481_ = lean_apply_2(v_toMul_282_, v___x_478_, v_tB_277_);
v___x_482_ = lean_apply_2(v_toAdd_283_, v___x_480_, v___x_481_);
v___x_483_ = lean_apply_2(v_toMul_282_, v_a_273_, v___x_302_);
lean_inc_n(v___x_483_, 2);
v___x_484_ = lean_apply_2(v_toMul_282_, v___x_483_, v_c_275_);
v___x_485_ = lean_apply_2(v_toMul_282_, v___x_484_, v_tA_276_);
lean_inc(v___x_485_);
v___x_486_ = lean_apply_2(v_toMul_282_, v___x_485_, v___x_312_);
v___x_487_ = lean_apply_2(v_toAdd_283_, v___x_482_, v___x_486_);
v___x_488_ = lean_apply_2(v_toMul_282_, v___x_485_, v_tB_277_);
v___x_489_ = lean_apply_2(v_toSub_288_, v___x_487_, v___x_488_);
v___x_490_ = lean_apply_2(v_toMul_282_, v___x_476_, v___x_355_);
lean_inc(v___x_490_);
v___x_491_ = lean_apply_2(v_toMul_282_, v___x_490_, v___x_312_);
v___x_492_ = lean_apply_2(v_toAdd_283_, v___x_489_, v___x_491_);
v___x_493_ = lean_apply_2(v_toMul_282_, v___x_490_, v_tB_277_);
v___x_494_ = lean_apply_2(v_toSub_288_, v___x_492_, v___x_493_);
v___x_495_ = lean_apply_2(v_toMul_282_, v___x_483_, v___x_304_);
v___x_496_ = lean_apply_2(v_toMul_282_, v___x_495_, v___x_306_);
v___x_497_ = lean_apply_2(v_toSub_288_, v___x_494_, v___x_496_);
v___x_498_ = lean_apply_2(v_toMul_282_, v___x_476_, v___x_304_);
v___x_499_ = lean_apply_2(v_toMul_282_, v___x_498_, v___x_312_);
v___x_500_ = lean_apply_2(v_toSub_288_, v___x_497_, v___x_499_);
v___x_501_ = lean_apply_2(v_toMul_282_, v___x_428_, v_a_273_);
v___x_502_ = lean_apply_2(v_toMul_282_, v___x_501_, v___x_302_);
v___x_503_ = lean_apply_2(v_toMul_282_, v___x_502_, v___x_304_);
v___x_504_ = lean_apply_2(v_toMul_282_, v___x_503_, v_tB_277_);
v___x_505_ = lean_apply_2(v_toAdd_283_, v___x_500_, v___x_504_);
v___x_506_ = lean_apply_2(v_toMul_282_, v___x_483_, v_tA_276_);
lean_inc(v___x_506_);
v___x_507_ = lean_apply_2(v_toMul_282_, v___x_506_, v___x_306_);
v___x_508_ = lean_apply_2(v_toAdd_283_, v___x_505_, v___x_507_);
v___x_509_ = lean_apply_2(v_toMul_282_, v___x_506_, v_tB_277_);
v___x_510_ = lean_apply_2(v_toSub_288_, v___x_508_, v___x_509_);
v___x_511_ = lean_apply_2(v_toMul_282_, v___x_449_, v_b_274_);
v___x_512_ = lean_apply_2(v_toMul_282_, v___x_511_, v_c_275_);
lean_inc_n(v___x_512_, 2);
v___x_513_ = lean_apply_2(v_toMul_282_, v___x_512_, v___x_355_);
lean_inc(v___x_513_);
v___x_514_ = lean_apply_2(v_toMul_282_, v___x_513_, v___x_312_);
v___x_515_ = lean_apply_2(v_toSub_288_, v___x_510_, v___x_514_);
v___x_516_ = lean_apply_2(v_toMul_282_, v___x_513_, v_tB_277_);
v___x_517_ = lean_apply_2(v_toAdd_283_, v___x_515_, v___x_516_);
v___x_518_ = lean_apply_2(v_toMul_282_, v___x_512_, v___x_304_);
lean_inc(v___x_518_);
v___x_519_ = lean_apply_2(v_toMul_282_, v___x_518_, v___x_306_);
v___x_520_ = lean_apply_2(v_toAdd_283_, v___x_517_, v___x_519_);
v___x_521_ = lean_apply_2(v_toMul_282_, v___x_518_, v_tB_277_);
v___x_522_ = lean_apply_2(v_toSub_288_, v___x_520_, v___x_521_);
v___x_523_ = lean_apply_2(v_toMul_282_, v___x_512_, v_tA_276_);
lean_inc(v___x_523_);
v___x_524_ = lean_apply_2(v_toMul_282_, v___x_523_, v___x_306_);
v___x_525_ = lean_apply_2(v_toSub_288_, v___x_522_, v___x_524_);
v___x_526_ = lean_apply_2(v_toMul_282_, v___x_523_, v___x_312_);
v___x_527_ = lean_apply_2(v_toAdd_283_, v___x_525_, v___x_526_);
v___x_528_ = lean_apply_2(v_toMul_282_, v_a_273_, v_b_274_);
lean_inc_n(v___x_528_, 2);
v___x_529_ = lean_apply_2(v_toMul_282_, v___x_528_, v___x_355_);
lean_inc(v___x_529_);
v___x_530_ = lean_apply_2(v_toMul_282_, v___x_529_, v___x_312_);
v___x_531_ = lean_apply_2(v_toSub_288_, v___x_527_, v___x_530_);
v___x_532_ = lean_apply_2(v_toMul_282_, v___x_529_, v_tB_277_);
v___x_533_ = lean_apply_2(v_toAdd_283_, v___x_531_, v___x_532_);
v___x_534_ = lean_apply_2(v_toMul_282_, v___x_528_, v___x_304_);
lean_inc(v___x_534_);
v___x_535_ = lean_apply_2(v_toMul_282_, v___x_534_, v___x_306_);
v___x_536_ = lean_apply_2(v_toAdd_283_, v___x_533_, v___x_535_);
v___x_537_ = lean_apply_2(v_toMul_282_, v___x_534_, v_tB_277_);
v___x_538_ = lean_apply_2(v_toSub_288_, v___x_536_, v___x_537_);
v___x_539_ = lean_apply_2(v_toMul_282_, v___x_528_, v_tA_276_);
lean_inc(v___x_539_);
v___x_540_ = lean_apply_2(v_toMul_282_, v___x_539_, v___x_306_);
v___x_541_ = lean_apply_2(v_toSub_288_, v___x_538_, v___x_540_);
v___x_542_ = lean_apply_2(v_toMul_282_, v___x_539_, v___x_312_);
v___x_543_ = lean_apply_2(v_toAdd_283_, v___x_541_, v___x_542_);
v___x_544_ = lean_apply_2(v_toMul_282_, v_a_273_, v_c_275_);
lean_inc(v___x_544_);
v___x_545_ = lean_apply_2(v_toMul_282_, v___x_544_, v___x_304_);
lean_inc(v___x_545_);
v___x_546_ = lean_apply_2(v_toMul_282_, v___x_545_, v___x_439_);
v___x_547_ = lean_apply_2(v_toSub_288_, v___x_543_, v___x_546_);
v___x_548_ = lean_apply_2(v_toMul_282_, v___x_545_, v___x_306_);
v___x_549_ = lean_apply_2(v_toAdd_283_, v___x_547_, v___x_548_);
v___x_550_ = lean_apply_2(v_toMul_282_, v___x_544_, v_tA_276_);
lean_inc(v___x_550_);
v___x_551_ = lean_apply_2(v_toMul_282_, v___x_550_, v___x_439_);
v___x_552_ = lean_apply_2(v_toAdd_283_, v___x_549_, v___x_551_);
v___x_553_ = lean_apply_2(v_toMul_282_, v___x_550_, v___x_306_);
v___x_554_ = lean_apply_2(v_toSub_288_, v___x_552_, v___x_553_);
v___x_555_ = lean_apply_2(v_toMul_282_, v___x_302_, v_c_275_);
v___x_556_ = lean_apply_2(v_npow_295_, v___x_427_, v_tA_276_);
lean_inc(v___x_556_);
lean_inc(v___x_555_);
v___x_557_ = lean_apply_2(v_toMul_282_, v___x_555_, v___x_556_);
lean_inc(v___x_557_);
v___x_558_ = lean_apply_2(v_toMul_282_, v___x_557_, v___x_312_);
v___x_559_ = lean_apply_2(v_toSub_288_, v___x_554_, v___x_558_);
v___x_560_ = lean_apply_2(v_toMul_282_, v___x_557_, v_tB_277_);
v___x_561_ = lean_apply_2(v_toAdd_283_, v___x_559_, v___x_560_);
v___x_562_ = lean_apply_2(v_toMul_282_, v___x_555_, v___x_355_);
lean_inc(v___x_562_);
v___x_563_ = lean_apply_2(v_toMul_282_, v___x_562_, v___x_312_);
v___x_564_ = lean_apply_2(v_toAdd_283_, v___x_561_, v___x_563_);
v___x_565_ = lean_apply_2(v_toMul_282_, v___x_562_, v_tB_277_);
v___x_566_ = lean_apply_2(v_toSub_288_, v___x_564_, v___x_565_);
v___x_567_ = lean_apply_2(v_toMul_282_, v_b_274_, v_c_275_);
lean_inc(v___x_567_);
v___x_568_ = lean_apply_2(v_toMul_282_, v___x_567_, v___x_556_);
lean_inc(v___x_568_);
v___x_569_ = lean_apply_2(v_toMul_282_, v___x_568_, v___x_312_);
v___x_570_ = lean_apply_2(v_toAdd_283_, v___x_566_, v___x_569_);
v___x_571_ = lean_apply_2(v_toMul_282_, v___x_568_, v_tB_277_);
v___x_572_ = lean_apply_2(v_toSub_288_, v___x_570_, v___x_571_);
v___x_573_ = lean_apply_2(v_toMul_282_, v___x_567_, v___x_355_);
lean_inc(v___x_573_);
v___x_574_ = lean_apply_2(v_toMul_282_, v___x_573_, v___x_312_);
v___x_575_ = lean_apply_2(v_toSub_288_, v___x_572_, v___x_574_);
v___x_576_ = lean_apply_2(v_toMul_282_, v___x_573_, v_tB_277_);
v___x_577_ = lean_apply_2(v_toAdd_283_, v___x_575_, v___x_576_);
return v___x_577_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_PaperB_gapQuot(lean_object* v_K_578_, lean_object* v_inst_579_, lean_object* v_a_580_, lean_object* v_b_581_, lean_object* v_c_582_, lean_object* v_tA_583_, lean_object* v_tB_584_){
_start:
{
lean_object* v___x_585_; 
v___x_585_ = lp_jeffreyorder_PaperB_gapQuot___redArg(v_inst_579_, v_a_580_, v_b_581_, v_c_582_, v_tA_583_, v_tB_584_);
return v___x_585_;
}
}
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_mathlib_Mathlib(uint8_t builtin);
static bool _G_initialized = false;
LEAN_EXPORT lean_object* initialize_jeffreyorder_AssocLocalityStructural(uint8_t builtin) {
lean_object * res;
if (_G_initialized) return lean_io_result_mk_ok(lean_box(0));
_G_initialized = true;
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_mathlib_Mathlib(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
return lean_io_result_mk_ok(lean_box(0));
}
#ifdef __cplusplus
}
#endif
