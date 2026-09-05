// Lean compiler output
// Module: JeffreyOrder.LemmaSEP
// Imports: public import Init public meta import Init public import Mathlib.Algebra.BigOperators.Fin public import Mathlib.Data.Real.Basic public import Mathlib.Tactic.FieldSimp public import Mathlib.Tactic.Ring
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
uint8_t lean_nat_dec_eq(lean_object*, lean_object*);
lean_object* lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_applyStep___redArg(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_applyStep(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_applyStep___boxed(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_applySteps(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder___private_JeffreyOrder_LemmaSEP_0__JeffreyOrder_applySteps_match__1_splitter___redArg(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder___private_JeffreyOrder_LemmaSEP_0__JeffreyOrder_applySteps_match__1_splitter(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder___private_JeffreyOrder_LemmaSEP_0__JeffreyOrder_applySteps_match__1_splitter___boxed(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___redArg(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___redArg___boxed(lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___boxed(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT uint8_t lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1___redArg(lean_object*, lean_object*, lean_object*, uint8_t, lean_object*, uint8_t, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1___redArg___boxed(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT uint8_t lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1(lean_object*, lean_object*, lean_object*, uint8_t, lean_object*, lean_object*, uint8_t, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1___boxed(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT uint8_t lp_jeffreyorder_JeffreyOrder_set2(lean_object*, lean_object*, lean_object*, lean_object*, uint8_t, uint8_t, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_set2___boxed(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_applyStep___redArg(lean_object* v_P_1_, lean_object* v_s_2_, lean_object* v_x_3_){
_start:
{
lean_object* v_attr_4_; lean_object* v_factor_5_; lean_object* v___x_6_; lean_object* v___x_7_; lean_object* v___x_8_; lean_object* v___f_9_; 
v_attr_4_ = lean_ctor_get(v_s_2_, 0);
lean_inc(v_attr_4_);
v_factor_5_ = lean_ctor_get(v_s_2_, 1);
lean_inc(v_factor_5_);
lean_dec_ref(v_s_2_);
lean_inc_ref(v_x_3_);
v___x_6_ = lean_apply_1(v_P_1_, v_x_3_);
v___x_7_ = lean_apply_1(v_x_3_, v_attr_4_);
v___x_8_ = lean_apply_1(v_factor_5_, v___x_7_);
v___f_9_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_9_, 0, v___x_6_);
lean_closure_set(v___f_9_, 1, v___x_8_);
return v___f_9_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_applyStep(lean_object* v_N_10_, lean_object* v_P_11_, lean_object* v_s_12_, lean_object* v_x_13_){
_start:
{
lean_object* v___x_14_; 
v___x_14_ = lp_jeffreyorder_JeffreyOrder_applyStep___redArg(v_P_11_, v_s_12_, v_x_13_);
return v___x_14_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_applyStep___boxed(lean_object* v_N_15_, lean_object* v_P_16_, lean_object* v_s_17_, lean_object* v_x_18_){
_start:
{
lean_object* v_res_19_; 
v_res_19_ = lp_jeffreyorder_JeffreyOrder_applyStep(v_N_15_, v_P_16_, v_s_17_, v_x_18_);
lean_dec(v_N_15_);
return v_res_19_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_applySteps(lean_object* v_N_20_, lean_object* v_P_21_, lean_object* v_x_22_, lean_object* v_a_23_){
_start:
{
if (lean_obj_tag(v_x_22_) == 0)
{
lean_object* v___x_24_; 
lean_dec(v_N_20_);
v___x_24_ = lean_apply_1(v_P_21_, v_a_23_);
return v___x_24_;
}
else
{
lean_object* v_head_25_; lean_object* v_tail_26_; lean_object* v___x_27_; 
v_head_25_ = lean_ctor_get(v_x_22_, 0);
lean_inc(v_head_25_);
v_tail_26_ = lean_ctor_get(v_x_22_, 1);
lean_inc(v_tail_26_);
lean_dec_ref_known(v_x_22_, 2);
lean_inc(v_N_20_);
v___x_27_ = lean_alloc_closure((void*)(lp_jeffreyorder_JeffreyOrder_applyStep___boxed), 4, 3);
lean_closure_set(v___x_27_, 0, v_N_20_);
lean_closure_set(v___x_27_, 1, v_P_21_);
lean_closure_set(v___x_27_, 2, v_head_25_);
v_P_21_ = v___x_27_;
v_x_22_ = v_tail_26_;
goto _start;
}
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder___private_JeffreyOrder_LemmaSEP_0__JeffreyOrder_applySteps_match__1_splitter___redArg(lean_object* v_x_29_, lean_object* v_h__1_30_, lean_object* v_h__2_31_){
_start:
{
if (lean_obj_tag(v_x_29_) == 0)
{
lean_object* v___x_32_; lean_object* v___x_33_; 
lean_dec(v_h__2_31_);
v___x_32_ = lean_box(0);
v___x_33_ = lean_apply_1(v_h__1_30_, v___x_32_);
return v___x_33_;
}
else
{
lean_object* v_head_34_; lean_object* v_tail_35_; lean_object* v___x_36_; 
lean_dec(v_h__1_30_);
v_head_34_ = lean_ctor_get(v_x_29_, 0);
lean_inc(v_head_34_);
v_tail_35_ = lean_ctor_get(v_x_29_, 1);
lean_inc(v_tail_35_);
lean_dec_ref_known(v_x_29_, 2);
v___x_36_ = lean_apply_2(v_h__2_31_, v_head_34_, v_tail_35_);
return v___x_36_;
}
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder___private_JeffreyOrder_LemmaSEP_0__JeffreyOrder_applySteps_match__1_splitter(lean_object* v_N_37_, lean_object* v_motive_38_, lean_object* v_x_39_, lean_object* v_h__1_40_, lean_object* v_h__2_41_){
_start:
{
if (lean_obj_tag(v_x_39_) == 0)
{
lean_object* v___x_42_; lean_object* v___x_43_; 
lean_dec(v_h__2_41_);
v___x_42_ = lean_box(0);
v___x_43_ = lean_apply_1(v_h__1_40_, v___x_42_);
return v___x_43_;
}
else
{
lean_object* v_head_44_; lean_object* v_tail_45_; lean_object* v___x_46_; 
lean_dec(v_h__1_40_);
v_head_44_ = lean_ctor_get(v_x_39_, 0);
lean_inc(v_head_44_);
v_tail_45_ = lean_ctor_get(v_x_39_, 1);
lean_inc(v_tail_45_);
lean_dec_ref_known(v_x_39_, 2);
v___x_46_ = lean_apply_2(v_h__2_41_, v_head_44_, v_tail_45_);
return v___x_46_;
}
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder___private_JeffreyOrder_LemmaSEP_0__JeffreyOrder_applySteps_match__1_splitter___boxed(lean_object* v_N_47_, lean_object* v_motive_48_, lean_object* v_x_49_, lean_object* v_h__1_50_, lean_object* v_h__2_51_){
_start:
{
lean_object* v_res_52_; 
v_res_52_ = lp_jeffreyorder___private_JeffreyOrder_LemmaSEP_0__JeffreyOrder_applySteps_match__1_splitter(v_N_47_, v_motive_48_, v_x_49_, v_h__1_50_, v_h__2_51_);
lean_dec(v_N_47_);
return v_res_52_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___redArg(lean_object* v_f_53_, lean_object* v_a_x27_54_, lean_object* v_v_55_, lean_object* v_a_56_){
_start:
{
uint8_t v___x_57_; 
v___x_57_ = lean_nat_dec_eq(v_a_56_, v_a_x27_54_);
if (v___x_57_ == 0)
{
lean_object* v___x_58_; 
v___x_58_ = lean_apply_1(v_f_53_, v_a_56_);
return v___x_58_;
}
else
{
lean_dec(v_a_56_);
lean_dec(v_f_53_);
lean_inc(v_v_55_);
return v_v_55_;
}
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___redArg___boxed(lean_object* v_f_59_, lean_object* v_a_x27_60_, lean_object* v_v_61_, lean_object* v_a_62_){
_start:
{
lean_object* v_res_63_; 
v_res_63_ = lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___redArg(v_f_59_, v_a_x27_60_, v_v_61_, v_a_62_);
lean_dec(v_v_61_);
lean_dec(v_a_x27_60_);
return v_res_63_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0(lean_object* v_N_64_, lean_object* v_00_u03b2_65_, lean_object* v_f_66_, lean_object* v_a_x27_67_, lean_object* v_v_68_, lean_object* v_a_69_){
_start:
{
lean_object* v___x_70_; 
v___x_70_ = lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___redArg(v_f_66_, v_a_x27_67_, v_v_68_, v_a_69_);
return v___x_70_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___boxed(lean_object* v_N_71_, lean_object* v_00_u03b2_72_, lean_object* v_f_73_, lean_object* v_a_x27_74_, lean_object* v_v_75_, lean_object* v_a_76_){
_start:
{
lean_object* v_res_77_; 
v_res_77_ = lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0(v_N_71_, v_00_u03b2_72_, v_f_73_, v_a_x27_74_, v_v_75_, v_a_76_);
lean_dec(v_v_75_);
lean_dec(v_a_x27_74_);
lean_dec(v_N_71_);
return v_res_77_;
}
}
LEAN_EXPORT uint8_t lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1___redArg(lean_object* v_N_78_, lean_object* v_x_79_, lean_object* v_i_80_, uint8_t v_b_81_, lean_object* v_a_x27_82_, uint8_t v_v_83_, lean_object* v_a_84_){
_start:
{
uint8_t v___x_85_; 
v___x_85_ = lean_nat_dec_eq(v_a_84_, v_a_x27_82_);
if (v___x_85_ == 0)
{
lean_object* v___x_86_; lean_object* v___x_87_; uint8_t v___x_88_; 
v___x_86_ = lean_box(v_b_81_);
v___x_87_ = lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___redArg(v_x_79_, v_i_80_, v___x_86_, v_a_84_);
lean_dec(v___x_86_);
v___x_88_ = lean_unbox(v___x_87_);
lean_dec(v___x_87_);
return v___x_88_;
}
else
{
lean_dec(v_a_84_);
lean_dec_ref(v_x_79_);
return v_v_83_;
}
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1___redArg___boxed(lean_object* v_N_89_, lean_object* v_x_90_, lean_object* v_i_91_, lean_object* v_b_92_, lean_object* v_a_x27_93_, lean_object* v_v_94_, lean_object* v_a_95_){
_start:
{
uint8_t v_b_boxed_96_; uint8_t v_v_boxed_97_; uint8_t v_res_98_; lean_object* v_r_99_; 
v_b_boxed_96_ = lean_unbox(v_b_92_);
v_v_boxed_97_ = lean_unbox(v_v_94_);
v_res_98_ = lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1___redArg(v_N_89_, v_x_90_, v_i_91_, v_b_boxed_96_, v_a_x27_93_, v_v_boxed_97_, v_a_95_);
lean_dec(v_a_x27_93_);
lean_dec(v_i_91_);
lean_dec(v_N_89_);
v_r_99_ = lean_box(v_res_98_);
return v_r_99_;
}
}
LEAN_EXPORT uint8_t lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1(lean_object* v_N_100_, lean_object* v_x_101_, lean_object* v_i_102_, uint8_t v_b_103_, lean_object* v_N_104_, lean_object* v_a_x27_105_, uint8_t v_v_106_, lean_object* v_a_107_){
_start:
{
uint8_t v___x_108_; 
v___x_108_ = lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1___redArg(v_N_100_, v_x_101_, v_i_102_, v_b_103_, v_a_x27_105_, v_v_106_, v_a_107_);
return v___x_108_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1___boxed(lean_object* v_N_109_, lean_object* v_x_110_, lean_object* v_i_111_, lean_object* v_b_112_, lean_object* v_N_113_, lean_object* v_a_x27_114_, lean_object* v_v_115_, lean_object* v_a_116_){
_start:
{
uint8_t v_b_boxed_117_; uint8_t v_v_boxed_118_; uint8_t v_res_119_; lean_object* v_r_120_; 
v_b_boxed_117_ = lean_unbox(v_b_112_);
v_v_boxed_118_ = lean_unbox(v_v_115_);
v_res_119_ = lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1(v_N_109_, v_x_110_, v_i_111_, v_b_boxed_117_, v_N_113_, v_a_x27_114_, v_v_boxed_118_, v_a_116_);
lean_dec(v_a_x27_114_);
lean_dec(v_N_113_);
lean_dec(v_i_111_);
lean_dec(v_N_109_);
v_r_120_ = lean_box(v_res_119_);
return v_r_120_;
}
}
LEAN_EXPORT uint8_t lp_jeffreyorder_JeffreyOrder_set2(lean_object* v_N_121_, lean_object* v_x_122_, lean_object* v_i_123_, lean_object* v_j_124_, uint8_t v_b_125_, uint8_t v_d_126_, lean_object* v_a_127_){
_start:
{
uint8_t v___x_128_; 
v___x_128_ = lp_jeffreyorder_Function_update___at___00JeffreyOrder_set2_spec__0___at___00JeffreyOrder_set2_spec__1___redArg(v_N_121_, v_x_122_, v_i_123_, v_b_125_, v_j_124_, v_d_126_, v_a_127_);
return v___x_128_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_set2___boxed(lean_object* v_N_129_, lean_object* v_x_130_, lean_object* v_i_131_, lean_object* v_j_132_, lean_object* v_b_133_, lean_object* v_d_134_, lean_object* v_a_135_){
_start:
{
uint8_t v_b_boxed_136_; uint8_t v_d_boxed_137_; uint8_t v_res_138_; lean_object* v_r_139_; 
v_b_boxed_136_ = lean_unbox(v_b_133_);
v_d_boxed_137_ = lean_unbox(v_d_134_);
v_res_138_ = lp_jeffreyorder_JeffreyOrder_set2(v_N_129_, v_x_130_, v_i_131_, v_j_132_, v_b_boxed_136_, v_d_boxed_137_, v_a_135_);
lean_dec(v_j_132_);
lean_dec(v_i_131_);
lean_dec(v_N_129_);
v_r_139_ = lean_box(v_res_138_);
return v_r_139_;
}
}
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_mathlib_Mathlib_Algebra_BigOperators_Fin(uint8_t builtin);
lean_object* initialize_mathlib_Mathlib_Data_Real_Basic(uint8_t builtin);
lean_object* initialize_mathlib_Mathlib_Tactic_FieldSimp(uint8_t builtin);
lean_object* initialize_mathlib_Mathlib_Tactic_Ring(uint8_t builtin);
static bool _G_initialized = false;
LEAN_EXPORT lean_object* initialize_jeffreyorder_JeffreyOrder_LemmaSEP(uint8_t builtin) {
lean_object * res;
if (_G_initialized) return lean_io_result_mk_ok(lean_box(0));
_G_initialized = true;
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_mathlib_Mathlib_Algebra_BigOperators_Fin(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_mathlib_Mathlib_Data_Real_Basic(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_mathlib_Mathlib_Tactic_FieldSimp(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_mathlib_Mathlib_Tactic_Ring(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
return lean_io_result_mk_ok(lean_box(0));
}
#ifdef __cplusplus
}
#endif
