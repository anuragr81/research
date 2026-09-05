// Lean compiler output
// Module: JeffreyOrder.Basic
// Imports: public import Init public meta import Init public import Mathlib.Data.Real.Basic public import Mathlib.Tactic.FieldSimp public import Mathlib.Tactic.Ring public import Mathlib.Tactic.Linarith public import Mathlib.Tactic.LinearCombination
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
lean_object* lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_(lean_object*, lean_object*, lean_object*);
lean_object* lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_(lean_object*, lean_object*, lean_object*);
lean_object* lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_(lean_object*, lean_object*);
extern lean_object* lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_total(lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_mA0(lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_mA1(lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_mB0(lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_mB1(lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_assoc(lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_inner(lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_smul(lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_add(lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_sub(lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_prior(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_indep(lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_meanBelief(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_total(lean_object* v_Q_1_){
_start:
{
lean_object* v_a00_2_; lean_object* v_a01_3_; lean_object* v_a10_4_; lean_object* v_a11_5_; lean_object* v___f_6_; lean_object* v___f_7_; lean_object* v___f_8_; 
v_a00_2_ = lean_ctor_get(v_Q_1_, 0);
lean_inc(v_a00_2_);
v_a01_3_ = lean_ctor_get(v_Q_1_, 1);
lean_inc(v_a01_3_);
v_a10_4_ = lean_ctor_get(v_Q_1_, 2);
lean_inc(v_a10_4_);
v_a11_5_ = lean_ctor_get(v_Q_1_, 3);
lean_inc(v_a11_5_);
lean_dec_ref(v_Q_1_);
v___f_6_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_6_, 0, v_a00_2_);
lean_closure_set(v___f_6_, 1, v_a01_3_);
v___f_7_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_7_, 0, v___f_6_);
lean_closure_set(v___f_7_, 1, v_a10_4_);
v___f_8_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_8_, 0, v___f_7_);
lean_closure_set(v___f_8_, 1, v_a11_5_);
return v___f_8_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_mA0(lean_object* v_Q_9_){
_start:
{
lean_object* v_a00_10_; lean_object* v_a01_11_; lean_object* v___f_12_; 
v_a00_10_ = lean_ctor_get(v_Q_9_, 0);
lean_inc(v_a00_10_);
v_a01_11_ = lean_ctor_get(v_Q_9_, 1);
lean_inc(v_a01_11_);
lean_dec_ref(v_Q_9_);
v___f_12_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_12_, 0, v_a00_10_);
lean_closure_set(v___f_12_, 1, v_a01_11_);
return v___f_12_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_mA1(lean_object* v_Q_13_){
_start:
{
lean_object* v_a10_14_; lean_object* v_a11_15_; lean_object* v___f_16_; 
v_a10_14_ = lean_ctor_get(v_Q_13_, 2);
lean_inc(v_a10_14_);
v_a11_15_ = lean_ctor_get(v_Q_13_, 3);
lean_inc(v_a11_15_);
lean_dec_ref(v_Q_13_);
v___f_16_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_16_, 0, v_a10_14_);
lean_closure_set(v___f_16_, 1, v_a11_15_);
return v___f_16_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_mB0(lean_object* v_Q_17_){
_start:
{
lean_object* v_a00_18_; lean_object* v_a10_19_; lean_object* v___f_20_; 
v_a00_18_ = lean_ctor_get(v_Q_17_, 0);
lean_inc(v_a00_18_);
v_a10_19_ = lean_ctor_get(v_Q_17_, 2);
lean_inc(v_a10_19_);
lean_dec_ref(v_Q_17_);
v___f_20_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_20_, 0, v_a00_18_);
lean_closure_set(v___f_20_, 1, v_a10_19_);
return v___f_20_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_mB1(lean_object* v_Q_21_){
_start:
{
lean_object* v_a01_22_; lean_object* v_a11_23_; lean_object* v___f_24_; 
v_a01_22_ = lean_ctor_get(v_Q_21_, 1);
lean_inc(v_a01_22_);
v_a11_23_ = lean_ctor_get(v_Q_21_, 3);
lean_inc(v_a11_23_);
lean_dec_ref(v_Q_21_);
v___f_24_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_24_, 0, v_a01_22_);
lean_closure_set(v___f_24_, 1, v_a11_23_);
return v___f_24_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_assoc(lean_object* v_Q_25_){
_start:
{
lean_object* v_a00_26_; lean_object* v_a01_27_; lean_object* v_a10_28_; lean_object* v_a11_29_; lean_object* v___f_30_; lean_object* v___f_31_; lean_object* v___f_32_; lean_object* v___f_33_; 
v_a00_26_ = lean_ctor_get(v_Q_25_, 0);
lean_inc(v_a00_26_);
v_a01_27_ = lean_ctor_get(v_Q_25_, 1);
lean_inc(v_a01_27_);
v_a10_28_ = lean_ctor_get(v_Q_25_, 2);
lean_inc(v_a10_28_);
v_a11_29_ = lean_ctor_get(v_Q_25_, 3);
lean_inc(v_a11_29_);
lean_dec_ref(v_Q_25_);
v___f_30_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_30_, 0, v_a00_26_);
lean_closure_set(v___f_30_, 1, v_a11_29_);
v___f_31_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_31_, 0, v_a01_27_);
lean_closure_set(v___f_31_, 1, v_a10_28_);
v___f_32_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_32_, 0, v___f_31_);
v___f_33_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_33_, 0, v___f_30_);
lean_closure_set(v___f_33_, 1, v___f_32_);
return v___f_33_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_inner(lean_object* v_X_34_, lean_object* v_Y_35_){
_start:
{
lean_object* v_a00_36_; lean_object* v_a01_37_; lean_object* v_a10_38_; lean_object* v_a11_39_; lean_object* v_a00_40_; lean_object* v_a01_41_; lean_object* v_a10_42_; lean_object* v_a11_43_; lean_object* v___f_44_; lean_object* v___f_45_; lean_object* v___f_46_; lean_object* v___f_47_; lean_object* v___f_48_; lean_object* v___f_49_; lean_object* v___f_50_; 
v_a00_36_ = lean_ctor_get(v_X_34_, 0);
lean_inc(v_a00_36_);
v_a01_37_ = lean_ctor_get(v_X_34_, 1);
lean_inc(v_a01_37_);
v_a10_38_ = lean_ctor_get(v_X_34_, 2);
lean_inc(v_a10_38_);
v_a11_39_ = lean_ctor_get(v_X_34_, 3);
lean_inc(v_a11_39_);
lean_dec_ref(v_X_34_);
v_a00_40_ = lean_ctor_get(v_Y_35_, 0);
lean_inc(v_a00_40_);
v_a01_41_ = lean_ctor_get(v_Y_35_, 1);
lean_inc(v_a01_41_);
v_a10_42_ = lean_ctor_get(v_Y_35_, 2);
lean_inc(v_a10_42_);
v_a11_43_ = lean_ctor_get(v_Y_35_, 3);
lean_inc(v_a11_43_);
lean_dec_ref(v_Y_35_);
v___f_44_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_44_, 0, v_a00_36_);
lean_closure_set(v___f_44_, 1, v_a00_40_);
v___f_45_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_45_, 0, v_a01_37_);
lean_closure_set(v___f_45_, 1, v_a01_41_);
v___f_46_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_46_, 0, v___f_44_);
lean_closure_set(v___f_46_, 1, v___f_45_);
v___f_47_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_47_, 0, v_a10_38_);
lean_closure_set(v___f_47_, 1, v_a10_42_);
v___f_48_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_48_, 0, v___f_46_);
lean_closure_set(v___f_48_, 1, v___f_47_);
v___f_49_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_49_, 0, v_a11_39_);
lean_closure_set(v___f_49_, 1, v_a11_43_);
v___f_50_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_50_, 0, v___f_48_);
lean_closure_set(v___f_50_, 1, v___f_49_);
return v___f_50_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_smul(lean_object* v_k_51_, lean_object* v_X_52_){
_start:
{
lean_object* v_a00_53_; lean_object* v_a01_54_; lean_object* v_a10_55_; lean_object* v_a11_56_; lean_object* v___x_58_; uint8_t v_isShared_59_; uint8_t v_isSharedCheck_67_; 
v_a00_53_ = lean_ctor_get(v_X_52_, 0);
v_a01_54_ = lean_ctor_get(v_X_52_, 1);
v_a10_55_ = lean_ctor_get(v_X_52_, 2);
v_a11_56_ = lean_ctor_get(v_X_52_, 3);
v_isSharedCheck_67_ = !lean_is_exclusive(v_X_52_);
if (v_isSharedCheck_67_ == 0)
{
v___x_58_ = v_X_52_;
v_isShared_59_ = v_isSharedCheck_67_;
goto v_resetjp_57_;
}
else
{
lean_inc(v_a11_56_);
lean_inc(v_a10_55_);
lean_inc(v_a01_54_);
lean_inc(v_a00_53_);
lean_dec(v_X_52_);
v___x_58_ = lean_box(0);
v_isShared_59_ = v_isSharedCheck_67_;
goto v_resetjp_57_;
}
v_resetjp_57_:
{
lean_object* v___f_60_; lean_object* v___f_61_; lean_object* v___f_62_; lean_object* v___f_63_; lean_object* v___x_65_; 
lean_inc_n(v_k_51_, 3);
v___f_60_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_60_, 0, v_k_51_);
lean_closure_set(v___f_60_, 1, v_a00_53_);
v___f_61_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_61_, 0, v_k_51_);
lean_closure_set(v___f_61_, 1, v_a01_54_);
v___f_62_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_62_, 0, v_k_51_);
lean_closure_set(v___f_62_, 1, v_a10_55_);
v___f_63_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_63_, 0, v_k_51_);
lean_closure_set(v___f_63_, 1, v_a11_56_);
if (v_isShared_59_ == 0)
{
lean_ctor_set(v___x_58_, 3, v___f_63_);
lean_ctor_set(v___x_58_, 2, v___f_62_);
lean_ctor_set(v___x_58_, 1, v___f_61_);
lean_ctor_set(v___x_58_, 0, v___f_60_);
v___x_65_ = v___x_58_;
goto v_reusejp_64_;
}
else
{
lean_object* v_reuseFailAlloc_66_; 
v_reuseFailAlloc_66_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v_reuseFailAlloc_66_, 0, v___f_60_);
lean_ctor_set(v_reuseFailAlloc_66_, 1, v___f_61_);
lean_ctor_set(v_reuseFailAlloc_66_, 2, v___f_62_);
lean_ctor_set(v_reuseFailAlloc_66_, 3, v___f_63_);
v___x_65_ = v_reuseFailAlloc_66_;
goto v_reusejp_64_;
}
v_reusejp_64_:
{
return v___x_65_;
}
}
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_add(lean_object* v_X_68_, lean_object* v_Y_69_){
_start:
{
lean_object* v_a00_70_; lean_object* v_a01_71_; lean_object* v_a10_72_; lean_object* v_a11_73_; lean_object* v_a00_74_; lean_object* v_a01_75_; lean_object* v_a10_76_; lean_object* v_a11_77_; lean_object* v___x_79_; uint8_t v_isShared_80_; uint8_t v_isSharedCheck_88_; 
v_a00_70_ = lean_ctor_get(v_X_68_, 0);
lean_inc(v_a00_70_);
v_a01_71_ = lean_ctor_get(v_X_68_, 1);
lean_inc(v_a01_71_);
v_a10_72_ = lean_ctor_get(v_X_68_, 2);
lean_inc(v_a10_72_);
v_a11_73_ = lean_ctor_get(v_X_68_, 3);
lean_inc(v_a11_73_);
lean_dec_ref(v_X_68_);
v_a00_74_ = lean_ctor_get(v_Y_69_, 0);
v_a01_75_ = lean_ctor_get(v_Y_69_, 1);
v_a10_76_ = lean_ctor_get(v_Y_69_, 2);
v_a11_77_ = lean_ctor_get(v_Y_69_, 3);
v_isSharedCheck_88_ = !lean_is_exclusive(v_Y_69_);
if (v_isSharedCheck_88_ == 0)
{
v___x_79_ = v_Y_69_;
v_isShared_80_ = v_isSharedCheck_88_;
goto v_resetjp_78_;
}
else
{
lean_inc(v_a11_77_);
lean_inc(v_a10_76_);
lean_inc(v_a01_75_);
lean_inc(v_a00_74_);
lean_dec(v_Y_69_);
v___x_79_ = lean_box(0);
v_isShared_80_ = v_isSharedCheck_88_;
goto v_resetjp_78_;
}
v_resetjp_78_:
{
lean_object* v___f_81_; lean_object* v___f_82_; lean_object* v___f_83_; lean_object* v___f_84_; lean_object* v___x_86_; 
v___f_81_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_81_, 0, v_a00_70_);
lean_closure_set(v___f_81_, 1, v_a00_74_);
v___f_82_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_82_, 0, v_a01_71_);
lean_closure_set(v___f_82_, 1, v_a01_75_);
v___f_83_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_83_, 0, v_a10_72_);
lean_closure_set(v___f_83_, 1, v_a10_76_);
v___f_84_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_84_, 0, v_a11_73_);
lean_closure_set(v___f_84_, 1, v_a11_77_);
if (v_isShared_80_ == 0)
{
lean_ctor_set(v___x_79_, 3, v___f_84_);
lean_ctor_set(v___x_79_, 2, v___f_83_);
lean_ctor_set(v___x_79_, 1, v___f_82_);
lean_ctor_set(v___x_79_, 0, v___f_81_);
v___x_86_ = v___x_79_;
goto v_reusejp_85_;
}
else
{
lean_object* v_reuseFailAlloc_87_; 
v_reuseFailAlloc_87_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v_reuseFailAlloc_87_, 0, v___f_81_);
lean_ctor_set(v_reuseFailAlloc_87_, 1, v___f_82_);
lean_ctor_set(v_reuseFailAlloc_87_, 2, v___f_83_);
lean_ctor_set(v_reuseFailAlloc_87_, 3, v___f_84_);
v___x_86_ = v_reuseFailAlloc_87_;
goto v_reusejp_85_;
}
v_reusejp_85_:
{
return v___x_86_;
}
}
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Mat_sub(lean_object* v_X_89_, lean_object* v_Y_90_){
_start:
{
lean_object* v_a00_91_; lean_object* v_a01_92_; lean_object* v_a10_93_; lean_object* v_a11_94_; lean_object* v_a00_95_; lean_object* v_a01_96_; lean_object* v_a10_97_; lean_object* v_a11_98_; lean_object* v___x_100_; uint8_t v_isShared_101_; uint8_t v_isSharedCheck_113_; 
v_a00_91_ = lean_ctor_get(v_X_89_, 0);
lean_inc(v_a00_91_);
v_a01_92_ = lean_ctor_get(v_X_89_, 1);
lean_inc(v_a01_92_);
v_a10_93_ = lean_ctor_get(v_X_89_, 2);
lean_inc(v_a10_93_);
v_a11_94_ = lean_ctor_get(v_X_89_, 3);
lean_inc(v_a11_94_);
lean_dec_ref(v_X_89_);
v_a00_95_ = lean_ctor_get(v_Y_90_, 0);
v_a01_96_ = lean_ctor_get(v_Y_90_, 1);
v_a10_97_ = lean_ctor_get(v_Y_90_, 2);
v_a11_98_ = lean_ctor_get(v_Y_90_, 3);
v_isSharedCheck_113_ = !lean_is_exclusive(v_Y_90_);
if (v_isSharedCheck_113_ == 0)
{
v___x_100_ = v_Y_90_;
v_isShared_101_ = v_isSharedCheck_113_;
goto v_resetjp_99_;
}
else
{
lean_inc(v_a11_98_);
lean_inc(v_a10_97_);
lean_inc(v_a01_96_);
lean_inc(v_a00_95_);
lean_dec(v_Y_90_);
v___x_100_ = lean_box(0);
v_isShared_101_ = v_isSharedCheck_113_;
goto v_resetjp_99_;
}
v_resetjp_99_:
{
lean_object* v___f_102_; lean_object* v___f_103_; lean_object* v___f_104_; lean_object* v___f_105_; lean_object* v___f_106_; lean_object* v___f_107_; lean_object* v___f_108_; lean_object* v___f_109_; lean_object* v___x_111_; 
v___f_102_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_102_, 0, v_a00_95_);
v___f_103_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_103_, 0, v_a00_91_);
lean_closure_set(v___f_103_, 1, v___f_102_);
v___f_104_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_104_, 0, v_a01_96_);
v___f_105_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_105_, 0, v_a01_92_);
lean_closure_set(v___f_105_, 1, v___f_104_);
v___f_106_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_106_, 0, v_a10_97_);
v___f_107_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_107_, 0, v_a10_93_);
lean_closure_set(v___f_107_, 1, v___f_106_);
v___f_108_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_108_, 0, v_a11_98_);
v___f_109_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_109_, 0, v_a11_94_);
lean_closure_set(v___f_109_, 1, v___f_108_);
if (v_isShared_101_ == 0)
{
lean_ctor_set(v___x_100_, 3, v___f_109_);
lean_ctor_set(v___x_100_, 2, v___f_107_);
lean_ctor_set(v___x_100_, 1, v___f_105_);
lean_ctor_set(v___x_100_, 0, v___f_103_);
v___x_111_ = v___x_100_;
goto v_reusejp_110_;
}
else
{
lean_object* v_reuseFailAlloc_112_; 
v_reuseFailAlloc_112_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v_reuseFailAlloc_112_, 0, v___f_103_);
lean_ctor_set(v_reuseFailAlloc_112_, 1, v___f_105_);
lean_ctor_set(v_reuseFailAlloc_112_, 2, v___f_107_);
lean_ctor_set(v_reuseFailAlloc_112_, 3, v___f_109_);
v___x_111_ = v_reuseFailAlloc_112_;
goto v_reusejp_110_;
}
v_reusejp_110_:
{
return v___x_111_;
}
}
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_prior(lean_object* v_00_u03b1_114_, lean_object* v_00_u03b2_115_, lean_object* v_c_116_){
_start:
{
lean_object* v___f_117_; lean_object* v___f_118_; lean_object* v___x_119_; lean_object* v___f_120_; lean_object* v___f_121_; lean_object* v___f_122_; lean_object* v___f_123_; lean_object* v___f_124_; lean_object* v___f_125_; lean_object* v___f_126_; lean_object* v___f_127_; lean_object* v___f_128_; lean_object* v___f_129_; lean_object* v___f_130_; lean_object* v___x_131_; 
lean_inc_n(v_00_u03b2_115_, 2);
lean_inc_n(v_00_u03b1_114_, 2);
v___f_117_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_117_, 0, v_00_u03b1_114_);
lean_closure_set(v___f_117_, 1, v_00_u03b2_115_);
lean_inc_n(v_c_116_, 2);
v___f_118_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_118_, 0, v___f_117_);
lean_closure_set(v___f_118_, 1, v_c_116_);
v___x_119_ = lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
v___f_120_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_120_, 0, v_00_u03b2_115_);
v___f_121_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_121_, 0, v___x_119_);
lean_closure_set(v___f_121_, 1, v___f_120_);
lean_inc_ref(v___f_121_);
v___f_122_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_122_, 0, v_00_u03b1_114_);
lean_closure_set(v___f_122_, 1, v___f_121_);
v___f_123_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_123_, 0, v_c_116_);
lean_inc_ref(v___f_123_);
v___f_124_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_124_, 0, v___f_122_);
lean_closure_set(v___f_124_, 1, v___f_123_);
v___f_125_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_125_, 0, v_00_u03b1_114_);
v___f_126_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_126_, 0, v___x_119_);
lean_closure_set(v___f_126_, 1, v___f_125_);
lean_inc_ref(v___f_126_);
v___f_127_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_127_, 0, v___f_126_);
lean_closure_set(v___f_127_, 1, v_00_u03b2_115_);
v___f_128_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_128_, 0, v___f_127_);
lean_closure_set(v___f_128_, 1, v___f_123_);
v___f_129_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_129_, 0, v___f_126_);
lean_closure_set(v___f_129_, 1, v___f_121_);
v___f_130_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_130_, 0, v___f_129_);
lean_closure_set(v___f_130_, 1, v_c_116_);
v___x_131_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v___x_131_, 0, v___f_118_);
lean_ctor_set(v___x_131_, 1, v___f_124_);
lean_ctor_set(v___x_131_, 2, v___f_128_);
lean_ctor_set(v___x_131_, 3, v___f_130_);
return v___x_131_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_indep(lean_object* v_q_u2080_132_, lean_object* v_r_u2080_133_){
_start:
{
lean_object* v___f_134_; lean_object* v___x_135_; lean_object* v___f_136_; lean_object* v___f_137_; lean_object* v___f_138_; lean_object* v___f_139_; lean_object* v___f_140_; lean_object* v___f_141_; lean_object* v___f_142_; lean_object* v___x_143_; 
lean_inc_n(v_r_u2080_133_, 2);
lean_inc_n(v_q_u2080_132_, 2);
v___f_134_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_134_, 0, v_q_u2080_132_);
lean_closure_set(v___f_134_, 1, v_r_u2080_133_);
v___x_135_ = lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
v___f_136_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_136_, 0, v_r_u2080_133_);
v___f_137_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_137_, 0, v___x_135_);
lean_closure_set(v___f_137_, 1, v___f_136_);
lean_inc_ref(v___f_137_);
v___f_138_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_138_, 0, v_q_u2080_132_);
lean_closure_set(v___f_138_, 1, v___f_137_);
v___f_139_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_139_, 0, v_q_u2080_132_);
v___f_140_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_140_, 0, v___x_135_);
lean_closure_set(v___f_140_, 1, v___f_139_);
lean_inc_ref(v___f_140_);
v___f_141_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_141_, 0, v___f_140_);
lean_closure_set(v___f_141_, 1, v_r_u2080_133_);
v___f_142_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_142_, 0, v___f_140_);
lean_closure_set(v___f_142_, 1, v___f_137_);
v___x_143_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v___x_143_, 0, v___f_134_);
lean_ctor_set(v___x_143_, 1, v___f_138_);
lean_ctor_set(v___x_143_, 2, v___f_141_);
lean_ctor_set(v___x_143_, 3, v___f_142_);
return v___x_143_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_meanBelief(lean_object* v_lam_144_, lean_object* v_X_145_, lean_object* v_Y_146_){
_start:
{
lean_object* v___x_147_; lean_object* v___x_148_; lean_object* v___f_149_; lean_object* v___f_150_; lean_object* v___x_151_; lean_object* v___x_152_; 
lean_inc(v_lam_144_);
v___x_147_ = lp_jeffreyorder_JeffreyOrder_Mat_smul(v_lam_144_, v_X_145_);
v___x_148_ = lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
v___f_149_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_149_, 0, v_lam_144_);
v___f_150_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_150_, 0, v___x_148_);
lean_closure_set(v___f_150_, 1, v___f_149_);
v___x_151_ = lp_jeffreyorder_JeffreyOrder_Mat_smul(v___f_150_, v_Y_146_);
v___x_152_ = lp_jeffreyorder_JeffreyOrder_Mat_add(v___x_147_, v___x_151_);
return v___x_152_;
}
}
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_mathlib_Mathlib_Data_Real_Basic(uint8_t builtin);
lean_object* initialize_mathlib_Mathlib_Tactic_FieldSimp(uint8_t builtin);
lean_object* initialize_mathlib_Mathlib_Tactic_Ring(uint8_t builtin);
lean_object* initialize_mathlib_Mathlib_Tactic_Linarith(uint8_t builtin);
lean_object* initialize_mathlib_Mathlib_Tactic_LinearCombination(uint8_t builtin);
static bool _G_initialized = false;
LEAN_EXPORT lean_object* initialize_jeffreyorder_JeffreyOrder_Basic(uint8_t builtin) {
lean_object * res;
if (_G_initialized) return lean_io_result_mk_ok(lean_box(0));
_G_initialized = true;
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_Init(builtin);
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
res = initialize_mathlib_Mathlib_Tactic_Linarith(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_mathlib_Mathlib_Tactic_LinearCombination(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
return lean_io_result_mk_ok(lean_box(0));
}
#ifdef __cplusplus
}
#endif
