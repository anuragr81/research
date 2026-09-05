// Lean compiler output
// Module: JeffreyOrder.PropPRO
// Imports: public import Init public meta import Init public import JeffreyOrder.PropDEC
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
extern lean_object* lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
lean_object* lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_(lean_object*, lean_object*);
lean_object* lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_(lean_object*, lean_object*, lean_object*);
extern lean_object* lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1850581184____hygCtx___hyg_8_;
lean_object* lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_R1(lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_R2(lean_object*);
static lean_once_cell_t lp_jeffreyorder_JeffreyOrder_Jmat___closed__0_once = LEAN_ONCE_CELL_INITIALIZER;
static lean_object* lp_jeffreyorder_JeffreyOrder_Jmat___closed__0;
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Jmat;
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_gradAssoc(lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Zpar(lean_object*, lean_object*);
static lean_once_cell_t lp_jeffreyorder_JeffreyOrder_margA___closed__0_once = LEAN_ONCE_CELL_INITIALIZER;
static lean_object* lp_jeffreyorder_JeffreyOrder_margA___closed__0;
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_margA;
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_R1(lean_object* v_q_u2080_1_){
_start:
{
lean_object* v___f_2_; lean_object* v___x_3_; lean_object* v___f_4_; lean_object* v___f_5_; lean_object* v___x_6_; 
lean_inc(v_q_u2080_1_);
v___f_2_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_2_, 0, v_q_u2080_1_);
v___x_3_ = lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
lean_inc_ref(v___f_2_);
v___f_4_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_4_, 0, v___x_3_);
lean_closure_set(v___f_4_, 1, v___f_2_);
lean_inc_ref(v___f_4_);
v___f_5_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_5_, 0, v___f_4_);
v___x_6_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v___x_6_, 0, v_q_u2080_1_);
lean_ctor_set(v___x_6_, 1, v___f_2_);
lean_ctor_set(v___x_6_, 2, v___f_4_);
lean_ctor_set(v___x_6_, 3, v___f_5_);
return v___x_6_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_R2(lean_object* v_r_u2080_7_){
_start:
{
lean_object* v___x_8_; lean_object* v___f_9_; lean_object* v___f_10_; lean_object* v___f_11_; lean_object* v___x_12_; 
v___x_8_ = lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
lean_inc(v_r_u2080_7_);
v___f_9_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_9_, 0, v_r_u2080_7_);
lean_inc_ref(v___f_9_);
v___f_10_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_10_, 0, v___x_8_);
lean_closure_set(v___f_10_, 1, v___f_9_);
lean_inc_ref(v___f_10_);
v___f_11_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_11_, 0, v___f_10_);
v___x_12_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v___x_12_, 0, v_r_u2080_7_);
lean_ctor_set(v___x_12_, 1, v___f_10_);
lean_ctor_set(v___x_12_, 2, v___f_9_);
lean_ctor_set(v___x_12_, 3, v___f_11_);
return v___x_12_;
}
}
static lean_object* _init_lp_jeffreyorder_JeffreyOrder_Jmat___closed__0(void){
_start:
{
lean_object* v___x_13_; lean_object* v___x_14_; 
v___x_13_ = lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
v___x_14_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v___x_14_, 0, v___x_13_);
lean_ctor_set(v___x_14_, 1, v___x_13_);
lean_ctor_set(v___x_14_, 2, v___x_13_);
lean_ctor_set(v___x_14_, 3, v___x_13_);
return v___x_14_;
}
}
static lean_object* _init_lp_jeffreyorder_JeffreyOrder_Jmat(void){
_start:
{
lean_object* v___x_15_; 
v___x_15_ = lean_obj_once(&lp_jeffreyorder_JeffreyOrder_Jmat___closed__0, &lp_jeffreyorder_JeffreyOrder_Jmat___closed__0_once, _init_lp_jeffreyorder_JeffreyOrder_Jmat___closed__0);
return v___x_15_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_gradAssoc(lean_object* v_q_u2080_16_, lean_object* v_r_u2080_17_){
_start:
{
lean_object* v___x_18_; lean_object* v___f_19_; lean_object* v___f_20_; lean_object* v___f_21_; lean_object* v___f_22_; lean_object* v___f_23_; lean_object* v___f_24_; lean_object* v___f_25_; lean_object* v___f_26_; lean_object* v___f_27_; lean_object* v___f_28_; lean_object* v___x_29_; 
v___x_18_ = lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
lean_inc_n(v_q_u2080_16_, 2);
v___f_19_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_19_, 0, v_q_u2080_16_);
v___f_20_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_20_, 0, v___x_18_);
lean_closure_set(v___f_20_, 1, v___f_19_);
lean_inc_n(v_r_u2080_17_, 2);
v___f_21_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_21_, 0, v_r_u2080_17_);
v___f_22_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_22_, 0, v___x_18_);
lean_closure_set(v___f_22_, 1, v___f_21_);
lean_inc_ref(v___f_22_);
lean_inc_ref(v___f_20_);
v___f_23_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_23_, 0, v___f_20_);
lean_closure_set(v___f_23_, 1, v___f_22_);
v___f_24_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_24_, 0, v___f_20_);
lean_closure_set(v___f_24_, 1, v_r_u2080_17_);
v___f_25_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_25_, 0, v___f_24_);
v___f_26_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_26_, 0, v_q_u2080_16_);
lean_closure_set(v___f_26_, 1, v___f_22_);
v___f_27_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_27_, 0, v___f_26_);
v___f_28_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_28_, 0, v_q_u2080_16_);
lean_closure_set(v___f_28_, 1, v_r_u2080_17_);
v___x_29_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v___x_29_, 0, v___f_23_);
lean_ctor_set(v___x_29_, 1, v___f_25_);
lean_ctor_set(v___x_29_, 2, v___f_27_);
lean_ctor_set(v___x_29_, 3, v___f_28_);
return v___x_29_;
}
}
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_Zpar(lean_object* v_00_u03b1_30_, lean_object* v_00_u03b2_31_){
_start:
{
lean_object* v___f_32_; lean_object* v___x_33_; lean_object* v___f_34_; lean_object* v___f_35_; lean_object* v___f_36_; lean_object* v___f_37_; lean_object* v___f_38_; lean_object* v___f_39_; 
lean_inc(v_00_u03b2_31_);
lean_inc(v_00_u03b1_30_);
v___f_32_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_32_, 0, v_00_u03b1_30_);
lean_closure_set(v___f_32_, 1, v_00_u03b2_31_);
v___x_33_ = lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
v___f_34_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_34_, 0, v_00_u03b1_30_);
v___f_35_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_35_, 0, v___x_33_);
lean_closure_set(v___f_35_, 1, v___f_34_);
v___f_36_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_36_, 0, v___f_32_);
lean_closure_set(v___f_36_, 1, v___f_35_);
v___f_37_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_2451848184____hygCtx___hyg_8_), 2, 1);
lean_closure_set(v___f_37_, 0, v_00_u03b2_31_);
v___f_38_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_1138242547____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_38_, 0, v___x_33_);
lean_closure_set(v___f_38_, 1, v___f_37_);
v___f_39_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_39_, 0, v___f_36_);
lean_closure_set(v___f_39_, 1, v___f_38_);
return v___f_39_;
}
}
static lean_object* _init_lp_jeffreyorder_JeffreyOrder_margA___closed__0(void){
_start:
{
lean_object* v___x_40_; lean_object* v___x_41_; lean_object* v___x_42_; 
v___x_40_ = lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1279875089____hygCtx___hyg_8_;
v___x_41_ = lp_mathlib_Real_definition_00___x40_Mathlib_Data_Real_Basic_1850581184____hygCtx___hyg_8_;
v___x_42_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v___x_42_, 0, v___x_41_);
lean_ctor_set(v___x_42_, 1, v___x_41_);
lean_ctor_set(v___x_42_, 2, v___x_40_);
lean_ctor_set(v___x_42_, 3, v___x_40_);
return v___x_42_;
}
}
static lean_object* _init_lp_jeffreyorder_JeffreyOrder_margA(void){
_start:
{
lean_object* v___x_43_; 
v___x_43_ = lean_obj_once(&lp_jeffreyorder_JeffreyOrder_margA___closed__0, &lp_jeffreyorder_JeffreyOrder_margA___closed__0_once, _init_lp_jeffreyorder_JeffreyOrder_margA___closed__0);
return v___x_43_;
}
}
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_jeffreyorder_JeffreyOrder_PropDEC(uint8_t builtin);
static bool _G_initialized = false;
LEAN_EXPORT lean_object* initialize_jeffreyorder_JeffreyOrder_PropPRO(uint8_t builtin) {
lean_object * res;
if (_G_initialized) return lean_io_result_mk_ok(lean_box(0));
_G_initialized = true;
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_jeffreyorder_JeffreyOrder_PropDEC(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
lp_jeffreyorder_JeffreyOrder_Jmat = _init_lp_jeffreyorder_JeffreyOrder_Jmat();
lean_mark_persistent(lp_jeffreyorder_JeffreyOrder_Jmat);
lp_jeffreyorder_JeffreyOrder_margA = _init_lp_jeffreyorder_JeffreyOrder_margA();
lean_mark_persistent(lp_jeffreyorder_JeffreyOrder_margA);
return lean_io_result_mk_ok(lean_box(0));
}
#ifdef __cplusplus
}
#endif
