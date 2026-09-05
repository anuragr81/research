// Lean compiler output
// Module: JeffreyOrder.PropDEC
// Imports: public import Init public meta import Init public import JeffreyOrder.PropIMM
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
lean_object* lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_(lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_sep(lean_object*, lean_object*, lean_object*, lean_object*, lean_object*);
LEAN_EXPORT lean_object* lp_jeffreyorder_JeffreyOrder_sep(lean_object* v_g_u2080_1_, lean_object* v_g_u2081_2_, lean_object* v_h_u2080_3_, lean_object* v_h_u2081_4_, lean_object* v_Q_5_){
_start:
{
lean_object* v_a00_6_; lean_object* v_a01_7_; lean_object* v_a10_8_; lean_object* v_a11_9_; lean_object* v___x_11_; uint8_t v_isShared_12_; uint8_t v_isSharedCheck_24_; 
v_a00_6_ = lean_ctor_get(v_Q_5_, 0);
v_a01_7_ = lean_ctor_get(v_Q_5_, 1);
v_a10_8_ = lean_ctor_get(v_Q_5_, 2);
v_a11_9_ = lean_ctor_get(v_Q_5_, 3);
v_isSharedCheck_24_ = !lean_is_exclusive(v_Q_5_);
if (v_isSharedCheck_24_ == 0)
{
v___x_11_ = v_Q_5_;
v_isShared_12_ = v_isSharedCheck_24_;
goto v_resetjp_10_;
}
else
{
lean_inc(v_a11_9_);
lean_inc(v_a10_8_);
lean_inc(v_a01_7_);
lean_inc(v_a00_6_);
lean_dec(v_Q_5_);
v___x_11_ = lean_box(0);
v_isShared_12_ = v_isSharedCheck_24_;
goto v_resetjp_10_;
}
v_resetjp_10_:
{
lean_object* v___f_13_; lean_object* v___f_14_; lean_object* v___f_15_; lean_object* v___f_16_; lean_object* v___f_17_; lean_object* v___f_18_; lean_object* v___f_19_; lean_object* v___f_20_; lean_object* v___x_22_; 
lean_inc(v_h_u2080_3_);
lean_inc(v_g_u2080_1_);
v___f_13_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_13_, 0, v_g_u2080_1_);
lean_closure_set(v___f_13_, 1, v_h_u2080_3_);
v___f_14_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_14_, 0, v___f_13_);
lean_closure_set(v___f_14_, 1, v_a00_6_);
lean_inc(v_h_u2081_4_);
v___f_15_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_15_, 0, v_g_u2080_1_);
lean_closure_set(v___f_15_, 1, v_h_u2081_4_);
v___f_16_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_16_, 0, v___f_15_);
lean_closure_set(v___f_16_, 1, v_a01_7_);
lean_inc(v_g_u2081_2_);
v___f_17_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_17_, 0, v_g_u2081_2_);
lean_closure_set(v___f_17_, 1, v_h_u2080_3_);
v___f_18_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_18_, 0, v___f_17_);
lean_closure_set(v___f_18_, 1, v_a10_8_);
v___f_19_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_19_, 0, v_g_u2081_2_);
lean_closure_set(v___f_19_, 1, v_h_u2081_4_);
v___f_20_ = lean_alloc_closure((void*)(lp_mathlib_Real_definition___lam__0_00___x40_Mathlib_Data_Real_Basic_4214226450____hygCtx___hyg_8_), 3, 2);
lean_closure_set(v___f_20_, 0, v___f_19_);
lean_closure_set(v___f_20_, 1, v_a11_9_);
if (v_isShared_12_ == 0)
{
lean_ctor_set(v___x_11_, 3, v___f_20_);
lean_ctor_set(v___x_11_, 2, v___f_18_);
lean_ctor_set(v___x_11_, 1, v___f_16_);
lean_ctor_set(v___x_11_, 0, v___f_14_);
v___x_22_ = v___x_11_;
goto v_reusejp_21_;
}
else
{
lean_object* v_reuseFailAlloc_23_; 
v_reuseFailAlloc_23_ = lean_alloc_ctor(0, 4, 0);
lean_ctor_set(v_reuseFailAlloc_23_, 0, v___f_14_);
lean_ctor_set(v_reuseFailAlloc_23_, 1, v___f_16_);
lean_ctor_set(v_reuseFailAlloc_23_, 2, v___f_18_);
lean_ctor_set(v_reuseFailAlloc_23_, 3, v___f_20_);
v___x_22_ = v_reuseFailAlloc_23_;
goto v_reusejp_21_;
}
v_reusejp_21_:
{
return v___x_22_;
}
}
}
}
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_Init(uint8_t builtin);
lean_object* initialize_jeffreyorder_JeffreyOrder_PropIMM(uint8_t builtin);
static bool _G_initialized = false;
LEAN_EXPORT lean_object* initialize_jeffreyorder_JeffreyOrder_PropDEC(uint8_t builtin) {
lean_object * res;
if (_G_initialized) return lean_io_result_mk_ok(lean_box(0));
_G_initialized = true;
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_Init(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
res = initialize_jeffreyorder_JeffreyOrder_PropIMM(builtin);
if (lean_io_result_is_error(res)) return res;
lean_dec_ref(res);
return lean_io_result_mk_ok(lean_box(0));
}
#ifdef __cplusplus
}
#endif
