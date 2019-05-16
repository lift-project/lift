
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TUPLE3_FLOAT_FLOAT_FLOAT_H
#define TUPLE3_FLOAT_FLOAT_FLOAT_H
; 
// NOTE: trying to print unprintable type: float
 // NOTE: trying to print unprintable type: float
 // NOTE: trying to print unprintable type: float
typedef struct{
    float _0;
    float _1;
    float _2;
} Tuple3_float_float_float;


#endif
 ; 
#ifndef CROSS_CALC_H
#define CROSS_CALC_H
; 
Tuple3_float_float_float cross_calc(float a1, float a2, float a3, float b1, float b2, float b3){
    { return {a2 * b3 - a3 * b2, a3 * b1 - a1 * b3, a1 * b2 - a2 * b1 };}; 
}

#endif
 ; 
void cross(Tuple3_float_float_float * v_initial_param_350_166, Tuple3_float_float_float * v_initial_param_351_167, Tuple3_float_float_float * & v_user_func_377_169, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_377_169 = reinterpret_cast<Tuple3_float_float_float *>(malloc((v_N_0 * sizeof(Tuple3_float_float_float)))); 
    // For each element processed sequentially
    for (int v_i_165 = 0;(v_i_165 <= (-1 + v_N_0)); (++v_i_165)){
        v_user_func_377_169[v_i_165] = cross_calc(v_initial_param_350_166[v_i_165]._0, v_initial_param_350_166[v_i_165]._1, v_initial_param_350_166[v_i_165]._2, v_initial_param_351_167[v_i_165]._0, v_initial_param_351_167[v_i_165]._1, v_initial_param_351_167[v_i_165]._2); 
    }
}
}; 