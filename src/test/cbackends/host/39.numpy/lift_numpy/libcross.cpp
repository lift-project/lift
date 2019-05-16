
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
void cross(Tuple3_float_float_float * v_initial_param_341_157, Tuple3_float_float_float * v_initial_param_342_158, Tuple3_float_float_float * & v_user_func_368_160, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_368_160 = reinterpret_cast<Tuple3_float_float_float *>(malloc((v_N_0 * sizeof(Tuple3_float_float_float)))); 
    // For each element processed sequentially
    for (int v_i_156 = 0;(v_i_156 <= (-1 + v_N_0)); (++v_i_156)){
        v_user_func_368_160[v_i_156] = cross_calc(v_initial_param_341_157[v_i_156]._0, v_initial_param_341_157[v_i_156]._1, v_initial_param_341_157[v_i_156]._2, v_initial_param_342_158[v_i_156]._0, v_initial_param_342_158[v_i_156]._1, v_initial_param_342_158[v_i_156]._2); 
    }
}
}; 