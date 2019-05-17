
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
void cross(Tuple3_float_float_float * v_initial_param_358_176, Tuple3_float_float_float * v_initial_param_359_177, Tuple3_float_float_float * & v_user_func_385_179, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_385_179 = reinterpret_cast<Tuple3_float_float_float *>(malloc((v_N_0 * sizeof(Tuple3_float_float_float)))); 
    // For each element processed sequentially
    for (int v_i_175 = 0;(v_i_175 <= (-1 + v_N_0)); (++v_i_175)){
        v_user_func_385_179[v_i_175] = cross_calc(v_initial_param_358_176[v_i_175]._0, v_initial_param_358_176[v_i_175]._1, v_initial_param_358_176[v_i_175]._2, v_initial_param_359_177[v_i_175]._0, v_initial_param_359_177[v_i_175]._1, v_initial_param_359_177[v_i_175]._2); 
    }
}
}; 