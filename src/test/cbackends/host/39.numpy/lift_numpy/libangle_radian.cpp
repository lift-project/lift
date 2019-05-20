
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ANGLE_RANDIAN_UF_H
#define ANGLE_RANDIAN_UF_H
; 
float angle_randian_uf(float x, float y){
    { return atan2(y,x); }; 
}

#endif
 ; 
void angle_radian(Tuple2_float_float * v_initial_param_776_355, float * & v_user_func_782_356, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_782_356 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_354 = 0;(v_i_354 <= (-1 + v_N_0)); (++v_i_354)){
        v_user_func_782_356[v_i_354] = angle_randian_uf(v_initial_param_776_355[v_i_354]._0, v_initial_param_776_355[v_i_354]._1); 
    }
}
}; 