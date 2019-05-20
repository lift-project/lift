
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ANGLE_DEGREE_UF_H
#define ANGLE_DEGREE_UF_H
; 
float angle_degree_uf(float x, float y){
    { return atan2(y,x) * 180 / M_PI; }; 
}

#endif
 ; 
void angle_degree(Tuple2_float_float * v_initial_param_800_364, float * & v_user_func_806_365, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_806_365 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_363 = 0;(v_i_363 <= (-1 + v_N_0)); (++v_i_363)){
        v_user_func_806_365[v_i_363] = angle_degree_uf(v_initial_param_800_364[v_i_363]._0, v_initial_param_800_364[v_i_363]._1); 
    }
}
}; 