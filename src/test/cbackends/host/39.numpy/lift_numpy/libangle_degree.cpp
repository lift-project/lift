
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
void angle_degree(Tuple2_float_float * v_initial_param_3796_714, float * & v_user_func_3802_715, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3802_715 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_713 = 0;(v_i_713 <= (-1 + v_N_352)); (++v_i_713)){
        v_user_func_3802_715[v_i_713] = angle_degree_uf(v_initial_param_3796_714[v_i_713]._0, v_initial_param_3796_714[v_i_713]._1); 
    }
}
}; 