
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
void angle_radian(Tuple2_float_float * v_initial_param_3785_711, float * & v_user_func_3791_712, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3791_712 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_710 = 0;(v_i_710 <= (-1 + v_N_352)); (++v_i_710)){
        v_user_func_3791_712[v_i_710] = angle_randian_uf(v_initial_param_3785_711[v_i_710]._0, v_initial_param_3785_711[v_i_710]._1); 
    }
}
}; 