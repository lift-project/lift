
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
void angle_radian(Tuple2_float_float * v_initial_param_757_339, float * & v_user_func_763_340, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_763_340 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_338 = 0;(v_i_338 <= (-1 + v_N_0)); (++v_i_338)){
        v_user_func_763_340[v_i_338] = angle_randian_uf(v_initial_param_757_339[v_i_338]._0, v_initial_param_757_339[v_i_338]._1); 
    }
}
}; 