
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
void angle_radian(Tuple2_float_float * v_initial_param_742_324, float * & v_user_func_748_325, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_748_325 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_323 = 0;(v_i_323 <= (-1 + v_N_0)); (++v_i_323)){
        v_user_func_748_325[v_i_323] = angle_randian_uf(v_initial_param_742_324[v_i_323]._0, v_initial_param_742_324[v_i_323]._1); 
    }
}
}; 