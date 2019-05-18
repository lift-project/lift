
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
void angle_radian(Tuple2_float_float * v_initial_param_745_327, float * & v_user_func_751_328, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_751_328 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_326 = 0;(v_i_326 <= (-1 + v_N_0)); (++v_i_326)){
        v_user_func_751_328[v_i_326] = angle_randian_uf(v_initial_param_745_327[v_i_326]._0, v_initial_param_745_327[v_i_326]._1); 
    }
}
}; 