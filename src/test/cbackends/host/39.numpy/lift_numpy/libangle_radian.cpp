
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
void angle_radian(Tuple2_float_float * v_initial_param_789_361, float * & v_user_func_795_362, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_795_362 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_360 = 0;(v_i_360 <= (-1 + v_N_0)); (++v_i_360)){
        v_user_func_795_362[v_i_360] = angle_randian_uf(v_initial_param_789_361[v_i_360]._0, v_initial_param_789_361[v_i_360]._1); 
    }
}
}; 