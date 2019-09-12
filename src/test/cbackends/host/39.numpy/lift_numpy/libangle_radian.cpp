
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
void angle_radian(Tuple2_float_float * v_initial_param_7729_3122, float * & v_user_func_7735_3123, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7735_3123 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3121 = 0;(v_i_3121 <= (-1 + v_N_2763)); (++v_i_3121)){
        v_user_func_7735_3123[v_i_3121] = angle_randian_uf(v_initial_param_7729_3122[v_i_3121]._0, v_initial_param_7729_3122[v_i_3121]._1); 
    }
}
}; 