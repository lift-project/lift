
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
void angle_degree(Tuple2_float_float * v_initial_param_7740_3125, float * & v_user_func_7746_3126, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7746_3126 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3124 = 0;(v_i_3124 <= (-1 + v_N_2763)); (++v_i_3124)){
        v_user_func_7746_3126[v_i_3124] = angle_degree_uf(v_initial_param_7740_3125[v_i_3124]._0, v_initial_param_7740_3125[v_i_3124]._1); 
    }
}
}; 