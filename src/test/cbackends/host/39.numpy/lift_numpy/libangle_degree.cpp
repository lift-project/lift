
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
void angle_degree(Tuple2_float_float * v_initial_param_768_344, float * & v_user_func_774_345, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_774_345 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_343 = 0;(v_i_343 <= (-1 + v_N_0)); (++v_i_343)){
        v_user_func_774_345[v_i_343] = angle_degree_uf(v_initial_param_768_344[v_i_343]._0, v_initial_param_768_344[v_i_343]._1); 
    }
}
}; 