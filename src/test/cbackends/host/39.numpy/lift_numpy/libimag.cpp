
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef IMAG_UF_H
#define IMAG_UF_H
; 
float imag_uf(float x, float y){
    { return y; }; 
}

#endif
 ; 
void imag(Tuple2_float_float * v_initial_param_790_348, float * & v_user_func_796_349, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_796_349 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_347 = 0;(v_i_347 <= (-1 + v_N_0)); (++v_i_347)){
        v_user_func_796_349[v_i_347] = imag_uf(v_initial_param_790_348[v_i_347]._0, v_initial_param_790_348[v_i_347]._1); 
    }
}
}; 