
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
void imag(Tuple2_float_float * v_initial_param_809_364, float * & v_user_func_815_365, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_815_365 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_363 = 0;(v_i_363 <= (-1 + v_N_0)); (++v_i_363)){
        v_user_func_815_365[v_i_363] = imag_uf(v_initial_param_809_364[v_i_363]._0, v_initial_param_809_364[v_i_363]._1); 
    }
}
}; 