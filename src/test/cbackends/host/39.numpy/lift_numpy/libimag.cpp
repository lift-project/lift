
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
void imag(Tuple2_float_float * v_initial_param_778_336, float * & v_user_func_784_337, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_784_337 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_335 = 0;(v_i_335 <= (-1 + v_N_0)); (++v_i_335)){
        v_user_func_784_337[v_i_335] = imag_uf(v_initial_param_778_336[v_i_335]._0, v_initial_param_778_336[v_i_335]._1); 
    }
}
}; 