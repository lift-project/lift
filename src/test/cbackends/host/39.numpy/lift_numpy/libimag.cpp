
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
void imag(Tuple2_float_float * v_initial_param_822_370, float * & v_user_func_828_371, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_828_371 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_369 = 0;(v_i_369 <= (-1 + v_N_0)); (++v_i_369)){
        v_user_func_828_371[v_i_369] = imag_uf(v_initial_param_822_370[v_i_369]._0, v_initial_param_822_370[v_i_369]._1); 
    }
}
}; 