
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
void imag(Tuple2_float_float * v_initial_param_790_350, float * & v_user_func_796_351, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_796_351 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_349 = 0;(v_i_349 <= (-1 + v_N_0)); (++v_i_349)){
        v_user_func_796_351[v_i_349] = imag_uf(v_initial_param_790_350[v_i_349]._0, v_initial_param_790_350[v_i_349]._1); 
    }
}
}; 