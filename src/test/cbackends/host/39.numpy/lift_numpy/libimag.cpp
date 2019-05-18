
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
void imag(Tuple2_float_float * v_initial_param_779_337, float * & v_user_func_785_338, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_785_338 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_336 = 0;(v_i_336 <= (-1 + v_N_0)); (++v_i_336)){
        v_user_func_785_338[v_i_336] = imag_uf(v_initial_param_779_337[v_i_336]._0, v_initial_param_779_337[v_i_336]._1); 
    }
}
}; 