
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
void imag(Tuple2_float_float * v_initial_param_3818_720, float * & v_user_func_3824_721, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3824_721 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_719 = 0;(v_i_719 <= (-1 + v_N_352)); (++v_i_719)){
        v_user_func_3824_721[v_i_719] = imag_uf(v_initial_param_3818_720[v_i_719]._0, v_initial_param_3818_720[v_i_719]._1); 
    }
}
}; 