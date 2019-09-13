
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
void imag(Tuple2_float_float * v_initial_param_7762_3131, float * & v_user_func_7768_3132, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7768_3132 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3130 = 0;(v_i_3130 <= (-1 + v_N_2763)); (++v_i_3130)){
        v_user_func_7768_3132[v_i_3130] = imag_uf(v_initial_param_7762_3131[v_i_3130]._0, v_initial_param_7762_3131[v_i_3130]._1); 
    }
}
}; 