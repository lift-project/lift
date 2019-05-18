
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MULTIPLY_UF_H
#define MULTIPLY_UF_H
; 
float multiply_uf(float x, float y){
    return x * y;; 
}

#endif
 ; 
void multiply(float * v_initial_param_624_267, float * v_initial_param_625_268, float * & v_user_func_631_270, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_631_270 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_266 = 0;(v_i_266 <= (-1 + v_N_0)); (++v_i_266)){
        v_user_func_631_270[v_i_266] = multiply_uf(v_initial_param_624_267[v_i_266], v_initial_param_625_268[v_i_266]); 
    }
}
}; 