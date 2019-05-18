
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
void multiply(float * v_initial_param_623_266, float * v_initial_param_624_267, float * & v_user_func_630_269, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_630_269 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_265 = 0;(v_i_265 <= (-1 + v_N_0)); (++v_i_265)){
        v_user_func_630_269[v_i_265] = multiply_uf(v_initial_param_623_266[v_i_265], v_initial_param_624_267[v_i_265]); 
    }
}
}; 