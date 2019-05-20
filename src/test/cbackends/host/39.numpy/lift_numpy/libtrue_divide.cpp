
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIVIDE_UF_H
#define DIVIDE_UF_H
; 
float divide_uf(float x, float y){
    return x / y;; 
}

#endif
 ; 
void true_divide(float * v_initial_param_671_317, float * v_initial_param_672_318, float * & v_user_func_678_320, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_678_320 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_316 = 0;(v_i_316 <= (-1 + v_N_0)); (++v_i_316)){
        v_user_func_678_320[v_i_316] = divide_uf(v_initial_param_671_317[v_i_316], v_initial_param_672_318[v_i_316]); 
    }
}
}; 