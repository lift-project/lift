
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
void true_divide(float * v_initial_param_641_292, float * v_initial_param_642_293, float * & v_user_func_648_295, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_648_295 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_291 = 0;(v_i_291 <= (-1 + v_N_0)); (++v_i_291)){
        v_user_func_648_295[v_i_291] = divide_uf(v_initial_param_641_292[v_i_291], v_initial_param_642_293[v_i_291]); 
    }
}
}; 