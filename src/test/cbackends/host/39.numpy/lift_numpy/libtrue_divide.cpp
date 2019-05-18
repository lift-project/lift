
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
void true_divide(float * v_initial_param_641_290, float * v_initial_param_642_291, float * & v_user_func_648_293, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_648_293 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_289 = 0;(v_i_289 <= (-1 + v_N_0)); (++v_i_289)){
        v_user_func_648_293[v_i_289] = divide_uf(v_initial_param_641_290[v_i_289], v_initial_param_642_291[v_i_289]); 
    }
}
}; 