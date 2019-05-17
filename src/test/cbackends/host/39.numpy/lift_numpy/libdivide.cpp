
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
void divide(float * v_initial_param_634_268, float * v_initial_param_635_269, float * & v_user_func_641_271, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_641_271 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_267 = 0;(v_i_267 <= (-1 + v_N_0)); (++v_i_267)){
        v_user_func_641_271[v_i_267] = divide_uf(v_initial_param_634_268[v_i_267], v_initial_param_635_269[v_i_267]); 
    }
}
}; 