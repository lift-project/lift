
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
void true_divide(float * v_initial_param_652_301, float * v_initial_param_653_302, float * & v_user_func_659_304, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_659_304 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_300 = 0;(v_i_300 <= (-1 + v_N_0)); (++v_i_300)){
        v_user_func_659_304[v_i_300] = divide_uf(v_initial_param_652_301[v_i_300], v_initial_param_653_302[v_i_300]); 
    }
}
}; 