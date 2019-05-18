
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
void divide(float * v_initial_param_638_272, float * v_initial_param_639_273, float * & v_user_func_645_275, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_645_275 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_271 = 0;(v_i_271 <= (-1 + v_N_0)); (++v_i_271)){
        v_user_func_645_275[v_i_271] = divide_uf(v_initial_param_638_272[v_i_271], v_initial_param_639_273[v_i_271]); 
    }
}
}; 