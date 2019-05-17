
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
void true_divide(float * v_initial_param_634_283, float * v_initial_param_635_284, float * & v_user_func_641_286, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_641_286 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_282 = 0;(v_i_282 <= (-1 + v_N_0)); (++v_i_282)){
        v_user_func_641_286[v_i_282] = divide_uf(v_initial_param_634_283[v_i_282], v_initial_param_635_284[v_i_282]); 
    }
}
}; 