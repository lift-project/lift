
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
void multiply(float * v_initial_param_638_283, float * v_initial_param_639_284, float * & v_user_func_645_286, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_645_286 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_282 = 0;(v_i_282 <= (-1 + v_N_0)); (++v_i_282)){
        v_user_func_645_286[v_i_282] = multiply_uf(v_initial_param_638_283[v_i_282], v_initial_param_639_284[v_i_282]); 
    }
}
}; 