
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
void multiply(float * v_initial_param_627_270, float * v_initial_param_628_271, float * & v_user_func_634_273, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_634_273 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_269 = 0;(v_i_269 <= (-1 + v_N_0)); (++v_i_269)){
        v_user_func_634_273[v_i_269] = multiply_uf(v_initial_param_627_270[v_i_269], v_initial_param_628_271[v_i_269]); 
    }
}
}; 