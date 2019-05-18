
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
void multiply(float * v_initial_param_626_269, float * v_initial_param_627_270, float * & v_user_func_633_272, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_633_272 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_268 = 0;(v_i_268 <= (-1 + v_N_0)); (++v_i_268)){
        v_user_func_633_272[v_i_268] = multiply_uf(v_initial_param_626_269[v_i_268], v_initial_param_627_270[v_i_268]); 
    }
}
}; 