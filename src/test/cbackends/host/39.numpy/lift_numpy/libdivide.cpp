
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
void divide(float * v_initial_param_635_269, float * v_initial_param_636_270, float * & v_user_func_642_272, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_642_272 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_268 = 0;(v_i_268 <= (-1 + v_N_0)); (++v_i_268)){
        v_user_func_642_272[v_i_268] = divide_uf(v_initial_param_635_269[v_i_268], v_initial_param_636_270[v_i_268]); 
    }
}
}; 