
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
void divide(float * v_initial_param_636_270, float * v_initial_param_637_271, float * & v_user_func_643_273, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_643_273 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_269 = 0;(v_i_269 <= (-1 + v_N_0)); (++v_i_269)){
        v_user_func_643_273[v_i_269] = divide_uf(v_initial_param_636_270[v_i_269], v_initial_param_637_271[v_i_269]); 
    }
}
}; 