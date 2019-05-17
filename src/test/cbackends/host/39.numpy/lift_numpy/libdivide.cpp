
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
void divide(float * v_initial_param_632_266, float * v_initial_param_633_267, float * & v_user_func_639_269, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_639_269 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_265 = 0;(v_i_265 <= (-1 + v_N_0)); (++v_i_265)){
        v_user_func_639_269[v_i_265] = divide_uf(v_initial_param_632_266[v_i_265], v_initial_param_633_267[v_i_265]); 
    }
}
}; 