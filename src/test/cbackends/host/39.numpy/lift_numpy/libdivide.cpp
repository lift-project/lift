
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
void divide(float * v_initial_param_671_302, float * v_initial_param_672_303, float * & v_user_func_678_305, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_678_305 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_301 = 0;(v_i_301 <= (-1 + v_N_0)); (++v_i_301)){
        v_user_func_678_305[v_i_301] = divide_uf(v_initial_param_671_302[v_i_301], v_initial_param_672_303[v_i_301]); 
    }
}
}; 