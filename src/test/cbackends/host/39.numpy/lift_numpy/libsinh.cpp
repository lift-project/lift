
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SINH_UF_H
#define SINH_UF_H
; 
float sinh_uf(float x){
    { return sinh(x); }; 
}

#endif
 ; 
void sinh(float * v_initial_param_214_128, float * & v_user_func_216_129, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_216_129 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_127 = 0;(v_i_127 <= (-1 + v_N_0)); (++v_i_127)){
        v_user_func_216_129[v_i_127] = sinh_uf(v_initial_param_214_128[v_i_127]); 
    }
}
}; 