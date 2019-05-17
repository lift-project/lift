
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
void sinh(float * v_initial_param_193_105, float * & v_user_func_195_106, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_195_106 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_104 = 0;(v_i_104 <= (-1 + v_N_0)); (++v_i_104)){
        v_user_func_195_106[v_i_104] = sinh_uf(v_initial_param_193_105[v_i_104]); 
    }
}
}; 