
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TANH_UF_H
#define TANH_UF_H
; 
float tanh_uf(float x){
    { return tanh(x); }; 
}

#endif
 ; 
void tanh(float * v_initial_param_193_95, float * & v_user_func_195_96, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_195_96 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_94 = 0;(v_i_94 <= (-1 + v_N_0)); (++v_i_94)){
        v_user_func_195_96[v_i_94] = tanh_uf(v_initial_param_193_95[v_i_94]); 
    }
}
}; 