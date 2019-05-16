
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TRUNC_UF_H
#define TRUNC_UF_H
; 
float trunc_uf(float x){
    return trunc(x);; 
}

#endif
 ; 
void trunc(float * v_initial_param_262_131, float * & v_user_func_264_132, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_264_132 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_130 = 0;(v_i_130 <= (-1 + v_N_0)); (++v_i_130)){
        v_user_func_264_132[v_i_130] = trunc_uf(v_initial_param_262_131[v_i_130]); 
    }
}
}; 