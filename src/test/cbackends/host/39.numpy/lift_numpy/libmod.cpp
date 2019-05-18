
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMOD_UF_H
#define FMOD_UF_H
; 
float fmod_uf(float x, float y){
    return ((int)x) % ((int)y);; 
}

#endif
 ; 
void mod(float * v_initial_param_697_312, float * v_initial_param_698_313, float * & v_user_func_704_315, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_704_315 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_311 = 0;(v_i_311 <= (-1 + v_N_0)); (++v_i_311)){
        v_user_func_704_315[v_i_311] = fmod_uf(v_initial_param_697_312[v_i_311], v_initial_param_698_313[v_i_311]); 
    }
}
}; 