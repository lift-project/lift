
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
void fmod(float * v_initial_param_727_332, float * v_initial_param_728_333, float * & v_user_func_734_335, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_734_335 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_331 = 0;(v_i_331 <= (-1 + v_N_0)); (++v_i_331)){
        v_user_func_734_335[v_i_331] = fmod_uf(v_initial_param_727_332[v_i_331], v_initial_param_728_333[v_i_331]); 
    }
}
}; 