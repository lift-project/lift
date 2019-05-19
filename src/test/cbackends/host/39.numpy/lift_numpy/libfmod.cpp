
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
void fmod(float * v_initial_param_708_316, float * v_initial_param_709_317, float * & v_user_func_715_319, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_715_319 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_315 = 0;(v_i_315 <= (-1 + v_N_0)); (++v_i_315)){
        v_user_func_715_319[v_i_315] = fmod_uf(v_initial_param_708_316[v_i_315], v_initial_param_709_317[v_i_315]); 
    }
}
}; 