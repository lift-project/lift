
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
void fmod(float * v_initial_param_694_302, float * v_initial_param_695_303, float * & v_user_func_701_305, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_701_305 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_301 = 0;(v_i_301 <= (-1 + v_N_0)); (++v_i_301)){
        v_user_func_701_305[v_i_301] = fmod_uf(v_initial_param_694_302[v_i_301], v_initial_param_695_303[v_i_301]); 
    }
}
}; 