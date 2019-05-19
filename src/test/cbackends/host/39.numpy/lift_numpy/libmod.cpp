
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
void mod(float * v_initial_param_708_323, float * v_initial_param_709_324, float * & v_user_func_715_326, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_715_326 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_322 = 0;(v_i_322 <= (-1 + v_N_0)); (++v_i_322)){
        v_user_func_715_326[v_i_322] = fmod_uf(v_initial_param_708_323[v_i_322], v_initial_param_709_324[v_i_322]); 
    }
}
}; 