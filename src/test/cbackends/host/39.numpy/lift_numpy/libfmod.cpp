
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
void fmod(float * v_initial_param_708_318, float * v_initial_param_709_319, float * & v_user_func_715_321, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_715_321 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_317 = 0;(v_i_317 <= (-1 + v_N_0)); (++v_i_317)){
        v_user_func_715_321[v_i_317] = fmod_uf(v_initial_param_708_318[v_i_317], v_initial_param_709_319[v_i_317]); 
    }
}
}; 