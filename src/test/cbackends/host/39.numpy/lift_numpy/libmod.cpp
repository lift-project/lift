
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
void mod(float * v_initial_param_708_321, float * v_initial_param_709_322, float * & v_user_func_715_324, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_715_324 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_320 = 0;(v_i_320 <= (-1 + v_N_0)); (++v_i_320)){
        v_user_func_715_324[v_i_320] = fmod_uf(v_initial_param_708_321[v_i_320], v_initial_param_709_322[v_i_320]); 
    }
}
}; 