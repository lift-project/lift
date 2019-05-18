
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
void fmod(float * v_initial_param_693_301, float * v_initial_param_694_302, float * & v_user_func_700_304, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_700_304 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_300 = 0;(v_i_300 <= (-1 + v_N_0)); (++v_i_300)){
        v_user_func_700_304[v_i_300] = fmod_uf(v_initial_param_693_301[v_i_300], v_initial_param_694_302[v_i_300]); 
    }
}
}; 