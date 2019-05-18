
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
void fmod(float * v_initial_param_697_305, float * v_initial_param_698_306, float * & v_user_func_704_308, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_704_308 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_304 = 0;(v_i_304 <= (-1 + v_N_0)); (++v_i_304)){
        v_user_func_704_308[v_i_304] = fmod_uf(v_initial_param_697_305[v_i_304], v_initial_param_698_306[v_i_304]); 
    }
}
}; 