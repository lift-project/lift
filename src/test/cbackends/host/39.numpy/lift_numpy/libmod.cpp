
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
void mod(float * v_initial_param_696_309, float * v_initial_param_697_310, float * & v_user_func_703_312, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_703_312 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_308 = 0;(v_i_308 <= (-1 + v_N_0)); (++v_i_308)){
        v_user_func_703_312[v_i_308] = fmod_uf(v_initial_param_696_309[v_i_308], v_initial_param_697_310[v_i_308]); 
    }
}
}; 