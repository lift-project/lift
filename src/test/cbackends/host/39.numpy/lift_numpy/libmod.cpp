
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
void mod(float * v_initial_param_727_337, float * v_initial_param_728_338, float * & v_user_func_734_340, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_734_340 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_336 = 0;(v_i_336 <= (-1 + v_N_0)); (++v_i_336)){
        v_user_func_734_340[v_i_336] = fmod_uf(v_initial_param_727_337[v_i_336], v_initial_param_728_338[v_i_336]); 
    }
}
}; 