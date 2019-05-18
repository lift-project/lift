
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
void mod(float * v_initial_param_697_310, float * v_initial_param_698_311, float * & v_user_func_704_313, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_704_313 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_309 = 0;(v_i_309 <= (-1 + v_N_0)); (++v_i_309)){
        v_user_func_704_313[v_i_309] = fmod_uf(v_initial_param_697_310[v_i_309], v_initial_param_698_311[v_i_309]); 
    }
}
}; 