
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
void mod(float * v_initial_param_740_343, float * v_initial_param_741_344, float * & v_user_func_747_346, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_747_346 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_342 = 0;(v_i_342 <= (-1 + v_N_0)); (++v_i_342)){
        v_user_func_747_346[v_i_342] = fmod_uf(v_initial_param_740_343[v_i_342], v_initial_param_741_344[v_i_342]); 
    }
}
}; 