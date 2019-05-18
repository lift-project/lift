
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
void mod(float * v_initial_param_693_306, float * v_initial_param_694_307, float * & v_user_func_700_309, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_700_309 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_305 = 0;(v_i_305 <= (-1 + v_N_0)); (++v_i_305)){
        v_user_func_700_309[v_i_305] = fmod_uf(v_initial_param_693_306[v_i_305], v_initial_param_694_307[v_i_305]); 
    }
}
}; 