
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
void fmod(float * v_initial_param_740_338, float * v_initial_param_741_339, float * & v_user_func_747_341, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_747_341 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_337 = 0;(v_i_337 <= (-1 + v_N_0)); (++v_i_337)){
        v_user_func_747_341[v_i_337] = fmod_uf(v_initial_param_740_338[v_i_337], v_initial_param_741_339[v_i_337]); 
    }
}
}; 