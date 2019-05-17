
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
void mod(float * v_initial_param_692_305, float * v_initial_param_693_306, float * & v_user_func_699_308, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_699_308 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_304 = 0;(v_i_304 <= (-1 + v_N_0)); (++v_i_304)){
        v_user_func_699_308[v_i_304] = fmod_uf(v_initial_param_692_305[v_i_304], v_initial_param_693_306[v_i_304]); 
    }
}
}; 