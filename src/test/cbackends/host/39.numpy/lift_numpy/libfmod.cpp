
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
void fmod(float * v_initial_param_3736_688, float * v_initial_param_3737_689, float * & v_user_func_3743_691, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3743_691 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_687 = 0;(v_i_687 <= (-1 + v_N_352)); (++v_i_687)){
        v_user_func_3743_691[v_i_687] = fmod_uf(v_initial_param_3736_688[v_i_687], v_initial_param_3737_689[v_i_687]); 
    }
}
}; 