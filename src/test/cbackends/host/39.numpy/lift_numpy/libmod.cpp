
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
void mod(float * v_initial_param_3736_693, float * v_initial_param_3737_694, float * & v_user_func_3743_696, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3743_696 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_692 = 0;(v_i_692 <= (-1 + v_N_352)); (++v_i_692)){
        v_user_func_3743_696[v_i_692] = fmod_uf(v_initial_param_3736_693[v_i_692], v_initial_param_3737_694[v_i_692]); 
    }
}
}; 