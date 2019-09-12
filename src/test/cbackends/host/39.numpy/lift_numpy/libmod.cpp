
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
void mod(float * v_initial_param_7680_3104, float * v_initial_param_7681_3105, float * & v_user_func_7687_3107, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7687_3107 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3103 = 0;(v_i_3103 <= (-1 + v_N_2763)); (++v_i_3103)){
        v_user_func_7687_3107[v_i_3103] = fmod_uf(v_initial_param_7680_3104[v_i_3103], v_initial_param_7681_3105[v_i_3103]); 
    }
}
}; 