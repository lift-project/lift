
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
void fmod(float * v_initial_param_7680_3099, float * v_initial_param_7681_3100, float * & v_user_func_7687_3102, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7687_3102 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3098 = 0;(v_i_3098 <= (-1 + v_N_2763)); (++v_i_3098)){
        v_user_func_7687_3102[v_i_3098] = fmod_uf(v_initial_param_7680_3099[v_i_3098], v_initial_param_7681_3100[v_i_3098]); 
    }
}
}; 