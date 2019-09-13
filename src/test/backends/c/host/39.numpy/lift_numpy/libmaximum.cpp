
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MAXIMUM_UF_H
#define MAXIMUM_UF_H
; 
float maximum_uf(float x, float y){
    { return max(x,y); }; 
}

#endif
 ; 
void maximum(float * v_initial_param_7848_3161, float * v_initial_param_7849_3162, float * & v_user_func_7855_3164, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7855_3164 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3160 = 0;(v_i_3160 <= (-1 + v_N_2763)); (++v_i_3160)){
        v_user_func_7855_3164[v_i_3160] = maximum_uf(v_initial_param_7848_3161[v_i_3160], v_initial_param_7849_3162[v_i_3160]); 
    }
}
}; 