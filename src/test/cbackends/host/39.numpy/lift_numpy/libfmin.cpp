
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMIN_UF_H
#define FMIN_UF_H
; 
float fmin_uf(float x, float y){
    { return min(x,y); }; 
}

#endif
 ; 
void fmin(float * v_initial_param_7890_3176, float * v_initial_param_7891_3177, float * & v_user_func_7897_3179, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7897_3179 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3175 = 0;(v_i_3175 <= (-1 + v_N_2763)); (++v_i_3175)){
        v_user_func_7897_3179[v_i_3175] = fmin_uf(v_initial_param_7890_3176[v_i_3175], v_initial_param_7891_3177[v_i_3175]); 
    }
}
}; 