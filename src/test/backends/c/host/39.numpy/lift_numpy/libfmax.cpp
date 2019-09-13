
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMAX_UF_H
#define FMAX_UF_H
; 
float fmax_uf(float x, float y){
    { return max(x,y); }; 
}

#endif
 ; 
void fmax(float * v_initial_param_7876_3171, float * v_initial_param_7877_3172, float * & v_user_func_7883_3174, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7883_3174 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3170 = 0;(v_i_3170 <= (-1 + v_N_2763)); (++v_i_3170)){
        v_user_func_7883_3174[v_i_3170] = fmax_uf(v_initial_param_7876_3171[v_i_3170], v_initial_param_7877_3172[v_i_3170]); 
    }
}
}; 