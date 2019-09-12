
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef REMAINDER_UF_H
#define REMAINDER_UF_H
; 
float remainder_uf(float x, float y){
    if(x>=0) return x - floor(x/y)*y; else return x - round(x/y)*y; 
}

#endif
 ; 
void lift_remainder(float * v_initial_param_7701_3112, float * v_initial_param_7702_3113, float * & v_user_func_7708_3115, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7708_3115 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3111 = 0;(v_i_3111 <= (-1 + v_N_2763)); (++v_i_3111)){
        v_user_func_7708_3115[v_i_3111] = remainder_uf(v_initial_param_7701_3112[v_i_3111], v_initial_param_7702_3113[v_i_3111]); 
    }
}
}; 