
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef POWER_UF_H
#define POWER_UF_H
; 
float power_uf(float x, float y){
    return pow(x, y);; 
}

#endif
 ; 
void float_power(float * v_initial_param_7638_3094, float * v_initial_param_7639_3095, float * & v_user_func_7645_3097, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7645_3097 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3093 = 0;(v_i_3093 <= (-1 + v_N_2763)); (++v_i_3093)){
        v_user_func_7645_3097[v_i_3093] = power_uf(v_initial_param_7638_3094[v_i_3093], v_initial_param_7639_3095[v_i_3093]); 
    }
}
}; 