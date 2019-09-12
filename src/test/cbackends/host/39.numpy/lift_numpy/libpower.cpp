
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
void power(float * v_initial_param_7638_3074, float * v_initial_param_7639_3075, float * & v_user_func_7645_3077, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7645_3077 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_3073 = 0;(v_i_3073 <= (-1 + v_N_2763)); (++v_i_3073)){
        v_user_func_7645_3077[v_i_3073] = power_uf(v_initial_param_7638_3074[v_i_3073], v_initial_param_7639_3075[v_i_3073]); 
    }
}
}; 