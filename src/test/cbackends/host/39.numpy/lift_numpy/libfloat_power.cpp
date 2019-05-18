
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
void float_power(float * v_initial_param_654_299, float * v_initial_param_655_300, float * & v_user_func_661_302, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_661_302 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_298 = 0;(v_i_298 <= (-1 + v_N_0)); (++v_i_298)){
        v_user_func_661_302[v_i_298] = power_uf(v_initial_param_654_299[v_i_298], v_initial_param_655_300[v_i_298]); 
    }
}
}; 