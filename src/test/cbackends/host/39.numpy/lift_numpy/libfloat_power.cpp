
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
void float_power(float * v_initial_param_647_292, float * v_initial_param_648_293, float * & v_user_func_654_295, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_654_295 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_291 = 0;(v_i_291 <= (-1 + v_N_0)); (++v_i_291)){
        v_user_func_654_295[v_i_291] = power_uf(v_initial_param_647_292[v_i_291], v_initial_param_648_293[v_i_291]); 
    }
}
}; 