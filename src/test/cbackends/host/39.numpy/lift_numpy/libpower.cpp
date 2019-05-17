
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
void power(float * v_initial_param_647_272, float * v_initial_param_648_273, float * & v_user_func_654_275, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_654_275 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_271 = 0;(v_i_271 <= (-1 + v_N_0)); (++v_i_271)){
        v_user_func_654_275[v_i_271] = power_uf(v_initial_param_647_272[v_i_271], v_initial_param_648_273[v_i_271]); 
    }
}
}; 