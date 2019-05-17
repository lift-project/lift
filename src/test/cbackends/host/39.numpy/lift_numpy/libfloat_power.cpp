
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
void float_power(float * v_initial_param_650_295, float * v_initial_param_651_296, float * & v_user_func_657_298, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_657_298 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_294 = 0;(v_i_294 <= (-1 + v_N_0)); (++v_i_294)){
        v_user_func_657_298[v_i_294] = power_uf(v_initial_param_650_295[v_i_294], v_initial_param_651_296[v_i_294]); 
    }
}
}; 