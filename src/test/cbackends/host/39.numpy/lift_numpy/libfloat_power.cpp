
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
void float_power(float * v_initial_param_666_311, float * v_initial_param_667_312, float * & v_user_func_673_314, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_673_314 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_310 = 0;(v_i_310 <= (-1 + v_N_0)); (++v_i_310)){
        v_user_func_673_314[v_i_310] = power_uf(v_initial_param_666_311[v_i_310], v_initial_param_667_312[v_i_310]); 
    }
}
}; 