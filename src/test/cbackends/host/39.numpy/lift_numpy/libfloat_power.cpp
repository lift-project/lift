
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
void float_power(float * v_initial_param_698_333, float * v_initial_param_699_334, float * & v_user_func_705_336, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_705_336 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_332 = 0;(v_i_332 <= (-1 + v_N_0)); (++v_i_332)){
        v_user_func_705_336[v_i_332] = power_uf(v_initial_param_698_333[v_i_332], v_initial_param_699_334[v_i_332]); 
    }
}
}; 