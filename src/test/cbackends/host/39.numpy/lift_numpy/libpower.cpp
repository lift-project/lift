
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
void power(float * v_initial_param_655_282, float * v_initial_param_656_283, float * & v_user_func_662_285, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_662_285 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_281 = 0;(v_i_281 <= (-1 + v_N_0)); (++v_i_281)){
        v_user_func_662_285[v_i_281] = power_uf(v_initial_param_655_282[v_i_281], v_initial_param_656_283[v_i_281]); 
    }
}
}; 