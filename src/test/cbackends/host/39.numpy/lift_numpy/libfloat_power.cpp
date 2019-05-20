
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
void float_power(float * v_initial_param_685_327, float * v_initial_param_686_328, float * & v_user_func_692_330, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_692_330 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_326 = 0;(v_i_326 <= (-1 + v_N_0)); (++v_i_326)){
        v_user_func_692_330[v_i_326] = power_uf(v_initial_param_685_327[v_i_326], v_initial_param_686_328[v_i_326]); 
    }
}
}; 