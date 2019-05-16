
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIFF2_H
#define DIFF2_H
; 
float diff2(float l, float r){
    { return (r - l); }; 
}

#endif
 ; 
void ediff1d(float * v_initial_param_307_153, float * & v_user_func_310_154, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_310_154 = reinterpret_cast<float *>(malloc(((-1 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_151 = 0;(v_i_151 <= (-2 + v_N_0)); (++v_i_151)){
        // For each element reduced sequentially
        v_user_func_310_154[v_i_151] = 0.0f; 
        for (int v_i_152 = 0;(v_i_152 <= 1); (++v_i_152)){
            v_user_func_310_154[v_i_151] = diff2(v_user_func_310_154[v_i_151], v_initial_param_307_153[(v_i_151 + v_i_152)]); 
        }
    }
}
}; 