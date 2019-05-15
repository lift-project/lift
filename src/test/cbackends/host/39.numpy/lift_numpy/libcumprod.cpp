
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef PROD2_H
#define PROD2_H
; 
float prod2(float l, float r){
    { return (l * r); }; 
}

#endif; 
void cumprod(float * v_initial_param_279_129, float * & v_user_func_282_130, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_282_130 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_289 = 1.0f;
    for (int v_i_128 = 0;(v_i_128 <= (-1 + v_N_0)); (++v_i_128)){
        scan_acc_289 = prod2(scan_acc_289, v_initial_param_279_129[v_i_128]); 
        v_user_func_282_130[v_i_128] = scan_acc_289; 
    }
}
}; 