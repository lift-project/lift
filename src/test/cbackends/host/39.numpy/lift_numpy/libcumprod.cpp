
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef PROD2_UF_H
#define PROD2_UF_H
; 
float prod2_uf(float l, float r){
    { return (l * r); }; 
}

#endif
 ; 
void cumprod(float * v_initial_param_282_135, float * & v_user_func_285_136, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_285_136 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_292 = 1.0f;
    for (int v_i_134 = 0;(v_i_134 <= (-1 + v_N_0)); (++v_i_134)){
        scan_acc_292 = prod2_uf(scan_acc_292, v_initial_param_282_135[v_i_134]); 
        v_user_func_285_136[v_i_134] = scan_acc_292; 
    }
}
}; 