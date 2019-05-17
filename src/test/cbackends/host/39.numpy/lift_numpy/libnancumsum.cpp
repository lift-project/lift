
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif
 ; 
void nancumsum(float * v_initial_param_318_170, float * & v_user_func_321_171, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_321_171 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_328 = 0.0f;
    for (int v_i_169 = 0;(v_i_169 <= (-1 + v_N_0)); (++v_i_169)){
        scan_acc_328 = add(scan_acc_328, v_initial_param_318_170[v_i_169]); 
        v_user_func_321_171[v_i_169] = scan_acc_328; 
    }
}
}; 