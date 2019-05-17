
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
void nancumsum(float * v_initial_param_317_169, float * & v_user_func_320_170, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_320_170 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_327 = 0.0f;
    for (int v_i_168 = 0;(v_i_168 <= (-1 + v_N_0)); (++v_i_168)){
        scan_acc_327 = add(scan_acc_327, v_initial_param_317_169[v_i_168]); 
        v_user_func_320_170[v_i_168] = scan_acc_327; 
    }
}
}; 