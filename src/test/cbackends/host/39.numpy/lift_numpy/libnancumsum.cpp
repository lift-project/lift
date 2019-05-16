
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
void nancumsum(float * v_initial_param_304_154, float * & v_user_func_307_155, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_307_155 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_314 = 0.0f;
    for (int v_i_153 = 0;(v_i_153 <= (-1 + v_N_0)); (++v_i_153)){
        scan_acc_314 = add(scan_acc_314, v_initial_param_304_154[v_i_153]); 
        v_user_func_307_155[v_i_153] = scan_acc_314; 
    }
}
}; 