
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
void nancumsum(float * v_initial_param_319_171, float * & v_user_func_322_172, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_322_172 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_329 = 0.0f;
    for (int v_i_170 = 0;(v_i_170 <= (-1 + v_N_0)); (++v_i_170)){
        scan_acc_329 = add(scan_acc_329, v_initial_param_319_171[v_i_170]); 
        v_user_func_322_172[v_i_170] = scan_acc_329; 
    }
}
}; 