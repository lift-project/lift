
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
void cumsum(float * v_initial_param_318_164, float * & v_user_func_321_165, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_321_165 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_328 = 0.0f;
    for (int v_i_163 = 0;(v_i_163 <= (-1 + v_N_0)); (++v_i_163)){
        scan_acc_328 = add(scan_acc_328, v_initial_param_318_164[v_i_163]); 
        v_user_func_321_165[v_i_163] = scan_acc_328; 
    }
}
}; 