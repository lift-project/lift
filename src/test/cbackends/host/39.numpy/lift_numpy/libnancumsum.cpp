
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
void nancumsum(float * v_initial_param_334_186, float * & v_user_func_337_187, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_337_187 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_344 = 0.0f;
    for (int v_i_185 = 0;(v_i_185 <= (-1 + v_N_0)); (++v_i_185)){
        scan_acc_344 = add(scan_acc_344, v_initial_param_334_186[v_i_185]); 
        v_user_func_337_187[v_i_185] = scan_acc_344; 
    }
}
}; 