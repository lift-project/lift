
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
void nancumsum(float * v_initial_param_334_188, float * & v_user_func_337_189, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_337_189 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_344 = 0.0f;
    for (int v_i_187 = 0;(v_i_187 <= (-1 + v_N_0)); (++v_i_187)){
        scan_acc_344 = add(scan_acc_344, v_initial_param_334_188[v_i_187]); 
        v_user_func_337_189[v_i_187] = scan_acc_344; 
    }
}
}; 