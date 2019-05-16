
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
void nancumsum(float * v_initial_param_302_152, float * & v_user_func_305_153, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_305_153 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_312 = 0.0f;
    for (int v_i_151 = 0;(v_i_151 <= (-1 + v_N_0)); (++v_i_151)){
        scan_acc_312 = add(scan_acc_312, v_initial_param_302_152[v_i_151]); 
        v_user_func_305_153[v_i_151] = scan_acc_312; 
    }
}
}; 