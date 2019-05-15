
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif; 
void cumsum(float * v_initial_param_292_133, float * & v_user_func_295_134, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_295_134 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_302 = 0.0f;
    for (int v_i_132 = 0;(v_i_132 <= (-1 + v_N_0)); (++v_i_132)){
        scan_acc_302 = add(scan_acc_302, v_initial_param_292_133[v_i_132]); 
        v_user_func_295_134[v_i_132] = scan_acc_302; 
    }
}
}; 