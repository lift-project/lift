
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
void cumsum(float * v_initial_param_291_130, float * & v_user_func_294_131, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_294_131 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_301 = 0.0f;
    for (int v_i_129 = 0;(v_i_129 <= (-1 + v_N_0)); (++v_i_129)){
        scan_acc_301 = add(scan_acc_301, v_initial_param_291_130[v_i_129]); 
        v_user_func_294_131[v_i_129] = scan_acc_301; 
    }
}
}; 