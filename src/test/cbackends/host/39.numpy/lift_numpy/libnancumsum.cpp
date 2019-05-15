
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
void nancumsum(float * v_initial_param_291_138, float * & v_user_func_294_139, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_294_139 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_301 = 0.0f;
    for (int v_i_137 = 0;(v_i_137 <= (-1 + v_N_0)); (++v_i_137)){
        scan_acc_301 = add(scan_acc_301, v_initial_param_291_138[v_i_137]); 
        v_user_func_294_139[v_i_137] = scan_acc_301; 
    }
}
}; 