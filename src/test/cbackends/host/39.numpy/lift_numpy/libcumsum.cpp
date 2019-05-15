
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
void cumsum(float * v_initial_param_291_132, float * & v_user_func_294_133, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_294_133 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_301 = 0.0f;
    for (int v_i_131 = 0;(v_i_131 <= (-1 + v_N_0)); (++v_i_131)){
        scan_acc_301 = add(scan_acc_301, v_initial_param_291_132[v_i_131]); 
        v_user_func_294_133[v_i_131] = scan_acc_301; 
    }
}
}; 