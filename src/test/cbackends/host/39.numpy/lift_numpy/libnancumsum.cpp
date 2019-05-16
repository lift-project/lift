
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
void nancumsum(float * v_initial_param_303_153, float * & v_user_func_306_154, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_306_154 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_313 = 0.0f;
    for (int v_i_152 = 0;(v_i_152 <= (-1 + v_N_0)); (++v_i_152)){
        scan_acc_313 = add(scan_acc_313, v_initial_param_303_153[v_i_152]); 
        v_user_func_306_154[v_i_152] = scan_acc_313; 
    }
}
}; 