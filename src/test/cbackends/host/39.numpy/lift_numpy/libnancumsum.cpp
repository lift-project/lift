
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
void nancumsum(float * v_initial_param_353_202, float * & v_user_func_356_203, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_356_203 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_363 = 0.0f;
    for (int v_i_201 = 0;(v_i_201 <= (-1 + v_N_0)); (++v_i_201)){
        scan_acc_363 = add(scan_acc_363, v_initial_param_353_202[v_i_201]); 
        v_user_func_356_203[v_i_201] = scan_acc_363; 
    }
}
}; 