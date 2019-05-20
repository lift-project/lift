
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
void cumsum(float * v_initial_param_353_196, float * & v_user_func_356_197, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_356_197 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_363 = 0.0f;
    for (int v_i_195 = 0;(v_i_195 <= (-1 + v_N_0)); (++v_i_195)){
        scan_acc_363 = add(scan_acc_363, v_initial_param_353_196[v_i_195]); 
        v_user_func_356_197[v_i_195] = scan_acc_363; 
    }
}
}; 