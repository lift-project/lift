
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
void nancumsum(float * v_initial_param_309_161, float * & v_user_func_312_162, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_312_162 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_319 = 0.0f;
    for (int v_i_160 = 0;(v_i_160 <= (-1 + v_N_0)); (++v_i_160)){
        scan_acc_319 = add(scan_acc_319, v_initial_param_309_161[v_i_160]); 
        v_user_func_312_162[v_i_160] = scan_acc_319; 
    }
}
}; 