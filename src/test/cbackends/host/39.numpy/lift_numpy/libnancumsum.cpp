
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
void nancumsum(float * v_initial_param_3362_558, float * & v_user_func_3365_559, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3365_559 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_3372 = 0.0f;
    for (int v_i_557 = 0;(v_i_557 <= (-1 + v_N_352)); (++v_i_557)){
        scan_acc_3372 = add(scan_acc_3372, v_initial_param_3362_558[v_i_557]); 
        v_user_func_3365_559[v_i_557] = scan_acc_3372; 
    }
}
}; 