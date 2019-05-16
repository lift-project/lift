
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
void cumsum(float * v_initial_param_302_146, float * & v_user_func_305_147, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_305_147 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_312 = 0.0f;
    for (int v_i_145 = 0;(v_i_145 <= (-1 + v_N_0)); (++v_i_145)){
        scan_acc_312 = add(scan_acc_312, v_initial_param_302_146[v_i_145]); 
        v_user_func_305_147[v_i_145] = scan_acc_312; 
    }
}
}; 