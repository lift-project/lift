
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
void cumsum(float * v_initial_param_334_180, float * & v_user_func_337_181, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_337_181 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_344 = 0.0f;
    for (int v_i_179 = 0;(v_i_179 <= (-1 + v_N_0)); (++v_i_179)){
        scan_acc_344 = add(scan_acc_344, v_initial_param_334_180[v_i_179]); 
        v_user_func_337_181[v_i_179] = scan_acc_344; 
    }
}
}; 