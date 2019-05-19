
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
void cumsum(float * v_initial_param_334_182, float * & v_user_func_337_183, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_337_183 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_344 = 0.0f;
    for (int v_i_181 = 0;(v_i_181 <= (-1 + v_N_0)); (++v_i_181)){
        scan_acc_344 = add(scan_acc_344, v_initial_param_334_182[v_i_181]); 
        v_user_func_337_183[v_i_181] = scan_acc_344; 
    }
}
}; 