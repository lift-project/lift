
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
void nancumsum(float * v_initial_param_313_165, float * & v_user_func_316_166, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_316_166 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_323 = 0.0f;
    for (int v_i_164 = 0;(v_i_164 <= (-1 + v_N_0)); (++v_i_164)){
        scan_acc_323 = add(scan_acc_323, v_initial_param_313_165[v_i_164]); 
        v_user_func_316_166[v_i_164] = scan_acc_323; 
    }
}
}; 