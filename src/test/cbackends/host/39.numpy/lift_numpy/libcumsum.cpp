
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
void cumsum(float * v_initial_param_317_163, float * & v_user_func_320_164, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_320_164 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_327 = 0.0f;
    for (int v_i_162 = 0;(v_i_162 <= (-1 + v_N_0)); (++v_i_162)){
        scan_acc_327 = add(scan_acc_327, v_initial_param_317_163[v_i_162]); 
        v_user_func_320_164[v_i_162] = scan_acc_327; 
    }
}
}; 