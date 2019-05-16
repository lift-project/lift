
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
void cumsum(float * v_initial_param_300_144, float * & v_user_func_303_145, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_303_145 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_310 = 0.0f;
    for (int v_i_143 = 0;(v_i_143 <= (-1 + v_N_0)); (++v_i_143)){
        scan_acc_310 = add(scan_acc_310, v_initial_param_300_144[v_i_143]); 
        v_user_func_303_145[v_i_143] = scan_acc_310; 
    }
}
}; 