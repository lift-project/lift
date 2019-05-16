
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
void nancumsum(float * v_initial_param_294_144, float * & v_user_func_297_145, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_297_145 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_304 = 0.0f;
    for (int v_i_143 = 0;(v_i_143 <= (-1 + v_N_0)); (++v_i_143)){
        scan_acc_304 = add(scan_acc_304, v_initial_param_294_144[v_i_143]); 
        v_user_func_297_145[v_i_143] = scan_acc_304; 
    }
}
}; 