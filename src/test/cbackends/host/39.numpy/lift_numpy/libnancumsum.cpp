
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
void nancumsum(float * v_initial_param_323_177, float * & v_user_func_326_178, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_326_178 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_333 = 0.0f;
    for (int v_i_176 = 0;(v_i_176 <= (-1 + v_N_0)); (++v_i_176)){
        scan_acc_333 = add(scan_acc_333, v_initial_param_323_177[v_i_176]); 
        v_user_func_326_178[v_i_176] = scan_acc_333; 
    }
}
}; 