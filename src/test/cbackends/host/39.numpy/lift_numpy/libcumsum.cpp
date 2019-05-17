
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
void cumsum(float * v_initial_param_312_158, float * & v_user_func_315_159, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_315_159 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_322 = 0.0f;
    for (int v_i_157 = 0;(v_i_157 <= (-1 + v_N_0)); (++v_i_157)){
        scan_acc_322 = add(scan_acc_322, v_initial_param_312_158[v_i_157]); 
        v_user_func_315_159[v_i_157] = scan_acc_322; 
    }
}
}; 