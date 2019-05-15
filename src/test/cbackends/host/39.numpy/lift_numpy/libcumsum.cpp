
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ADD_H
#define ADD_H
; 
float add(float l, float r){
    { return (l + r); }; 
}

#endif
 ; 
void cumsum(float * v_initial_param_1581_324, float * & v_user_func_1584_325, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1584_325 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_1591 = 0.0f;
    for (int v_i_323 = 0;(v_i_323 <= (-1 + v_N_190)); (++v_i_323)){
        scan_acc_1591 = add(scan_acc_1591, v_initial_param_1581_324[v_i_323]); 
        v_user_func_1584_325[v_i_323] = scan_acc_1591; 
    }
}
}; 