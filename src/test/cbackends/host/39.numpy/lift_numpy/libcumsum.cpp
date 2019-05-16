
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
void cumsum(float * v_initial_param_301_145, float * & v_user_func_304_146, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_304_146 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_311 = 0.0f;
    for (int v_i_144 = 0;(v_i_144 <= (-1 + v_N_0)); (++v_i_144)){
        scan_acc_311 = add(scan_acc_311, v_initial_param_301_145[v_i_144]); 
        v_user_func_304_146[v_i_144] = scan_acc_311; 
    }
}
}; 