
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
void nancumsum(float * v_initial_param_295_145, float * & v_user_func_298_146, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_298_146 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_305 = 0.0f;
    for (int v_i_144 = 0;(v_i_144 <= (-1 + v_N_0)); (++v_i_144)){
        scan_acc_305 = add(scan_acc_305, v_initial_param_295_145[v_i_144]); 
        v_user_func_298_146[v_i_144] = scan_acc_305; 
    }
}
}; 