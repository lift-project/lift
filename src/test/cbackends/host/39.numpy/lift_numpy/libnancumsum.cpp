
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
void nancumsum(float * v_initial_param_320_172, float * & v_user_func_323_173, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_323_173 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_330 = 0.0f;
    for (int v_i_171 = 0;(v_i_171 <= (-1 + v_N_0)); (++v_i_171)){
        scan_acc_330 = add(scan_acc_330, v_initial_param_320_172[v_i_171]); 
        v_user_func_323_173[v_i_171] = scan_acc_330; 
    }
}
}; 