
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
void nancumsum(float * v_initial_param_300_150, float * & v_user_func_303_151, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_303_151 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_310 = 0.0f;
    for (int v_i_149 = 0;(v_i_149 <= (-1 + v_N_0)); (++v_i_149)){
        scan_acc_310 = add(scan_acc_310, v_initial_param_300_150[v_i_149]); 
        v_user_func_303_151[v_i_149] = scan_acc_310; 
    }
}
}; 