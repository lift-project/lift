
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
void cumsum(float * v_initial_param_307_151, float * & v_user_func_310_152, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_310_152 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_317 = 0.0f;
    for (int v_i_150 = 0;(v_i_150 <= (-1 + v_N_0)); (++v_i_150)){
        scan_acc_317 = add(scan_acc_317, v_initial_param_307_151[v_i_150]); 
        v_user_func_310_152[v_i_150] = scan_acc_317; 
    }
}
}; 