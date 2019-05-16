
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
void nancumsum(float * v_initial_param_307_157, float * & v_user_func_310_158, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_310_158 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_317 = 0.0f;
    for (int v_i_156 = 0;(v_i_156 <= (-1 + v_N_0)); (++v_i_156)){
        scan_acc_317 = add(scan_acc_317, v_initial_param_307_157[v_i_156]); 
        v_user_func_310_158[v_i_156] = scan_acc_317; 
    }
}
}; 