
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef PROD2_UF_H
#define PROD2_UF_H
; 
float prod2_uf(float l, float r){
    { return (l * r); }; 
}

#endif
 ; 
void cumprod(float * v_initial_param_284_137, float * & v_user_func_287_138, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_287_138 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_294 = 1.0f;
    for (int v_i_136 = 0;(v_i_136 <= (-1 + v_N_0)); (++v_i_136)){
        scan_acc_294 = prod2_uf(scan_acc_294, v_initial_param_284_137[v_i_136]); 
        v_user_func_287_138[v_i_136] = scan_acc_294; 
    }
}
}; 