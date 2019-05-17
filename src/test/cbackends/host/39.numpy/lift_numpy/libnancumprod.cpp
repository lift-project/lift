
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
void nancumprod(float * v_initial_param_304_165, float * & v_user_func_307_166, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_307_166 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_314 = 1.0f;
    for (int v_i_164 = 0;(v_i_164 <= (-1 + v_N_0)); (++v_i_164)){
        scan_acc_314 = prod2_uf(scan_acc_314, v_initial_param_304_165[v_i_164]); 
        v_user_func_307_166[v_i_164] = scan_acc_314; 
    }
}
}; 