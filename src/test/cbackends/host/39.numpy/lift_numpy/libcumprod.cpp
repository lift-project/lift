
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
void cumprod(float * v_initial_param_341_193, float * & v_user_func_344_194, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_344_194 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_351 = 1.0f;
    for (int v_i_192 = 0;(v_i_192 <= (-1 + v_N_0)); (++v_i_192)){
        scan_acc_351 = prod2_uf(scan_acc_351, v_initial_param_341_193[v_i_192]); 
        v_user_func_344_194[v_i_192] = scan_acc_351; 
    }
}
}; 