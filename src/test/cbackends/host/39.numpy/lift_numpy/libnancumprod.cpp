
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
void nancumprod(float * v_initial_param_354_205, float * & v_user_func_357_206, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_357_206 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_364 = 1.0f;
    for (int v_i_204 = 0;(v_i_204 <= (-1 + v_N_0)); (++v_i_204)){
        scan_acc_364 = prod2_uf(scan_acc_364, v_initial_param_354_205[v_i_204]); 
        v_user_func_357_206[v_i_204] = scan_acc_364; 
    }
}
}; 