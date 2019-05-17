
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
void nancumprod(float * v_initial_param_301_162, float * & v_user_func_304_163, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_304_163 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_311 = 1.0f;
    for (int v_i_161 = 0;(v_i_161 <= (-1 + v_N_0)); (++v_i_161)){
        scan_acc_311 = prod2_uf(scan_acc_311, v_initial_param_301_162[v_i_161]); 
        v_user_func_304_163[v_i_161] = scan_acc_311; 
    }
}
}; 