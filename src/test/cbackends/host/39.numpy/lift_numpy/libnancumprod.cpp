
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
void nancumprod(float * v_initial_param_296_156, float * & v_user_func_299_157, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_299_157 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_306 = 1.0f;
    for (int v_i_155 = 0;(v_i_155 <= (-1 + v_N_0)); (++v_i_155)){
        scan_acc_306 = prod2_uf(scan_acc_306, v_initial_param_296_156[v_i_155]); 
        v_user_func_299_157[v_i_155] = scan_acc_306; 
    }
}
}; 