
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
void cumprod(float * v_initial_param_3350_549, float * & v_user_func_3353_550, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3353_550 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_3360 = 1.0f;
    for (int v_i_548 = 0;(v_i_548 <= (-1 + v_N_352)); (++v_i_548)){
        scan_acc_3360 = prod2_uf(scan_acc_3360, v_initial_param_3350_549[v_i_548]); 
        v_user_func_3353_550[v_i_548] = scan_acc_3360; 
    }
}
}; 