
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef PROD2_UF_H
#define PROD2_UF_H
; 
float prod2_uf(float l, float r){
    { return (l * r); }; 
}

#endif
 ; 
void cumprod(float * v_initial_param_1569_321, float * & v_user_func_1572_322, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1572_322 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_1579 = 1.0f;
    for (int v_i_320 = 0;(v_i_320 <= (-1 + v_N_190)); (++v_i_320)){
        scan_acc_1579 = prod2_uf(scan_acc_1579, v_initial_param_1569_321[v_i_320]); 
        v_user_func_1572_322[v_i_320] = scan_acc_1579; 
    }
}
}; 