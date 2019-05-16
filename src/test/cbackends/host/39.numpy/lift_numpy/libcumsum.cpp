
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
void cumsum(float * v_initial_param_304_148, float * & v_user_func_307_149, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_307_149 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_314 = 0.0f;
    for (int v_i_147 = 0;(v_i_147 <= (-1 + v_N_0)); (++v_i_147)){
        scan_acc_314 = add(scan_acc_314, v_initial_param_304_148[v_i_147]); 
        v_user_func_307_149[v_i_147] = scan_acc_314; 
    }
}
}; 