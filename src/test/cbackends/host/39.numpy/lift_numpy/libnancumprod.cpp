
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
void nancumprod(float * v_initial_param_7294_2966, float * & v_user_func_7297_2967, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7297_2967 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_7304 = 1.0f;
    for (int v_i_2965 = 0;(v_i_2965 <= (-1 + v_N_2763)); (++v_i_2965)){
        scan_acc_7304 = prod2_uf(scan_acc_7304, v_initial_param_7294_2966[v_i_2965]); 
        v_user_func_7297_2967[v_i_2965] = scan_acc_7304; 
    }
}
}; 