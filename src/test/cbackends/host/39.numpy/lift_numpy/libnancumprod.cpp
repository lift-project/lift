
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
void nancumprod(float * v_initial_param_289_148, float * & v_user_func_292_149, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_292_149 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element scanned sequentially
    float scan_acc_299 = 1.0f;
    for (int v_i_147 = 0;(v_i_147 <= (-1 + v_N_0)); (++v_i_147)){
        scan_acc_299 = prod2_uf(scan_acc_299, v_initial_param_289_148[v_i_147]); 
        v_user_func_292_149[v_i_147] = scan_acc_299; 
    }
}
}; 