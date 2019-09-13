
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FIX_UF_H
#define FIX_UF_H
; 
float fix_uf(float x){
    return trunc(x) ;; 
}

#endif
 ; 
void fix(float * v_initial_param_7210_2920, float * & v_user_func_7212_2921, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7212_2921 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2919 = 0;(v_i_2919 <= (-1 + v_N_2763)); (++v_i_2919)){
        v_user_func_7212_2921[v_i_2919] = fix_uf(v_initial_param_7210_2920[v_i_2919]); 
    }
}
}; 