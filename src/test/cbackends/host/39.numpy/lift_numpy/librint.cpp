
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef RINT_UF_H
#define RINT_UF_H
; 
float rint_uf(float x){
    return round(x) ;; 
}

#endif
 ; 
void rint(float * v_initial_param_7203_2917, float * & v_user_func_7205_2918, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7205_2918 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2916 = 0;(v_i_2916 <= (-1 + v_N_2763)); (++v_i_2916)){
        v_user_func_7205_2918[v_i_2916] = rint_uf(v_initial_param_7203_2917[v_i_2916]); 
    }
}
}; 