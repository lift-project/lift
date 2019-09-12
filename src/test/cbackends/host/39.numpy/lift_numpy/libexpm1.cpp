
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef EXPM1_UF_H
#define EXPM1_UF_H
; 
float expm1_uf(float x){
    return exp(x) - 1 ;; 
}

#endif
 ; 
void expm1(float * v_initial_param_7442_2998, float * & v_user_func_7444_2999, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7444_2999 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2997 = 0;(v_i_2997 <= (-1 + v_N_2763)); (++v_i_2997)){
        v_user_func_7444_2999[v_i_2997] = expm1_uf(v_initial_param_7442_2998[v_i_2997]); 
    }
}
}; 