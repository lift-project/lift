
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
void expm1(float * v_initial_param_3498_587, float * & v_user_func_3500_588, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3500_588 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_586 = 0;(v_i_586 <= (-1 + v_N_352)); (++v_i_586)){
        v_user_func_3500_588[v_i_586] = expm1_uf(v_initial_param_3498_587[v_i_586]); 
    }
}
}; 