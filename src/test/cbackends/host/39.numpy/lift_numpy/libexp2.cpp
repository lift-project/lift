
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef EXP2_UF_H
#define EXP2_UF_H
; 
float exp2_uf(float x){
    return pow(2,x) ;; 
}

#endif
 ; 
void exp2(float * v_initial_param_457_198, float * & v_user_func_459_199, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_459_199 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_197 = 0;(v_i_197 <= (-1 + v_N_0)); (++v_i_197)){
        v_user_func_459_199[v_i_197] = exp2_uf(v_initial_param_457_198[v_i_197]); 
    }
}
}; 