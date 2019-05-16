
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
void exp2(float * v_initial_param_452_193, float * & v_user_func_454_194, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_454_194 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_192 = 0;(v_i_192 <= (-1 + v_N_0)); (++v_i_192)){
        v_user_func_454_194[v_i_192] = exp2_uf(v_initial_param_452_193[v_i_192]); 
    }
}
}; 