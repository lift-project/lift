
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
void exp2(float * v_initial_param_3505_590, float * & v_user_func_3507_591, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3507_591 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_589 = 0;(v_i_589 <= (-1 + v_N_352)); (++v_i_589)){
        v_user_func_3507_591[v_i_589] = exp2_uf(v_initial_param_3505_590[v_i_589]); 
    }
}
}; 