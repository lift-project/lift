
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef EXP_UF_H
#define EXP_UF_H
; 
float exp_uf(float x){
    return exp(x) ;; 
}

#endif
 ; 
void lift_exp(float * v_initial_param_3491_584, float * & v_user_func_3493_585, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3493_585 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_583 = 0;(v_i_583 <= (-1 + v_N_352)); (++v_i_583)){
        v_user_func_3493_585[v_i_583] = exp_uf(v_initial_param_3491_584[v_i_583]); 
    }
}
}; 