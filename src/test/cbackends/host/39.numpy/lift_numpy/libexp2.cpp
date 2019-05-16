
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
void exp2(float * v_initial_param_451_191, float * & v_user_func_453_192, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_453_192 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_190 = 0;(v_i_190 <= (-1 + v_N_0)); (++v_i_190)){
        v_user_func_453_192[v_i_190] = exp2_uf(v_initial_param_451_191[v_i_190]); 
    }
}
}; 