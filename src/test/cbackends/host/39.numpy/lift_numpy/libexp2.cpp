
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
void exp2(float * v_initial_param_7449_3001, float * & v_user_func_7451_3002, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7451_3002 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3000 = 0;(v_i_3000 <= (-1 + v_N_2763)); (++v_i_3000)){
        v_user_func_7451_3002[v_i_3000] = exp2_uf(v_initial_param_7449_3001[v_i_3000]); 
    }
}
}; 