
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ROUND_UF_H
#define ROUND_UF_H
; 
float round_uf(float x){
    return ( ((int) ceil(x)) % 2 == 0 ? ceil(x) : ceil(x) -1) ;; 
}

#endif
 ; 
void around(float * v_initial_param_215_101, float * & v_user_func_217_102, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_217_102 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_100 = 0;(v_i_100 <= (-1 + v_N_0)); (++v_i_100)){
        v_user_func_217_102[v_i_100] = round_uf(v_initial_param_215_101[v_i_100]); 
    }
}
}; 