
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
void around(float * v_initial_param_256_146, float * & v_user_func_258_147, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_258_147 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_145 = 0;(v_i_145 <= (-1 + v_N_0)); (++v_i_145)){
        v_user_func_258_147[v_i_145] = round_uf(v_initial_param_256_146[v_i_145]); 
    }
}
}; 