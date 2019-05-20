
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
void around(float * v_initial_param_256_148, float * & v_user_func_258_149, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_258_149 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_147 = 0;(v_i_147 <= (-1 + v_N_0)); (++v_i_147)){
        v_user_func_258_149[v_i_147] = round_uf(v_initial_param_256_148[v_i_147]); 
    }
}
}; 