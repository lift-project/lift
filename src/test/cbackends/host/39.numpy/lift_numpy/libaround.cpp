
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ROUND_UF_H
#define ROUND_UF_H
; 
float round_uf(float x){
    return ( ((int) ceil(x)) % 2 == 0 ? ceil(x) : ceil(x) -1) ;; 
}

#endif
 ; 
void around(float * v_initial_param_1503_288, float * & v_user_func_1505_289, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1505_289 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_287 = 0;(v_i_287 <= (-1 + v_N_190)); (++v_i_287)){
        v_user_func_1505_289[v_i_287] = round_uf(v_initial_param_1503_288[v_i_287]); 
    }
}
}; 