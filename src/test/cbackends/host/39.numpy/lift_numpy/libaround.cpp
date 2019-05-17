
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
void around(float * v_initial_param_233_121, float * & v_user_func_235_122, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_235_122 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_120 = 0;(v_i_120 <= (-1 + v_N_0)); (++v_i_120)){
        v_user_func_235_122[v_i_120] = round_uf(v_initial_param_233_121[v_i_120]); 
    }
}
}; 