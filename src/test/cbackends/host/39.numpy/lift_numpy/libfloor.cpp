
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FLOOR_UF_H
#define FLOOR_UF_H
; 
float floor_uf(float x){
    return floor(x);; 
}

#endif
 ; 
void floor(float * v_initial_param_245_122, float * & v_user_func_247_123, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_247_123 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_121 = 0;(v_i_121 <= (-1 + v_N_0)); (++v_i_121)){
        v_user_func_247_123[v_i_121] = floor_uf(v_initial_param_245_122[v_i_121]); 
    }
}
}; 