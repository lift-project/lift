
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef RECIPROCAL_UF_H
#define RECIPROCAL_UF_H
; 
float reciprocal_uf(float x){
    return 1.0f/x; 
}

#endif
 ; 
void reciprocal(float * v_initial_param_603_258, float * & v_user_func_605_259, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_605_259 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_257 = 0;(v_i_257 <= (-1 + v_N_0)); (++v_i_257)){
        v_user_func_605_259[v_i_257] = reciprocal_uf(v_initial_param_603_258[v_i_257]); 
    }
}
}; 