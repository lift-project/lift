
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef CEIL_UF_H
#define CEIL_UF_H
; 
float ceil_uf(float x){
    return ceil(x);; 
}

#endif
 ; 
void ceil(float * v_initial_param_1531_303, float * & v_user_func_1533_304, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1533_304 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_302 = 0;(v_i_302 <= (-1 + v_N_190)); (++v_i_302)){
        v_user_func_1533_304[v_i_302] = ceil_uf(v_initial_param_1531_303[v_i_302]); 
    }
}
}; 