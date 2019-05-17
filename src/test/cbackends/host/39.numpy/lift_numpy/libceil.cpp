
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
void ceil(float * v_initial_param_266_141, float * & v_user_func_268_142, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_268_142 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_140 = 0;(v_i_140 <= (-1 + v_N_0)); (++v_i_140)){
        v_user_func_268_142[v_i_140] = ceil_uf(v_initial_param_266_141[v_i_140]); 
    }
}
}; 