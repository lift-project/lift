
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
void ceil(float * v_initial_param_284_159, float * & v_user_func_286_160, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_286_160 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_158 = 0;(v_i_158 <= (-1 + v_N_0)); (++v_i_158)){
        v_user_func_286_160[v_i_158] = ceil_uf(v_initial_param_284_159[v_i_158]); 
    }
}
}; 