
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
void ceil(float * v_initial_param_257_131, float * & v_user_func_259_132, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_259_132 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_130 = 0;(v_i_130 <= (-1 + v_N_0)); (++v_i_130)){
        v_user_func_259_132[v_i_130] = ceil_uf(v_initial_param_257_131[v_i_130]); 
    }
}
}; 