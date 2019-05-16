
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
void floor(float * v_initial_param_252_131, float * & v_user_func_254_132, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_254_132 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_130 = 0;(v_i_130 <= (-1 + v_N_0)); (++v_i_130)){
        v_user_func_254_132[v_i_130] = floor_uf(v_initial_param_252_131[v_i_130]); 
    }
}
}; 