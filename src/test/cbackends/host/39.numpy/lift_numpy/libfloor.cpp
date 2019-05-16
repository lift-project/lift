
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
void floor(float * v_initial_param_249_126, float * & v_user_func_251_127, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_251_127 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_125 = 0;(v_i_125 <= (-1 + v_N_0)); (++v_i_125)){
        v_user_func_251_127[v_i_125] = floor_uf(v_initial_param_249_126[v_i_125]); 
    }
}
}; 