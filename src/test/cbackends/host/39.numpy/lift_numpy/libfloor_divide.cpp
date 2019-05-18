
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FLOOR_DIV_UF_H
#define FLOOR_DIV_UF_H
; 
float floor_div_uf(float x, float y){
    return floor(x/y);; 
}

#endif
 ; 
void floor_divide(float * v_initial_param_680_292, float * v_initial_param_681_293, float * & v_user_func_687_295, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_687_295 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_291 = 0;(v_i_291 <= (-1 + v_N_0)); (++v_i_291)){
        v_user_func_687_295[v_i_291] = floor_div_uf(v_initial_param_680_292[v_i_291], v_initial_param_681_293[v_i_291]); 
    }
}
}; 