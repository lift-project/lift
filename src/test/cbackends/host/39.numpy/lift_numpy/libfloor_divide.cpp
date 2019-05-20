
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
void floor_divide(float * v_initial_param_713_322, float * v_initial_param_714_323, float * & v_user_func_720_325, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_720_325 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_321 = 0;(v_i_321 <= (-1 + v_N_0)); (++v_i_321)){
        v_user_func_720_325[v_i_321] = floor_div_uf(v_initial_param_713_322[v_i_321], v_initial_param_714_323[v_i_321]); 
    }
}
}; 