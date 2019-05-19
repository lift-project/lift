
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
void floor_divide(float * v_initial_param_694_308, float * v_initial_param_695_309, float * & v_user_func_701_311, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_701_311 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_307 = 0;(v_i_307 <= (-1 + v_N_0)); (++v_i_307)){
        v_user_func_701_311[v_i_307] = floor_div_uf(v_initial_param_694_308[v_i_307], v_initial_param_695_309[v_i_307]); 
    }
}
}; 