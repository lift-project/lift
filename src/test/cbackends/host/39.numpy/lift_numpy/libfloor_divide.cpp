
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
void floor_divide(float * v_initial_param_694_306, float * v_initial_param_695_307, float * & v_user_func_701_309, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_701_309 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_305 = 0;(v_i_305 <= (-1 + v_N_0)); (++v_i_305)){
        v_user_func_701_309[v_i_305] = floor_div_uf(v_initial_param_694_306[v_i_305], v_initial_param_695_307[v_i_305]); 
    }
}
}; 