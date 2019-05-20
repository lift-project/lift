
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
void floor_divide(float * v_initial_param_726_328, float * v_initial_param_727_329, float * & v_user_func_733_331, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_733_331 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_327 = 0;(v_i_327 <= (-1 + v_N_0)); (++v_i_327)){
        v_user_func_733_331[v_i_327] = floor_div_uf(v_initial_param_726_328[v_i_327], v_initial_param_727_329[v_i_327]); 
    }
}
}; 