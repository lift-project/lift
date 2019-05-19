
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MAXIMUM_UF_H
#define MAXIMUM_UF_H
; 
float maximum_uf(float x, float y){
    { return max(x,y); }; 
}

#endif
 ; 
void maximum(float * v_initial_param_876_380, float * v_initial_param_877_381, float * & v_user_func_883_383, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_883_383 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_379 = 0;(v_i_379 <= (-1 + v_N_0)); (++v_i_379)){
        v_user_func_883_383[v_i_379] = maximum_uf(v_initial_param_876_380[v_i_379], v_initial_param_877_381[v_i_379]); 
    }
}
}; 