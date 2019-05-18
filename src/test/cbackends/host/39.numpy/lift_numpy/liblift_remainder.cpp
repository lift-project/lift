
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef REMAINDER_UF_H
#define REMAINDER_UF_H
; 
float remainder_uf(float x, float y){
    if(x>=0) return x - floor(x/y)*y; else return x - round(x/y)*y; 
}

#endif
 ; 
void lift_remainder(float * v_initial_param_717_317, float * v_initial_param_718_318, float * & v_user_func_724_320, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_724_320 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_316 = 0;(v_i_316 <= (-1 + v_N_0)); (++v_i_316)){
        v_user_func_724_320[v_i_316] = remainder_uf(v_initial_param_717_317[v_i_316], v_initial_param_718_318[v_i_316]); 
    }
}
}; 