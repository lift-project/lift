
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
void lift_remainder(float * v_initial_param_761_351, float * v_initial_param_762_352, float * & v_user_func_768_354, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_768_354 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_350 = 0;(v_i_350 <= (-1 + v_N_0)); (++v_i_350)){
        v_user_func_768_354[v_i_350] = remainder_uf(v_initial_param_761_351[v_i_350], v_initial_param_762_352[v_i_350]); 
    }
}
}; 