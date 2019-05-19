
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SQRT_UF_H
#define SQRT_UF_H
; 
float sqrt_uf(float x){
    { return sqrt(x); }; 
}

#endif
 ; 
void sqrt(float * v_initial_param_841_362, float * & v_user_func_843_363, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_843_363 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_361 = 0;(v_i_361 <= (-1 + v_N_0)); (++v_i_361)){
        v_user_func_843_363[v_i_361] = sqrt_uf(v_initial_param_841_362[v_i_361]); 
    }
}
}; 