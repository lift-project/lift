
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIVIDE_UF_H
#define DIVIDE_UF_H
; 
float divide_uf(float x, float y){
    return x / y;; 
}

#endif
 ; 
void true_divide(float * v_initial_param_636_285, float * v_initial_param_637_286, float * & v_user_func_643_288, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_643_288 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_284 = 0;(v_i_284 <= (-1 + v_N_0)); (++v_i_284)){
        v_user_func_643_288[v_i_284] = divide_uf(v_initial_param_636_285[v_i_284], v_initial_param_637_286[v_i_284]); 
    }
}
}; 