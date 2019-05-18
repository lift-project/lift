
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SIGNBIT_UF_H
#define SIGNBIT_UF_H
; 
float signbit_uf(float x){
    return x<0? 1:0 ;; 
}

#endif
 ; 
void signbit(float * v_initial_param_550_235, float * & v_user_func_552_236, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_552_236 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_234 = 0;(v_i_234 <= (-1 + v_N_0)); (++v_i_234)){
        v_user_func_552_236[v_i_234] = signbit_uf(v_initial_param_550_235[v_i_234]); 
    }
}
}; 