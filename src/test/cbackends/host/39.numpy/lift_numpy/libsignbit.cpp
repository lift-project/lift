
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
void signbit(float * v_initial_param_534_217, float * & v_user_func_536_218, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_536_218 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_216 = 0;(v_i_216 <= (-1 + v_N_0)); (++v_i_216)){
        v_user_func_536_218[v_i_216] = signbit_uf(v_initial_param_534_217[v_i_216]); 
    }
}
}; 