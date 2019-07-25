
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
void signbit(float * v_initial_param_3589_618, float * & v_user_func_3591_619, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3591_619 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_617 = 0;(v_i_617 <= (-1 + v_N_352)); (++v_i_617)){
        v_user_func_3591_619[v_i_617] = signbit_uf(v_initial_param_3589_618[v_i_617]); 
    }
}
}; 