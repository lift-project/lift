
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
void signbit(float * v_initial_param_561_248, float * & v_user_func_563_249, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_563_249 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_247 = 0;(v_i_247 <= (-1 + v_N_0)); (++v_i_247)){
        v_user_func_563_249[v_i_247] = signbit_uf(v_initial_param_561_248[v_i_247]); 
    }
}
}; 