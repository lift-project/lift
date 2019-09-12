
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
void signbit(float * v_initial_param_7533_3029, float * & v_user_func_7535_3030, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7535_3030 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3028 = 0;(v_i_3028 <= (-1 + v_N_2763)); (++v_i_3028)){
        v_user_func_7535_3030[v_i_3028] = signbit_uf(v_initial_param_7533_3029[v_i_3028]); 
    }
}
}; 