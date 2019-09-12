
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOG10_UF_H
#define LOG10_UF_H
; 
float log10_uf(float x){
    return log10(x) ;; 
}

#endif
 ; 
void lift_log10(float * v_initial_param_7463_3007, float * & v_user_func_7465_3008, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7465_3008 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_3006 = 0;(v_i_3006 <= (-1 + v_N_2763)); (++v_i_3006)){
        v_user_func_7465_3008[v_i_3006] = log10_uf(v_initial_param_7463_3007[v_i_3006]); 
    }
}
}; 