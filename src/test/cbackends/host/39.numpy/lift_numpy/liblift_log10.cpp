
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
void lift_log10(float * v_initial_param_3519_596, float * & v_user_func_3521_597, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3521_597 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_595 = 0;(v_i_595 <= (-1 + v_N_352)); (++v_i_595)){
        v_user_func_3521_597[v_i_595] = log10_uf(v_initial_param_3519_596[v_i_595]); 
    }
}
}; 