
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
void lift_log10(float * v_initial_param_480_215, float * & v_user_func_482_216, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_482_216 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_214 = 0;(v_i_214 <= (-1 + v_N_0)); (++v_i_214)){
        v_user_func_482_216[v_i_214] = log10_uf(v_initial_param_480_215[v_i_214]); 
    }
}
}; 