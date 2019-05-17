
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOG1P_UF_H
#define LOG1P_UF_H
; 
float log1p_uf(float x){
    return log(1+x) ;; 
}

#endif
 ; 
void log1p(float * v_initial_param_486_211, float * & v_user_func_488_212, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_488_212 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_210 = 0;(v_i_210 <= (-1 + v_N_0)); (++v_i_210)){
        v_user_func_488_212[v_i_210] = log1p_uf(v_initial_param_486_211[v_i_210]); 
    }
}
}; 