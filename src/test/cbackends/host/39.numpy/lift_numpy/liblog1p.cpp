
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
void log1p(float * v_initial_param_3533_602, float * & v_user_func_3535_603, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3535_603 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_601 = 0;(v_i_601 <= (-1 + v_N_352)); (++v_i_601)){
        v_user_func_3535_603[v_i_601] = log1p_uf(v_initial_param_3533_602[v_i_601]); 
    }
}
}; 