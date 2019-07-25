
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef LOG2_UF_H
#define LOG2_UF_H
; 
float log2_uf(float x){
    return log2(x) ;; 
}

#endif
 ; 
void lift_log2(float * v_initial_param_3526_599, float * & v_user_func_3528_600, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3528_600 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_598 = 0;(v_i_598 <= (-1 + v_N_352)); (++v_i_598)){
        v_user_func_3528_600[v_i_598] = log2_uf(v_initial_param_3526_599[v_i_598]); 
    }
}
}; 