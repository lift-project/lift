
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SINC_UF_H
#define SINC_UF_H
; 
float sinc_uf(float x){
    return sin(M_PI*x)/(M_PI*x) ;; 
}

#endif
 ; 
void sinc(float * v_initial_param_3582_615, float * & v_user_func_3584_616, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3584_616 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_614 = 0;(v_i_614 <= (-1 + v_N_352)); (++v_i_614)){
        v_user_func_3584_616[v_i_614] = sinc_uf(v_initial_param_3582_615[v_i_614]); 
    }
}
}; 