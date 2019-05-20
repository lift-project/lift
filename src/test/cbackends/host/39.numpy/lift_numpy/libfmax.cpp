
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMAX_UF_H
#define FMAX_UF_H
; 
float fmax_uf(float x, float y){
    { return max(x,y); }; 
}

#endif
 ; 
void fmax(float * v_initial_param_936_410, float * v_initial_param_937_411, float * & v_user_func_943_413, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_943_413 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_409 = 0;(v_i_409 <= (-1 + v_N_0)); (++v_i_409)){
        v_user_func_943_413[v_i_409] = fmax_uf(v_initial_param_936_410[v_i_409], v_initial_param_937_411[v_i_409]); 
    }
}
}; 