
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TRUNC_UF_H
#define TRUNC_UF_H
; 
float trunc_uf(float x){
    return trunc(x);; 
}

#endif
 ; 
void trunc(float * v_initial_param_7231_2929, float * & v_user_func_7233_2930, int v_N_2763){
    // Allocate memory for output pointers
    v_user_func_7233_2930 = reinterpret_cast<float *>(malloc((v_N_2763 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_2928 = 0;(v_i_2928 <= (-1 + v_N_2763)); (++v_i_2928)){
        v_user_func_7233_2930[v_i_2928] = trunc_uf(v_initial_param_7231_2929[v_i_2928]); 
    }
}
}; 