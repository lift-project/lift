
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
void trunc(float * v_initial_param_3287_518, float * & v_user_func_3289_519, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3289_519 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_517 = 0;(v_i_517 <= (-1 + v_N_352)); (++v_i_517)){
        v_user_func_3289_519[v_i_517] = trunc_uf(v_initial_param_3287_518[v_i_517]); 
    }
}
}; 