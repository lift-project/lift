
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMIN_UF_H
#define FMIN_UF_H
; 
float fmin_uf(float x, float y){
    { return min(x,y); }; 
}

#endif
 ; 
void fmin(float * v_initial_param_937_409, float * v_initial_param_938_410, float * & v_user_func_944_412, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_944_412 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_408 = 0;(v_i_408 <= (-1 + v_N_0)); (++v_i_408)){
        v_user_func_944_412[v_i_408] = fmin_uf(v_initial_param_937_409[v_i_408], v_initial_param_938_410[v_i_408]); 
    }
}
}; 