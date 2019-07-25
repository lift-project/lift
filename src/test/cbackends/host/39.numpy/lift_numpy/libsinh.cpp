
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SINH_UF_H
#define SINH_UF_H
; 
float sinh_uf(float x){
    { return sinh(x); }; 
}

#endif
 ; 
void sinh(float * v_initial_param_3210_482, float * & v_user_func_3212_483, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3212_483 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_481 = 0;(v_i_481 <= (-1 + v_N_352)); (++v_i_481)){
        v_user_func_3212_483[v_i_481] = sinh_uf(v_initial_param_3210_482[v_i_481]); 
    }
}
}; 