
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef RECIPROCAL_UF_H
#define RECIPROCAL_UF_H
; 
float reciprocal_uf(float x){
    return 1.0f/x; 
}

#endif
 ; 
void reciprocal(float * v_initial_param_3645_644, float * & v_user_func_3647_645, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3647_645 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_643 = 0;(v_i_643 <= (-1 + v_N_352)); (++v_i_643)){
        v_user_func_3647_645[v_i_643] = reciprocal_uf(v_initial_param_3645_644[v_i_643]); 
    }
}
}; 