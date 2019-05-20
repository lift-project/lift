
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
void reciprocal(float * v_initial_param_636_288, float * & v_user_func_638_289, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_638_289 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_287 = 0;(v_i_287 <= (-1 + v_N_0)); (++v_i_287)){
        v_user_func_638_289[v_i_287] = reciprocal_uf(v_initial_param_636_288[v_i_287]); 
    }
}
}; 