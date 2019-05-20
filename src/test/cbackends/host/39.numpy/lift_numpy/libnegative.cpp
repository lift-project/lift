
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef NEGATIVE_UF_H
#define NEGATIVE_UF_H
; 
float negative_uf(float x){
    return (-1.0f)*x; 
}

#endif
 ; 
void negative(float * v_initial_param_663_300, float * & v_user_func_665_301, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_665_301 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_299 = 0;(v_i_299 <= (-1 + v_N_0)); (++v_i_299)){
        v_user_func_665_301[v_i_299] = negative_uf(v_initial_param_663_300[v_i_299]); 
    }
}
}; 