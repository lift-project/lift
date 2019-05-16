
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FLOOR_UF_H
#define FLOOR_UF_H
; 
float floor_uf(float x){
    return floor(x);; 
}

#endif
 ; 
void floor(float * v_initial_param_236_113, float * & v_user_func_238_114, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_238_114 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_112 = 0;(v_i_112 <= (-1 + v_N_0)); (++v_i_112)){
        v_user_func_238_114[v_i_112] = floor_uf(v_initial_param_236_113[v_i_112]); 
    }
}
}; 