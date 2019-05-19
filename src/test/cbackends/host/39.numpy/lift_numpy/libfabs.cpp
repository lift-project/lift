
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ABSOLUTE_UF_H
#define ABSOLUTE_UF_H
; 
float absolute_uf(float x){
    { return abs(x); }; 
}

#endif
 ; 
void fabs(float * v_initial_param_862_372, float * & v_user_func_864_373, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_864_373 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_371 = 0;(v_i_371 <= (-1 + v_N_0)); (++v_i_371)){
        v_user_func_864_373[v_i_371] = absolute_uf(v_initial_param_862_372[v_i_371]); 
    }
}
}; 