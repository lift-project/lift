
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
void absolute(float * v_initial_param_862_369, float * & v_user_func_864_370, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_864_370 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_368 = 0;(v_i_368 <= (-1 + v_N_0)); (++v_i_368)){
        v_user_func_864_370[v_i_368] = absolute_uf(v_initial_param_862_369[v_i_368]); 
    }
}
}; 