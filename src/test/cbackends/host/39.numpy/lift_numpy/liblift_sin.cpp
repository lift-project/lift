
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SIN_UF_H
#define SIN_UF_H
; 
float sin_uf(float x){
    { return sin(x); }; 
}

#endif
 ; 
void lift_sin(float * v_initial_param_121_83, float * & v_user_func_123_84, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_123_84 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_82 = 0;(v_i_82 <= (-1 + v_N_0)); (++v_i_82)){
        v_user_func_123_84[v_i_82] = sin_uf(v_initial_param_121_83[v_i_82]); 
    }
}
}; 