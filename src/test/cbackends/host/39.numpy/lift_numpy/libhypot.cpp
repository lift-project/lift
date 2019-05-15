
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef HYPOT_UF_H
#define HYPOT_UF_H
; 
float hypot_uf(float x, float y){
    { return sqrt((x*x)+(y*y)); }; 
}

#endif; 
void hypot(float * v_initial_param_120_49, float * v_initial_param_121_50, float * & v_user_func_127_52, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_127_52 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_48 = 0;(v_i_48 <= (-1 + v_N_0)); (++v_i_48)){
        v_user_func_127_52[v_i_48] = hypot_uf(v_initial_param_120_49[v_i_48], v_initial_param_121_50[v_i_48]); 
    }
}
}; 