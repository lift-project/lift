
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
void hypot(float * v_initial_param_120_52, float * v_initial_param_121_53, float * & v_user_func_127_55, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_127_55 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_51 = 0;(v_i_51 <= (-1 + v_N_0)); (++v_i_51)){
        v_user_func_127_55[v_i_51] = hypot_uf(v_initial_param_120_52[v_i_51], v_initial_param_121_53[v_i_51]); 
    }
}
}; 