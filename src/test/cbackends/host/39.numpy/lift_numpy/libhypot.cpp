
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
void hypot(float * v_initial_param_120_50, float * v_initial_param_121_51, float * & v_user_func_127_53, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_127_53 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_49 = 0;(v_i_49 <= (-1 + v_N_0)); (++v_i_49)){
        v_user_func_127_53[v_i_49] = hypot_uf(v_initial_param_120_50[v_i_49], v_initial_param_121_51[v_i_49]); 
    }
}
}; 