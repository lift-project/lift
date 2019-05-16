
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef HYPOT_UF_H
#define HYPOT_UF_H
; 
float hypot_uf(float x, float y){
    { return sqrt((x*x)+(y*y)); }; 
}

#endif
 ; 
void hypot(float * v_initial_param_123_60, float * v_initial_param_124_61, float * & v_user_func_130_63, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_130_63 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_59 = 0;(v_i_59 <= (-1 + v_N_0)); (++v_i_59)){
        v_user_func_130_63[v_i_59] = hypot_uf(v_initial_param_123_60[v_i_59], v_initial_param_124_61[v_i_59]); 
    }
}
}; 