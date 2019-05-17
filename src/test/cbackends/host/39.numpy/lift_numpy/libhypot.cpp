
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
void hypot(float * v_initial_param_140_79, float * v_initial_param_141_80, float * & v_user_func_147_82, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_147_82 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_78 = 0;(v_i_78 <= (-1 + v_N_0)); (++v_i_78)){
        v_user_func_147_82[v_i_78] = hypot_uf(v_initial_param_140_79[v_i_78], v_initial_param_141_80[v_i_78]); 
    }
}
}; 