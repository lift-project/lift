
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
void hypot(float * v_initial_param_148_87, float * v_initial_param_149_88, float * & v_user_func_155_90, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_155_90 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_86 = 0;(v_i_86 <= (-1 + v_N_0)); (++v_i_86)){
        v_user_func_155_90[v_i_86] = hypot_uf(v_initial_param_148_87[v_i_86], v_initial_param_149_88[v_i_86]); 
    }
}
}; 