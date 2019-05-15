
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef HYPOT_UF_H
#define HYPOT_UF_H
; 
float hypot_uf(float x, float y){
    { return sqrt((x*x)+(y*y)); }; 
}

#endif
 ; 
void hypot(float * v_initial_param_121_58, float * v_initial_param_122_59, float * & v_user_func_128_61, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_128_61 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_57 = 0;(v_i_57 <= (-1 + v_N_0)); (++v_i_57)){
        v_user_func_128_61[v_i_57] = hypot_uf(v_initial_param_121_58[v_i_57], v_initial_param_122_59[v_i_57]); 
    }
}
}; 