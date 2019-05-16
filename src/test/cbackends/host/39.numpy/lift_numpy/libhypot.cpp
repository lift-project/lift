
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
void hypot(float * v_initial_param_129_66, float * v_initial_param_130_67, float * & v_user_func_136_69, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_136_69 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_65 = 0;(v_i_65 <= (-1 + v_N_0)); (++v_i_65)){
        v_user_func_136_69[v_i_65] = hypot_uf(v_initial_param_129_66[v_i_65], v_initial_param_130_67[v_i_65]); 
    }
}
}; 