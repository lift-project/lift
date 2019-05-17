
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
void hypot(float * v_initial_param_146_85, float * v_initial_param_147_86, float * & v_user_func_153_88, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_153_88 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_84 = 0;(v_i_84 <= (-1 + v_N_0)); (++v_i_84)){
        v_user_func_153_88[v_i_84] = hypot_uf(v_initial_param_146_85[v_i_84], v_initial_param_147_86[v_i_84]); 
    }
}
}; 