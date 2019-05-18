
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
void hypot(float * v_initial_param_152_93, float * v_initial_param_153_94, float * & v_user_func_159_96, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_159_96 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_92 = 0;(v_i_92 <= (-1 + v_N_0)); (++v_i_92)){
        v_user_func_159_96[v_i_92] = hypot_uf(v_initial_param_152_93[v_i_92], v_initial_param_153_94[v_i_92]); 
    }
}
}; 