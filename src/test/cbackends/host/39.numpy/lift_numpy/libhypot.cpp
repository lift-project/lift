
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
void hypot(float * v_initial_param_163_104, float * v_initial_param_164_105, float * & v_user_func_170_107, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_170_107 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_103 = 0;(v_i_103 <= (-1 + v_N_0)); (++v_i_103)){
        v_user_func_170_107[v_i_103] = hypot_uf(v_initial_param_163_104[v_i_103], v_initial_param_164_105[v_i_103]); 
    }
}
}; 