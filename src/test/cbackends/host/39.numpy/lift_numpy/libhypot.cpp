
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
void hypot(float * v_initial_param_163_106, float * v_initial_param_164_107, float * & v_user_func_170_109, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_170_109 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_105 = 0;(v_i_105 <= (-1 + v_N_0)); (++v_i_105)){
        v_user_func_170_109[v_i_105] = hypot_uf(v_initial_param_163_106[v_i_105], v_initial_param_164_107[v_i_105]); 
    }
}
}; 