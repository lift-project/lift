
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
void hypot(float * v_initial_param_145_84, float * v_initial_param_146_85, float * & v_user_func_152_87, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_152_87 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_83 = 0;(v_i_83 <= (-1 + v_N_0)); (++v_i_83)){
        v_user_func_152_87[v_i_83] = hypot_uf(v_initial_param_145_84[v_i_83], v_initial_param_146_85[v_i_83]); 
    }
}
}; 