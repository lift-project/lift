
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
void hypot(float * v_initial_param_144_83, float * v_initial_param_145_84, float * & v_user_func_151_86, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_151_86 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_82 = 0;(v_i_82 <= (-1 + v_N_0)); (++v_i_82)){
        v_user_func_151_86[v_i_82] = hypot_uf(v_initial_param_144_83[v_i_82], v_initial_param_145_84[v_i_82]); 
    }
}
}; 