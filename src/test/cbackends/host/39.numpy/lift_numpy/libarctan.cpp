
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
; 
float arctan_uf(float x){
    { return atan(x); }; 
}

#endif
 ; 
void arctan(float * v_initial_param_142_85, float * & v_user_func_144_86, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_144_86 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_84 = 0;(v_i_84 <= (-1 + v_N_0)); (++v_i_84)){
        v_user_func_144_86[v_i_84] = arctan_uf(v_initial_param_142_85[v_i_84]); 
    }
}
}; 