
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCCOS_UF_H
#define ARCCOS_UF_H
; 
float arccos_uf(float x){
    { return acos(x); }; 
}

#endif
 ; 
void arccos(float * v_initial_param_130_77, float * & v_user_func_132_78, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_132_78 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_76 = 0;(v_i_76 <= (-1 + v_N_0)); (++v_i_76)){
        v_user_func_132_78[v_i_76] = arccos_uf(v_initial_param_130_77[v_i_76]); 
    }
}
}; 