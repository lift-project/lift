
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef ARCSIN_UF_H
#define ARCSIN_UF_H
; 
float arcsin_uf(float x){
    { return asin(x); }; 
}

#endif
 ; 
void arcsin(float * v_initial_param_124_75, float * & v_user_func_126_76, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_126_76 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_74 = 0;(v_i_74 <= (-1 + v_N_0)); (++v_i_74)){
        v_user_func_126_76[v_i_74] = arcsin_uf(v_initial_param_124_75[v_i_74]); 
    }
}
}; 