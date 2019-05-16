
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef GRAD2_UF_H
#define GRAD2_UF_H
; 
float grad2_uf(float l, float r){
    { return (l - r)/2.0f; }; 
}

#endif
 ; 
void gradient(float * v_initial_param_340_172, float * & v_user_func_346_173, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_346_173 = reinterpret_cast<float *>(malloc(((-2 + v_N_0) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_171 = 0;(v_i_171 <= (-3 + v_N_0)); (++v_i_171)){
        v_user_func_346_173[v_i_171] = grad2_uf(v_initial_param_340_172[(2 + v_i_171)], v_initial_param_340_172[v_i_171]); 
    }
}
}; 