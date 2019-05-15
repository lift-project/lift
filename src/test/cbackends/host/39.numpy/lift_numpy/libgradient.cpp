
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef GRAD2_UF_H
#define GRAD2_UF_H
; 
float grad2_uf(float l, float r){
    { return (l - r)/2.0f; }; 
}

#endif
 ; 
void gradient(float * v_initial_param_1612_341, float * & v_user_func_1618_342, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1618_342 = reinterpret_cast<float *>(malloc(((-2 + v_N_190) * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_340 = 0;(v_i_340 <= (-3 + v_N_190)); (++v_i_340)){
        v_user_func_1618_342[v_i_340] = grad2_uf(v_initial_param_1612_341[(2 + v_i_340)], v_initial_param_1612_341[v_i_340]); 
    }
}
}; 