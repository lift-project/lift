
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef ARCTAN_UF_H
#define ARCTAN_UF_H
; 
float arctan_uf(float x){
    { return atan(x); }; 
}

#endif
 ; 
void arctan(float * v_initial_param_1403_243, float * & v_user_func_1405_244, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1405_244 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_242 = 0;(v_i_242 <= (-1 + v_N_190)); (++v_i_242)){
        v_user_func_1405_244[v_i_242] = arctan_uf(v_initial_param_1403_243[v_i_242]); 
    }
}
}; 