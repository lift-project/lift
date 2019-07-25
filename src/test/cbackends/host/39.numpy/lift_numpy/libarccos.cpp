
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
void arccos(float * v_initial_param_3145_452, float * & v_user_func_3147_453, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3147_453 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_451 = 0;(v_i_451 <= (-1 + v_N_352)); (++v_i_451)){
        v_user_func_3147_453[v_i_451] = arccos_uf(v_initial_param_3145_452[v_i_451]); 
    }
}
}; 