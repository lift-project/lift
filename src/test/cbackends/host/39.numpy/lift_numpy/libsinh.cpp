
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef SINH_UF_H
#define SINH_UF_H
; 
float sinh_uf(float x){
    { return sinh(x); }; 
}

#endif
 ; 
void sinh(float * v_initial_param_1461_270, float * & v_user_func_1463_271, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1463_271 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_269 = 0;(v_i_269 <= (-1 + v_N_190)); (++v_i_269)){
        v_user_func_1463_271[v_i_269] = sinh_uf(v_initial_param_1461_270[v_i_269]); 
    }
}
}; 