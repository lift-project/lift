
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef COSH_UF_H
#define COSH_UF_H
; 
float cosh_uf(float x){
    { return cosh(x); }; 
}

#endif
 ; 
void cosh(float * v_initial_param_3217_485, float * & v_user_func_3219_486, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3219_486 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_484 = 0;(v_i_484 <= (-1 + v_N_352)); (++v_i_484)){
        v_user_func_3219_486[v_i_484] = cosh_uf(v_initial_param_3217_485[v_i_484]); 
    }
}
}; 