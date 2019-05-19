
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef FMAX_UF_H
#define FMAX_UF_H
; 
float fmax_uf(float x, float y){
    { return max(x,y); }; 
}

#endif
 ; 
void fmax(float * v_initial_param_904_388, float * v_initial_param_905_389, float * & v_user_func_911_391, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_911_391 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_387 = 0;(v_i_387 <= (-1 + v_N_0)); (++v_i_387)){
        v_user_func_911_391[v_i_387] = fmax_uf(v_initial_param_904_388[v_i_387], v_initial_param_905_389[v_i_387]); 
    }
}
}; 