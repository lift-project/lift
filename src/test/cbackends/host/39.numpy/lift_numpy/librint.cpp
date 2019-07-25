
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef RINT_UF_H
#define RINT_UF_H
; 
float rint_uf(float x){
    return round(x) ;; 
}

#endif
 ; 
void rint(float * v_initial_param_3259_506, float * & v_user_func_3261_507, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3261_507 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_505 = 0;(v_i_505 <= (-1 + v_N_352)); (++v_i_505)){
        v_user_func_3261_507[v_i_505] = rint_uf(v_initial_param_3259_506[v_i_505]); 
    }
}
}; 