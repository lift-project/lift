
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
void rint(float * v_initial_param_225_110, float * & v_user_func_227_111, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_227_111 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_109 = 0;(v_i_109 <= (-1 + v_N_0)); (++v_i_109)){
        v_user_func_227_111[v_i_109] = rint_uf(v_initial_param_225_110[v_i_109]); 
    }
}
}; 