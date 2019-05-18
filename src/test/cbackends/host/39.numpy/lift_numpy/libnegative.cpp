
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef NEGATIVE_UF_H
#define NEGATIVE_UF_H
; 
float negative_uf(float x){
    return (-1.0f)*x; 
}

#endif
 ; 
void negative(float * v_initial_param_620_269, float * & v_user_func_622_270, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_622_270 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_268 = 0;(v_i_268 <= (-1 + v_N_0)); (++v_i_268)){
        v_user_func_622_270[v_i_268] = negative_uf(v_initial_param_620_269[v_i_268]); 
    }
}
}; 