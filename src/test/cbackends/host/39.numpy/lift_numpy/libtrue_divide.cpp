
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef DIVIDE_UF_H
#define DIVIDE_UF_H
; 
float divide_uf(float x, float y){
    return x / y;; 
}

#endif
 ; 
void true_divide(float * v_initial_param_3680_673, float * v_initial_param_3681_674, float * & v_user_func_3687_676, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3687_676 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    {
        
    }
    {
        
    }
    // For each element processed sequentially
    for (int v_i_672 = 0;(v_i_672 <= (-1 + v_N_352)); (++v_i_672)){
        v_user_func_3687_676[v_i_672] = divide_uf(v_initial_param_3680_673[v_i_672], v_initial_param_3681_674[v_i_672]); 
    }
}
}; 