
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef SINH_UF_H
#define SINH_UF_H
; 
float sinh_uf(float x){
    { return sinh(x); }; 
}

#endif
 ; 
void sinh(float * v_initial_param_190_102, float * & v_user_func_192_103, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_192_103 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_101 = 0;(v_i_101 <= (-1 + v_N_0)); (++v_i_101)){
        v_user_func_192_103[v_i_101] = sinh_uf(v_initial_param_190_102[v_i_101]); 
    }
}
}; 