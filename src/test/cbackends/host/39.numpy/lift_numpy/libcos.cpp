
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef COS_UF_H
#define COS_UF_H
; 
float cos_uf(float x){
    { return cos(x); }; 
}

#endif
 ; 
void cos(float * v_initial_param_89_46, float * & v_user_func_91_47, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_91_47 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_45 = 0;(v_i_45 <= (-1 + v_N_0)); (++v_i_45)){
        v_user_func_91_47[v_i_45] = cos_uf(v_initial_param_89_46[v_i_45]); 
    }
}
}; 