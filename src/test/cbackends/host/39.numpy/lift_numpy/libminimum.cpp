
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef MINIMUM_UF_H
#define MINIMUM_UF_H
; 
float minimum_uf(float x, float y){
    { return min(x,y); }; 
}

#endif
 ; 
void minimum(float * v_initial_param_890_383, float * v_initial_param_891_384, float * & v_user_func_897_386, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_897_386 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_382 = 0;(v_i_382 <= (-1 + v_N_0)); (++v_i_382)){
        v_user_func_897_386[v_i_382] = minimum_uf(v_initial_param_890_383[v_i_382], v_initial_param_891_384[v_i_382]); 
    }
}
}; 