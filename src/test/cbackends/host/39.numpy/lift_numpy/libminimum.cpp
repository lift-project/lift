
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
void minimum(float * v_initial_param_890_385, float * v_initial_param_891_386, float * & v_user_func_897_388, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_897_388 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_384 = 0;(v_i_384 <= (-1 + v_N_0)); (++v_i_384)){
        v_user_func_897_388[v_i_384] = minimum_uf(v_initial_param_890_385[v_i_384], v_initial_param_891_386[v_i_384]); 
    }
}
}; 