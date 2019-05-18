
#include <bits/stdc++.h>

using namespace std;

    
namespace lift {; 
#ifndef TAN_UF_H
#define TAN_UF_H
; 
float tan_uf(float x){
    { return tan(x); }; 
}

#endif
 ; 
void tan(float * v_initial_param_121_76, float * & v_user_func_123_77, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_123_77 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_75 = 0;(v_i_75 <= (-1 + v_N_0)); (++v_i_75)){
        v_user_func_123_77[v_i_75] = tan_uf(v_initial_param_121_76[v_i_75]); 
    }
}
}; 