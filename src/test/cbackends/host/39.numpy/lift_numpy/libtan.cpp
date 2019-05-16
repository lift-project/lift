
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
void tan(float * v_initial_param_95_48, float * & v_user_func_97_49, int v_N_0){
    // Allocate memory for output pointers
    v_user_func_97_49 = reinterpret_cast<float *>(malloc((v_N_0 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_47 = 0;(v_i_47 <= (-1 + v_N_0)); (++v_i_47)){
        v_user_func_97_49[v_i_47] = tan_uf(v_initial_param_95_48[v_i_47]); 
    }
}
}; 