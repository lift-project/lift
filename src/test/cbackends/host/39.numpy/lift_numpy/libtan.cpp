
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
void tan(float * v_initial_param_3131_446, float * & v_user_func_3133_447, int v_N_352){
    // Allocate memory for output pointers
    v_user_func_3133_447 = reinterpret_cast<float *>(malloc((v_N_352 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_445 = 0;(v_i_445 <= (-1 + v_N_352)); (++v_i_445)){
        v_user_func_3133_447[v_i_445] = tan_uf(v_initial_param_3131_446[v_i_445]); 
    }
}
}; 