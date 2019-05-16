
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
void tan(float * v_initial_param_1382_234, float * & v_user_func_1384_235, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1384_235 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_233 = 0;(v_i_233 <= (-1 + v_N_190)); (++v_i_233)){
        v_user_func_1384_235[v_i_233] = tan_uf(v_initial_param_1382_234[v_i_233]); 
    }
}
}; 