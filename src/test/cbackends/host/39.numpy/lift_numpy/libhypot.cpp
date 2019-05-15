
#include <bits/stdc++.h>

using namespace std;

namespace lift {
    
#ifndef HYPOT_UF_H
#define HYPOT_UF_H
; 
float hypot_uf(float x, float y){
    { return sqrt((x*x)+(y*y)); }; 
}

#endif
 ; 
void hypot(float * v_initial_param_1410_246, float * v_initial_param_1411_247, float * & v_user_func_1417_249, int v_N_190){
    // Allocate memory for output pointers
    v_user_func_1417_249 = reinterpret_cast<float *>(malloc((v_N_190 * sizeof(float)))); 
    // For each element processed sequentially
    for (int v_i_245 = 0;(v_i_245 <= (-1 + v_N_190)); (++v_i_245)){
        v_user_func_1417_249[v_i_245] = hypot_uf(v_initial_param_1410_246[v_i_245], v_initial_param_1411_247[v_i_245]); 
    }
}
}; 